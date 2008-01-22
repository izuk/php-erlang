#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "php_erlang.h"

int le_erlang_x_buff;
int le_erlang_pid;

ZEND_DECLARE_MODULE_GLOBALS(erlang)

static function_entry erlang_functions[] = {
    PHP_FE(erlang_init, NULL)
    PHP_FE(erlang_term, NULL)
    PHP_FE(erlang_extract, NULL)
    PHP_FE(erlang_send, NULL)
    PHP_FE(erlang_send_reg, NULL)
    PHP_FE(erlang_receive, NULL)
    {NULL, NULL, NULL}
};

zend_module_entry erlang_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
    STANDARD_MODULE_HEADER,
#endif
    PHP_ERLANG_EXTNAME,
    erlang_functions,
    PHP_MINIT(erlang),
    PHP_MSHUTDOWN(erlang),
    PHP_RINIT(erlang),
    PHP_RSHUTDOWN(erlang),
    NULL,
#if ZEND_MODULE_API_NO >= 20010901
    PHP_ERLANG_VERSION,
#endif
    STANDARD_MODULE_PROPERTIES
};

#ifdef COMPILE_DL_ERLANG
ZEND_GET_MODULE(erlang)
#endif

PHP_INI_BEGIN()
PHP_INI_ENTRY("erlang.cookie", "PHP/Erlang", PHP_INI_SYSTEM, NULL)
PHP_INI_ENTRY("erlang.node", "erts@localhost", PHP_INI_SYSTEM, NULL)
PHP_INI_END()

static void php_erlang_init_globals( zend_erlang_globals *erlang_globals ) {
    erlang_globals->fd = 0;
}

static void php_erlang_x_buff_dtor( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
    ei_x_buff * x = (ei_x_buff *) rsrc->ptr;
    if( x ) {
        ei_x_free( x );
    }
}

PHP_MINIT_FUNCTION(erlang)
{
    REGISTER_INI_ENTRIES();
    ZEND_INIT_MODULE_GLOBALS(erlang, php_erlang_init_globals, NULL);

    le_erlang_x_buff = zend_register_list_destructors_ex( php_erlang_x_buff_dtor, NULL, PHP_ERLANG_X_BUFF_RES_NAME, module_number );
    le_erlang_pid = zend_register_list_destructors_ex( NULL, NULL, PHP_ERLANG_PID_RES_NAME, module_number );

    return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(erlang)
{
    UNREGISTER_INI_ENTRIES();
    return SUCCESS;
}

PHP_RINIT_FUNCTION(erlang)
{
    return SUCCESS;
}

PHP_RSHUTDOWN_FUNCTION(erlang)
{
    return SUCCESS;
}

PHP_FUNCTION(erlang_init)
{
    int fd;

    ei_x_buff x;
    erlang_msg msg;
    int ret;

    int loop = 1;

    fd = ERLANG_G(fd);

    // Check whether the link is still alive.
    if( fd > 0 ) {
        ei_x_new( & x );
        while( loop ) {
            ret = ei_xreceive_msg_tmo( ERLANG_G(fd), & msg, & x, 1 );
            switch( ret ) {
            case ERL_TICK:
                break;
            case ERL_MSG:
                switch( msg.msgtype ) {
                case ERL_SEND:
                case ERL_REG_SEND:
                case ERL_LINK:
                    break;
                case ERL_UNLINK:
                case ERL_EXIT:
                case ERL_EXIT2:
                    fd = 0;
                    loop = 0;
                    break;
                default:
                    php_error_docref( NULL TSRMLS_CC, E_WARNING, "unknown message received" );
                    fd = 0;
                    loop = 0;
                }
            case ERL_ERROR:
                if( erl_errno == ETIMEDOUT ) {
                    // This is really the desired exit point.
                    loop = 0;
                } else {
                    php_error_docref( NULL TSRMLS_CC, E_WARNING, "error reading persistent connection" );
                    fd = 0;
                    loop = 0;
                }
                break;
            default:
                php_error_docref( NULL TSRMLS_CC, E_ERROR, "unknown message on init" );
                fd = 0;
                loop = 0;
            }
        }
        ei_x_free( & x );
    }

    if( fd == 0 ) {
        ei_connect_init( & ERLANG_G(ec), "mynodename", INI_STR("erlang.cookie"), 0 );
        fd = ei_connect( & ERLANG_G(ec), INI_STR("erlang.node") );
        if( fd < 0 ) {
            php_error_docref( NULL TSRMLS_CC, E_ERROR, "couldn't connect to Erlang node" );
            ERLANG_G(fd) = 0;
            RETURN_FALSE;
        }
    }

    ERLANG_G(fd) = fd;

    RETURN_TRUE;
}

PHP_FUNCTION(erlang_self)
{
    erlang_pid * pid;
    pid = ei_self( & ERLANG_G(ec) );
    ZEND_REGISTER_RESOURCE(return_value, pid, le_erlang_pid);
}

PHP_FUNCTION(erlang_term)
{
    ei_x_buff * x;

    char * atom;
    int len;

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "s", & atom, & len ) == FAILURE ) {
        RETURN_FALSE;
    }

    x = emalloc( sizeof( ei_x_buff ) );

    ei_x_new_with_version( x );
    ei_x_encode_tuple_header( x, 2 );
    ei_x_encode_pid( x, ei_self( & ERLANG_G(ec) ) );
    ei_x_encode_atom( x, atom );

    ZEND_REGISTER_RESOURCE(return_value, x, le_erlang_x_buff);
}

zval * php_erl_decode( ei_x_buff * x ) {

    zval * z, * z0;

    int type;
    int size;

    char * buff;

    long long_value;
    double double_value;

    int i;

    ALLOC_INIT_ZVAL(z);

    ei_get_type( x->buff, & x->index, & type, & size );

    switch( type ) {
    case ERL_ATOM_EXT:
        buff = emalloc( size + 1 );
        ei_decode_atom( x->buff, & x->index, buff );
        buff[ size ] = '\0';
        ZVAL_STRING(z, buff, 0);
        break;
    case ERL_STRING_EXT:
        buff = emalloc( size + 1 );
        ei_decode_string( x->buff, & x->index, buff );
        buff[ size ] = '\0';
        ZVAL_STRING(z, buff, 0);
        break;
    case ERL_PID_EXT:
        buff = emalloc( sizeof( erlang_pid ) );
        ei_decode_pid( x->buff, & x->index, (erlang_pid *) buff );
        ALLOC_INIT_ZVAL(z);
        ZEND_REGISTER_RESOURCE(z, buff, le_erlang_pid);
        break;
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
        ei_decode_long( x->buff, & x->index, & long_value );
        ZVAL_LONG(z, long_value);
        break;
    case ERL_FLOAT_EXT:
        ei_decode_double( x->buff, & x->index, & double_value );
        ZVAL_DOUBLE(z, double_value);
        break;
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
        array_init( z );
        ei_decode_tuple_header( x->buff, & x->index, & size );
        for( i = 1; i <= size; i++ ) {
            z0 = php_erl_decode( x );
            if( z0 == NULL ) { return NULL; }
            add_next_index_zval( z, z0 );
        }
        break;
    case ERL_NIL_EXT:
    case ERL_LIST_EXT:
        array_init( z );
        ei_decode_list_header( x->buff, & x->index, & size );
        while( size > 0 ) {
            for( i = 1; i <= size; i++ ) {
                z0 = php_erl_decode( x );
                if( z0 == NULL ) { return NULL; }
                add_next_index_zval( z, z0 );
            }
            ei_decode_list_header( x->buff, & x->index, & size );
        }
        break;
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_PORT_EXT:
    case ERL_BINARY_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
    case ERL_NEW_FUN_EXT:
    case ERL_FUN_EXT:
    default:
        php_error_docref( NULL TSRMLS_CC, E_ERROR, "unsupported return type" );
        return NULL;
    }

    return z;
}

PHP_FUNCTION(erlang_extract)
{
    zval * zterm, * ret;
    ei_x_buff * x;
    int version;
    char atom[ MAXATOMLEN ];

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "r", & zterm ) == FAILURE ) {
        RETURN_FALSE;
    }

    ZEND_FETCH_RESOURCE(x, ei_x_buff *, & zterm, -1, PHP_ERLANG_X_BUFF_RES_NAME, le_erlang_x_buff);

    x->index = 0;
    ei_decode_version( x->buff, & x->index, & version );

    ret = php_erl_decode( x );
    if( ret == NULL ) {
        RETURN_FALSE;
    } else {
        * return_value = * ret;
    }
}

PHP_FUNCTION(erlang_send)
{
    zval * zpid, * zterm;
    long timeout = 0;

    erlang_pid * pid;
    ei_x_buff * x;
    int ret;

    if( ERLANG_G(fd) == 0 ) {
        php_error_docref( NULL TSRMLS_CC, E_ERROR, "not connected to an Erlang node" );
        RETURN_FALSE;
    }

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rr|l", & zpid, & zterm, & timeout ) == FAILURE ) {
        RETURN_FALSE;
    }

    ZEND_FETCH_RESOURCE(pid, erlang_pid *, & zpid, -1, PHP_ERLANG_PID_RES_NAME, le_erlang_pid);
    ZEND_FETCH_RESOURCE(x, ei_x_buff *, & zterm, -1, PHP_ERLANG_X_BUFF_RES_NAME, le_erlang_x_buff);

    ret = ei_send_tmo( ERLANG_G(fd), pid, x->buff, x->index, timeout );
    if( ret < 0 ) {
        php_error_docref( NULL TSRMLS_CC, E_WARNING, "error sending message" );
        RETURN_FALSE;
    }

    RETURN_TRUE;
}

PHP_FUNCTION(erlang_send_reg)
{

    char * name;
    int len;
    zval * zterm;
    long timeout = 0;

    ei_x_buff * x;
    int ret;

    if( ERLANG_G(fd) == 0 ) {
        php_error_docref( NULL TSRMLS_CC, E_ERROR, "not connected to an Erlang node" );
        RETURN_FALSE;
    }

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "sr|l", & name, & len, & zterm, & timeout ) == FAILURE ) {
        RETURN_FALSE;
    }

    ZEND_FETCH_RESOURCE(x, ei_x_buff *, & zterm, -1, PHP_ERLANG_X_BUFF_RES_NAME, le_erlang_x_buff);

    ret = ei_reg_send_tmo( & ERLANG_G(ec), ERLANG_G(fd), name, x->buff, x->index, timeout );

    if( ret < 0 ) {
        php_error_docref( NULL TSRMLS_CC, E_ERROR, "error sending message" );
        RETURN_FALSE;
    }

    RETURN_TRUE;
}

PHP_FUNCTION(erlang_receive)
{
    long timeout = 0;

    ei_x_buff * x;
    erlang_msg msg;
    int ret;

    if( ERLANG_G(fd) == 0 ) {
        php_error_docref( NULL TSRMLS_CC, E_ERROR, "not connected to an Erlang node" );
        RETURN_FALSE;
    }

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "|l", & timeout ) == FAILURE ) {
        RETURN_FALSE;
    }

    x = emalloc( sizeof( ei_x_buff ) );
    ei_x_new( x );

    while( 1 ) {
        ret = ei_xreceive_msg_tmo( ERLANG_G(fd), & msg, x, timeout );
        switch( ret ) {
        case ERL_TICK:
            break;
        case ERL_MSG:
            if( msg.msgtype == ERL_SEND ) {
                ZEND_REGISTER_RESOURCE(return_value, x, le_erlang_x_buff);
                return;
            } else {
                php_error_docref( NULL TSRMLS_CC, E_WARNING, "unknown message received" );
                ei_x_free( x );
                efree( x );
                RETURN_FALSE;
            }
            break;
        default:
            php_error_docref( NULL TSRMLS_CC, E_ERROR, "unknown message received" );
            ei_x_free( x );
            efree( x );
            RETURN_FALSE;
        }
    }
}
