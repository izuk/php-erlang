/*
    Copyright 2008 Itai Zukerman

    This file is part of PHP/Erlang.

    PHP/Erlang is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    PHP/Erlang is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with PHP/Erlang.  If not, see
    <http://www.gnu.org/licenses/>.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "php.h"
#include "php_ini.h"
#include "php_erlang.h"

int le_erlang_x_buff;
int le_erlang_pid;

ZEND_DECLARE_MODULE_GLOBALS(erlang)

static function_entry erlang_functions[] = {
    PHP_FE(erlang_init, NULL)
    PHP_FE(erlang_self, NULL)
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
    erlang_globals->instance = 0;
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
    fflush( stderr );

    return SUCCESS;
}

PHP_FUNCTION(erlang_init)
{
    char * node;
    int fd;

    int ret;
    char buff[ 100 ];

    fd = ERLANG_G(fd);

    if( fd > 0 ) {
        // Check connection.  There must be a better way to do this.
        // Doesn't this eat up valuable time?
        php_error( E_NOTICE, "checking connection" );
        while( 1 ) {
            ret = ei_receive_tmo( fd, buff, 100, 1 );
            if( ret == ERL_ERROR ) {
                if( erl_errno != ETIMEDOUT ) {
                    php_error( E_NOTICE, "persistent connection timed out" );
                    close( fd );
                    fd = 0;
                }
                break;
            }
        }
    }

    if( fd == 0 ) {

        // Connecting seems to sometimes fail without the "instance"
        // number if this node has recently timed out.  Perhaps the Erlang side
        // doesn't like it if a node times out and then tries to reconnect with the
        // same name?

        // On the other hand, I've been told that these node names don't get
        // garbage collected.  Maybe I should roll instance over after some
        // small number?

        snprintf( buff, 100, "php%010d%05d", (int) getpid(), ERLANG_G(instance)++ );
        ei_connect_init( & ERLANG_G(ec), buff, INI_STR("erlang.cookie"), 0 );

        php_error( E_NOTICE, "init %s %s %s",
                   ei_thisnodename( & ERLANG_G(ec) ),
                   ei_thishostname( & ERLANG_G(ec) ),
                   ei_thisalivename( & ERLANG_G(ec) ) );

        node = INI_STR("erlang.node");
        fd = ei_connect( & ERLANG_G(ec), node );

        if( fd < 0 ) {
            php_error( E_ERROR, "couldn't connect to Erlang node `%s' %d", node, erl_errno );
            RETURN_FALSE;
        }

        php_error( E_NOTICE, "connected to Erlang node `%s'", node );

        ERLANG_G(fd) = fd;
    }

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

    char * fmt;
    int len;
    int idx = 0;

    zval * z;
    HashTable * arr;
    HashPosition point;

    int ret;

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "sa", & fmt, & len, & z ) == FAILURE ) {
        RETURN_FALSE;
    }

    arr = Z_ARRVAL_P(z);
    zend_hash_internal_pointer_reset_ex( arr, & point );

    x = emalloc( sizeof( ei_x_buff ) );
    ei_x_new_with_version( x );

    ret = php_erlang_encode_term( x, fmt, & idx, arr, & point );
    if( ret < 0 ) {
        php_error( E_ERROR, "problem parsing term format `%s' at position %d", fmt, idx );
        RETURN_FALSE;
    }

    ZEND_REGISTER_RESOURCE(return_value, x, le_erlang_x_buff);
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

    ret = php_erlang_decode( x );
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
        php_error( E_ERROR, "not connected to an Erlang node" );
        RETURN_FALSE;
    }

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rr|l", & zpid, & zterm, & timeout ) == FAILURE ) {
        RETURN_FALSE;
    }

    ZEND_FETCH_RESOURCE(pid, erlang_pid *, & zpid, -1, PHP_ERLANG_PID_RES_NAME, le_erlang_pid);
    ZEND_FETCH_RESOURCE(x, ei_x_buff *, & zterm, -1, PHP_ERLANG_X_BUFF_RES_NAME, le_erlang_x_buff);

    ret = ei_send_tmo( ERLANG_G(fd), pid, x->buff, x->index, timeout );
    if( ret < 0 ) {
        php_error( E_WARNING, "error [%d] sending message", ret );
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
        php_error( E_ERROR, "not connected to an Erlang node" );
        RETURN_FALSE;
    }

    if( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "sr|l", & name, & len, & zterm, & timeout ) == FAILURE ) {
        RETURN_FALSE;
    }

    ZEND_FETCH_RESOURCE(x, ei_x_buff *, & zterm, -1, PHP_ERLANG_X_BUFF_RES_NAME, le_erlang_x_buff);

    ret = ei_reg_send_tmo( & ERLANG_G(ec), ERLANG_G(fd), name, x->buff, x->index, timeout );

    if( ret < 0 ) {
        php_error( E_ERROR, "error sending message" );
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
        php_error( E_ERROR, "not connected to an Erlang node" );
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
                php_error( E_WARNING, "unknown message type [%d] received", msg.msgtype );
                ei_x_free( x );
                efree( x );
                RETURN_FALSE;
            }
            break;
        case ERL_ERROR:
            if( erl_errno != ETIMEDOUT ) {
                php_error( E_ERROR, "error [%d] reading message", erl_errno );
            }
            ei_x_free( x );
            efree( x );
            RETURN_FALSE;
        default:
            php_error( E_ERROR, "error [%d, %d] reading message", ret, erl_errno );
            ei_x_free( x );
            efree( x );
            RETURN_FALSE;
        }
    }
}
