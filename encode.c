#include "php.h"
#include "php_erlang.h"

int php_erlang_encode_term_list( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point );
int php_erlang_encode_prim( ei_x_buff * x, int type, zval ** zp );

#define FMT_MOVE( n ) *idx = *idx + n
#define FMT_CHAR fmt[ *idx ]
#define FMT_NEXT_CHAR( ch ) ch = fmt[ *idx ]; FMT_MOVE( 1 )

int php_erlang_encode_term( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point ) {

    int ch;

    ei_x_buff xsub;
    int n;

    zval ** zp;

    FMT_NEXT_CHAR( ch );

    // Special case: empty list.
    if( ch == '[' && FMT_CHAR == ']' ) {
        FMT_MOVE( 1 );
        ei_x_encode_empty_list( x );
        return 0;
    }

    switch( ch ) {
    case '{':
    case '[':
        ei_x_new( & xsub );
        n = php_erlang_encode_term_list( & xsub, fmt, idx, arr, point );
        if( n < 0 ) {
            ei_x_free( & xsub );
            return -1;
        }
        if( ch == '{' ) {
            // Tuple.
            FMT_NEXT_CHAR( ch );
            if( ch == '}' ) {
                ei_x_encode_tuple_header( x, n );
                if( n > 0 ) {
                    ei_x_append( x, & xsub );
                }
                ei_x_free( & xsub );
            } else {
                ei_x_free( & xsub );
                return -1;
            }
        } else {
            // List.
            FMT_NEXT_CHAR( ch );
            if( ch == ']' || ch == '|' ) {
                ei_x_encode_list_header( x, n );
                ei_x_append( x, & xsub );
                ei_x_free( & xsub );
                if( ch == '|' ) {
                    // FIXME: Should make sure the term was a list.
                    if( php_erlang_encode_term( x, fmt, idx, arr, point ) < 0 ) {
                        return -1;
                    }
                } else {
                    ei_x_encode_empty_list( x );
                }
            } else {
                ei_x_free( & xsub );
                return -1;
            }
        }
        return n;

    case '~':
        if( zend_hash_get_current_data_ex( arr, (void **) & zp, point ) != SUCCESS ) {
            php_error( E_ERROR, "missing value needed by term `%s' at %d", fmt, *idx );
            return -1;
        }
        zend_hash_move_forward_ex( arr, point );
        FMT_NEXT_CHAR( ch );
        return php_erlang_encode_prim( x, ch, zp );

    default:
        return -1;
    }
}

int php_erlang_encode_term_list( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point ) {

    int ch;
    int n = 0;

    // FIXME: Should work for length-0 lists.
    if( php_erlang_encode_term( x, fmt, idx, arr, point ) < 0 ) {
        return -1;
    }
    n ++;

    while( FMT_CHAR == ',' ) {
        FMT_MOVE( 1 );
        if( php_erlang_encode_term( x, fmt, idx, arr, point ) < 0 ) {
            return -1;
        }
        n ++;
    }

    return n;
}

int php_erlang_encode_prim( ei_x_buff * x, int type, zval ** zp ) {
    switch( type ) {
    case 'a':
        if( Z_TYPE_PP( zp ) != IS_STRING ) { return -1; }
        ei_x_encode_atom( x, Z_STRVAL_PP( zp ) );
        break;
    case 's':
        if( Z_TYPE_PP( zp ) != IS_STRING ) { return -1; }
        ei_x_encode_string_len( x, Z_STRVAL_PP( zp ), Z_STRLEN_PP( zp ) );
        break;
    case 'l':
        if( Z_TYPE_PP( zp ) != IS_LONG ) { return -1; }
        ei_x_encode_long( x, Z_LVAL_PP( zp ) );
        break;
    case 'd':
        if( Z_TYPE_PP( zp ) != IS_DOUBLE ) { return -1; }
        ei_x_encode_double( x, Z_DVAL_PP( zp ) );
        break;
    case 'p':
        if( Z_TYPE_PP( zp ) != IS_RESOURCE ) { return -1; }
        // For now, assume we have the right resource.
        ei_x_encode_pid( x, (erlang_pid *) zend_fetch_resource( zp TSRMLS_CC, -1, PHP_ERLANG_PID_RES_NAME, NULL, 1, le_erlang_pid ) );
        break;
    case 't':
        if( Z_TYPE_PP( zp ) != IS_RESOURCE ) { return -1; }
        // For now, assume we have the right resource.
        ei_x_append( x, (ei_x_buff *) zend_fetch_resource( zp TSRMLS_CC, -1, PHP_ERLANG_X_BUFF_RES_NAME, NULL, 1, le_erlang_x_buff ) );
        break;
    default:
        return -1;
    }
    return 1;
}
