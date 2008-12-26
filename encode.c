/*
   +----------------------------------------------------------------------+
   | PHP Version 5                                                        |
   +----------------------------------------------------------------------+
   | Copyright (c) 1997-2008 The PHP Group                                |
   +----------------------------------------------------------------------+
   | This source file is subject to version 3.01 of the PHP license,      |
   | that is bundled with this package in the file LICENSE, and is        |
   | available through the world-wide-web at the following url:           |
   | http://www.php.net/license/3_01.txt                                  |
   | If you did not receive a copy of the PHP license and are unable to   |
   | obtain it through the world-wide-web, please send a note to          |
   | license@php.net so we can mail you a copy immediately.               |
   +----------------------------------------------------------------------+
   | Authors: Itai Zukerman <zukerman@math-hat.com>                       |
   +----------------------------------------------------------------------+
 */

#include "php.h"
#include "php_erlang.h"

int php_erlang_encode_term_list( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point );
int php_erlang_encode_prim( ei_x_buff * x, int type, zval ** zp );

#define FMT_MOVE( n ) *idx = *idx + n
#define FMT_CHAR fmt[ *idx ]
/* #define FMT_NEXT_CHAR( ch ) ch = fmt[ *idx ]; php_error( E_NOTICE, "read %c", ch ); FMT_MOVE( 1 ) */
#define FMT_NEXT_CHAR( ch ) ch = fmt[ *idx ]; FMT_MOVE( 1 )

int php_erlang_encode_term( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point ) {

    int ch;

    ei_x_buff xsub;
    int n;

    zval ** zp;

    FMT_NEXT_CHAR( ch );

    // Special case, empty tuple.
    if( ch == '{' && FMT_CHAR == '}' ) {
        ei_x_encode_tuple_header( x, 0 );
        FMT_MOVE( 1 );
        return 0;
    }

    // Special case, empty list.
    if( ch == '[' && FMT_CHAR == ']' ) {
        ei_x_encode_empty_list( x );
        FMT_MOVE( 1 );
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
                ei_x_append( x, & xsub );
                ei_x_free( & xsub );
            } else {
                ei_x_free( & xsub );
                return -1;
            }
        } else {
            // List.
            FMT_NEXT_CHAR( ch );
            if( ch == '|' ) {
                if( php_erlang_encode_term( & xsub, fmt, idx, arr, point ) < 0 ) {
                    ei_x_free( & xsub );
                    return -1;
                }
                // FIXME: Should make sure the term was a list.
                FMT_NEXT_CHAR( ch );
            } else {
                // Terminate the list.
                ei_x_encode_empty_list( & xsub );
            }
            if( ch == ']' ) {
                ei_x_encode_list_header( x, n );
                ei_x_append( x, & xsub );
                ei_x_free( & xsub );
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

    // FIXME:  We don't support empty tuples or lists.
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

    ei_x_buff * x_with_version, x_no_version;
    int idx, version;

    switch( type ) {
    case 'a':
        if( Z_TYPE_PP( zp ) != IS_STRING ) { return -1; }
        ei_x_encode_atom( x, Z_STRVAL_PP( zp ) );
        break;

    case 's':
        if( Z_TYPE_PP( zp ) != IS_STRING ) { return -1; }
        ei_x_encode_string_len( x, Z_STRVAL_PP( zp ), Z_STRLEN_PP( zp ) );
        break;

    case 'b':
        if( Z_TYPE_PP( zp ) != IS_STRING ) { return -1; }
        ei_x_encode_binary( x, Z_STRVAL_PP( zp ), Z_STRLEN_PP( zp ) );
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
        // We need to remove the leading version.
        x_with_version = (ei_x_buff *) zend_fetch_resource( zp TSRMLS_CC, -1, PHP_ERLANG_X_BUFF_RES_NAME, NULL, 1, le_erlang_x_buff );
        x_no_version = * x_with_version;
        idx = 0;
        if( ei_decode_version( x_with_version->buff, & idx, & version ) == 0 ) {
            x_no_version.buff += idx;
        }
        ei_x_append( x, & x_no_version );
        break;

    default:
        return -1;
    }
    return 1;
}
