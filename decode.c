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

zval * php_erlang_decode( ei_x_buff * x ) {

    zval * z, * z0;

    int type;
    int size;

    char * buff;

    long len;
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

    case ERL_BINARY_EXT:
        buff = emalloc( size );
        ei_decode_binary( x->buff, & x->index, buff, & len );
        ZVAL_STRINGL(z, buff, size, 0);
        break;

    case ERL_PID_EXT:
        buff = emalloc( sizeof( erlang_pid ) );
        ei_decode_pid( x->buff, & x->index, (erlang_pid *) buff );
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
            z0 = php_erlang_decode( x );
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
                z0 = php_erlang_decode( x );
                if( z0 == NULL ) { return NULL; }
                add_next_index_zval( z, z0 );
            }
            ei_decode_list_header( x->buff, & x->index, & size );
        }
        break;

    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_PORT_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
    case ERL_NEW_FUN_EXT:
    case ERL_FUN_EXT:
    default:
        php_error( E_ERROR, "unsupported term type %d", type );
        return NULL;
    }

    return z;
}
