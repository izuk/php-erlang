<?
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

{

    ini_set( "display_errors", 1 );
    ini_set( "error_reporting", E_ALL );

    erlang_init();
    $self = erlang_self();

    $subterm = erlang_term( "[~s,~s]", array( "a", "b" ) );

    for( $i = 0; $i < 4; $i++ ) {
        $message = erlang_term( "{~p,{~s,~t}}", array( $self, "foo{$i}", $subterm ) );
        erlang_send_reg( "echo", $message, 100 );
    }
    for( $i = 0; $i < 4; $i++ ) {
        $message = erlang_receive( 100 );
        if( $message ) {
            print_r( erlang_extract( $message ) );
            echo( "\n" );
        } else {
            echo( "Timeout!\n" );
        }
    }
}
?>
