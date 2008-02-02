%    Copyright 2008 Itai Zukerman
%
%    This file is part of PHP/Erlang.
%
%    PHP/Erlang is free software: you can redistribute it and/or
%    modify it under the terms of the GNU Lesser General Public
%    License as published by the Free Software Foundation, either
%    version 3 of the License, or (at your option) any later version.
%
%    PHP/Erlang is distributed in the hope that it will be useful, but
%    WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%    Lesser General Public License for more details.
%
%    You should have received a copy of the GNU Lesser General Public
%    License along with PHP/Erlang.  If not, see
%    <http://www.gnu.org/licenses/>.

-module( echo ).
-export( [ start/0, echo/0 ] ).

start() ->
    register( echo, spawn( echo, echo, [ ] ) ).

echo() ->
    receive
        { Pid, X } ->
            io:fwrite( "Got ~p.~n", [ X ] ),
            Pid ! X,
            echo();
        quit -> ok
    end.
    
