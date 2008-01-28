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

-module( talk ).
-export( [ start/0, talk/0, user/2, t/2 ] ).

start() ->
    ets:new( directory, [ named_table, set, public ] ),
    register( talk, self() ),
    talk().
    
talk() ->
    receive
        { 'DOWN', _, process, User, _ } ->
            ets:match_delete( directory, { '_', User } ),
            talk();

        { login, Pid, Name } ->
            case ets:lookup( directory, Name ) of
                [ { Name, User } ] ->
                    Pid ! { ok, User },
                    talk();
                _ ->
                    User = spawn( ?MODULE, user, [ Name, [ { "system", "Logged in." } ] ] ),
                    erlang:monitor( process, User ),
                    ets:insert( directory, { Name, User } ),
                    Pid ! { ok, User },
                    talk()
            end;

        { logout, Pid, Name } ->
            ets:delete( directory, Name ),
            Pid ! { ok },
            talk();

        { quit } ->
            ets:delete( directory ),
            ok
    end.

user( Name, Messages ) ->
    receive
        { message, N, M } ->
            user( Name, [ { N, M } | Messages ] );

        { read, Pid } ->
            Pid ! { ok, Messages },
            user( Name, [ ] );

        { send, Recipients, Message } ->
            lists:map( fun( N ) ->
                               case ets:lookup( directory, N ) of
                                   [ { N, U } ] -> U ! { message, Name, Message };
                                   _ -> ok
                               end
                       end, Recipients ),
            user( Name, Messages )

    after 3600000 -> exit( timeout )
    end.

t( P, T ) ->
    P ! T,
    receive
        X -> X
    after 2000 -> err
    end.
