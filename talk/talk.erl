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
