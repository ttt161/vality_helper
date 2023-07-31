-module(vh_auth).

%% API
-export([
    auth/2,
    auth/3
]).

auth(Login, Password) ->
    auth(Login, Password, #{}).

auth(Login0, Password0, Opts) ->
    Login = unicode:characters_to_binary(Login0),
    Password = unicode:characters_to_binary(Password0),
    {ok, Host} = application:get_env(vality_helper, auth_host),
    {ok, Port} = application:get_env(vality_helper, auth_port),
    Path = <<"/auth/realms/external/protocol/openid-connect/token">>,
    Headers = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
    ],
    Body = <<"username=", Login/binary,"&password=", Password/binary, "&client_id=common-api&grant_type=password">>,
    ConnPid = vh_common:connect(Host, Port, Opts),
    Result = case vh_common:post(ConnPid, Path, Headers, Body) of
        {200, RespHeaders, RespBody} ->
            #{<<"access_token">> := AccessToken} = jsx:decode(RespBody, [return_maps]),
            application:set_env(vality_helper, access_token, AccessToken),
            case lists:keyfind(<<"x-frame-options">>, 1, RespHeaders) of
                {_, Value} ->
                    Origin = binary:replace(Value, <<"ALLOW-FROM ">>, <<>>),
                    application:set_env(vality_helper, origin, Origin);
                _ -> skip
            end,
            {ok, AccessToken};
        BadResponse ->
            {error, BadResponse}

    end,
    vh_common:disconnect(ConnPid),
    Result.
