-module(vh_akm).

%% API
-export([
    issue_key/1,
    get_key/1,
    list_keys/0,
    list_keys/1,
    request_revoke_key/1,
    revoke_key/1
]).

issue_key(ApiKey) ->
    {ok, PartyId} = application:get_env(vality_helper, party_id),
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
    Body = jsx:encode(ApiKey),
    Headers = assemble_headers(),
    ConnPid = vh_common:connect(),
    Answer = vh_common:post(ConnPid, Path, Headers, Body),
    vh_common:disconnect(ConnPid),
    Answer.

get_key(ApiKeyId) ->
    {ok, PartyId} = application:get_env(vality_helper, party_id),
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary>>,
    Headers = assemble_headers(),
    ConnPid = vh_common:connect(),
    Answer = vh_common:get(ConnPid, Path, Headers),
    vh_common:disconnect(ConnPid),
    Answer.

list_keys() ->
    list_keys([{<<"limit">>, <<"1000">>}]).

list_keys(QsList) ->
    {ok, PartyId} = application:get_env(vality_helper, party_id),
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
    Headers = assemble_headers(),
    PathWithQuery = vh_common:maybe_query(Path, QsList),
    ConnPid = vh_common:connect(),
    Answer = vh_common:get(ConnPid, PathWithQuery, Headers),
    vh_common:disconnect(ConnPid),
    Answer.

request_revoke_key(ApiKeyId) ->
    {ok, PartyId} = application:get_env(vality_helper, party_id),
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary, "/status">>,
    Headers = assemble_headers(),
    Body = jsx:encode(#{<<"status">> => <<"revoked">>}),
    ConnPid = vh_common:connect(),
    Answer = vh_common:put(ConnPid, Path, Headers, Body),
    vh_common:disconnect(ConnPid),
    Answer.

revoke_key(PathWithQuery) ->
    Headers = assemble_headers(),
    ConnPid = vh_common:connect(),
    Answer = vh_common:get(ConnPid, PathWithQuery, Headers),
    vh_common:disconnect(ConnPid),
    Answer.

% Internal functions

assemble_headers() ->
    {ok, AccessToken} = application:get_env(vality_helper, access_token),
    assemble_headers(AccessToken).

assemble_headers(AccessToken) ->
    RequestId = base64:encode(uuid:get_v4()),
    assemble_headers(AccessToken, RequestId).

assemble_headers(AccessToken, RequestId) ->
    {ok, AccessToken} = application:get_env(vality_helper, access_token),
    {ok, Origin} = application:get_env(vality_helper, origin),
    io:format(user, "Set headers~nX-Request-ID = ~p~nOrigin = ~p~n", [RequestId, Origin]),
    [
        {<<"X-Request-ID">>, RequestId},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Origin">>, Origin},
        {<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}
    ].
