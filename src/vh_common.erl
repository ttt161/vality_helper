-module(vh_common).

%% API
-export([
    connect/0,
    connect/2,
    connect/3,
    disconnect/1,
    get/3,
    post/4,
    put/4,
    maybe_query/2
]).

connect() ->
    {ok, Host} = application:get_env(vality_helper, api_host),
    {ok, Port} = application:get_env(vality_helper, api_port),
    connect(Host, Port).

connect(Host, Port) ->
    connect(Host, Port, #{}).

connect(Host, Port, Opts) ->
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    {ok, _} = gun:await_up(ConnPid),
    ConnPid.

disconnect(ConnPid) ->
    gun:close(ConnPid).

get(ConnPid, Path, Headers) ->
    StreamRef = gun:get(ConnPid, Path, Headers),
    get_response(ConnPid, StreamRef).

post(ConnPid, Path, Headers, Body) ->
    StreamRef = gun:post(ConnPid, Path, Headers, Body),
    get_response(ConnPid, StreamRef).

put(ConnPid, Path, Headers, Body) ->
    StreamRef = gun:put(ConnPid, Path, Headers, Body),
    get_response(ConnPid, StreamRef).

get_response(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            {Status, Headers, <<>>};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            {Status, Headers, Body}
    end.

maybe_query(Path, []) ->
    Path;
maybe_query(Path, QsList) ->
    QS = uri_string:compose_query(QsList),
    <<Path/binary, "?", QS/binary>>.
