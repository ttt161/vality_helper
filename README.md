# vality_helper

### Usage

Set env in sys.config
```
[
    {vality_helper, [
        {auth_host, "auth.host"},
        {auth_port, 443},
        {api_host, "api.host"},
        {api_port, 443},
        {party_id, <<"your_party_id">>}
    ]}
].
```
Then run shell
```
rebar3 shell
```

```
> Login = "login_string".
> Password = "password_string".
> IssueKey = #{<<"name">> => <<"TestKey1234">>}.
> {ok, AccessToken} = vh_auth:auth(Login, Password).
> vh_akm:issue_key(IssueKey).
```
