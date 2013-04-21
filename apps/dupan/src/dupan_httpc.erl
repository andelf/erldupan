%%%-------------------------------------------------------------------
%%% @author Wang Shuyu <wangshuyu@FeatherAir>
%%% @copyright (C) 2013, Wang Shuyu
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2013 by Wang Shuyu <wangshuyu@FeatherAir>
%%%-------------------------------------------------------------------
-module(dupan_httpc).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).
-export([get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HEADERS, [{"user-agent",
                   "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_5) "
                   "AppleWebKit/537.36 (KHTML, like Gecko) "
                   "Chrome/27.0.1453.6 Safari/537.36"}]).
-define(PROFILE, baidu_disk).

-record(state, {username}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get(Url) ->
    gen_server:call(?SERVER, {get, Url}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Username = "000000000",
    Password = "111111111",
    inets:start(),
    ssl:start(),
    inets:start(httpc,
                [{profile, ?PROFILE},
                 {data_dir, "/Users/wangshuyu/Repos/erldupan/data/"}]),
    httpc:set_options([{cookies, enabled}], ?PROFILE),
    % {verbose, debug}])
    case test_login() of
        ok ->
            {ok, #state{username=Username}};
        need_login ->
            case do_login(Username, Password) of
                ok ->
                    {ok, #state{username=Username}};
                Other ->
                    {stop, Other}
            end
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Url}, _From, State) ->
    {ok, {_, _, Body}} = httpc:request(
                           get,
                           {Url,
                            ?HEADERS},
                           [], [], ?PROFILE),
    lager:info("get ~p body:~p~n", [Url, Body]),
    {Result} = jiffy:decode(Body),
    {reply, Result, State};
handle_call({quota}, _From, State) ->
    {ok, {_, _, Body}} = httpc:request(
                           get,
                           {"http://pan.baidu.com/api/quota?channel=chunlei&clienttype=0&web=1",
                            ?HEADERS},
                           [], [], ?PROFILE),
    lager:info("quota ret: ~p~n", [Body]),
    lager:info("parsed ret: ~p~n", [jiffy:decode(Body)]),
    {Prop} = jiffy:decode(Body),
    Result = case proplists:get_value(<<"errno">>, Prop) of
                 0 ->
                     {ok,
                      {proplists:get_value(<<"used">>, Prop),
                       proplists:get_value(<<"total">>, Prop)}};
                 Code ->
                     {error, {code, Code}}
             end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled request ~p~n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    inets:stop(httpc, dupan),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_login(Username, Password) ->
    Token = get_token(),
    login_check(Username, Token),
    login(Username, Password, Token),
    test_login().

get_token() ->
    httpc:request("https://passport.baidu.com/v2/?login", ?PROFILE),
    ApiInfoReq = {"https://passport.baidu.com/v2/api/?getapi&class=login&tpl=pp&apiver=v3", ?HEADERS},
    {ok, {_,_,Body}} = httpc:request(get, ApiInfoReq, [], [], ?PROFILE),
    lager:info("body: ~s~n", [Body]),
    {match,[[Token]]} = re:run(Body, "token\" : \"([^'\"]+?)\"", [global,{capture,[1],list}]),
    lager:info("debug cookie: ~p~n", [httpc:which_cookies(?PROFILE)]),
    Token.

login(Username, Password, Token) ->
    Params = [{"staticpage", "https://passport.baidu.com/v3Jump.html"},
              {"charset", "UTF-8"},
              {"token", Token},
              {"tpl", "pp"},
              {"apiver", "v3"},
              {"tt", timestamp()},
              {"codestring", ""},
              {"isPhone", "false"},
              {"safeflg", "0"},
              {"u", ""},
              {"username", Username},
              {"password", Password},
              {"verifycode", ""},
              {"mem_pass", "on"},
              {"ppui_logintime", "63409"},
              {"callback", "parent.bd__pcbs__ospy4m"}],
    %% io:format("~s~n", [url_encode(Params)]),
    {ok, {_,Headers,Body}} = httpc:request(post,
                                     {"https://passport.baidu.com/v2/api/?login", ?HEADERS,
                                     "application/x-www-form-urlencoded", url_encode(Params)},
                                     [], [], ?PROFILE),
    %% httpc cookie can't handle domain=xxx.com correctly
    httpc:store_cookies([{"set-cookie", re:replace(Cookie,"domain=baidu","domain=.baidu",[{return,list}])} ||
                            {"set-cookie", Cookie} <- Headers],
                        "https://passport.baidu.com/v2/api/?login",
                        ?PROFILE),
    %% io:format("debug ~p~n", [[{"set-cookie", Cookie} || {"set-cookie", Cookie} <- Headers]]),
    {match, [[_JumpUrl, JumpQuery]]} = re:run(Body,
                                  "decodeURIComponent\\((\".+?\")\\)\\+\"(.+?)\"", [global, {capture,[1,2],list}]),
    {ok, [{string, 1, JumpUrl}|_], _} = erl_scan:string(_JumpUrl),
    lager:info("got jump url:  ~s~n", [[JumpUrl|JumpQuery]]),
    httpc:request(JumpUrl ++ JumpQuery, ?PROFILE),
    ok.

login_check(Username, Token) ->
    Params = [{"token", Token},
              {"tpl", "pp"},
              {"apiver", "v3"},
              {"tt", timestamp()},
              {"username", Username},
              {"isphone", "false"},
              {"callback", "cb"}],
    io:format("login check: ~p~n", [httpc:request(get,
                                                  {"https://passport.baidu.com/v2/api/?logincheck&" ++ url_encode(Params),
                                                   ?HEADERS},
                                                  [], [], ?PROFILE)]),
    httpc:request("https://passport.baidu.com/passApi/html/_blank.html", ?PROFILE).



test_login() ->
    {ok, {{_,302,_},Headers,[]}} =httpc:request(get,
                                        {"https://passport.baidu.com/", ?HEADERS},
                                        [{autoredirect, false}], [], ?PROFILE),
    %% erlang does not support relatie location redirect
    Location = proplists:get_value("location", Headers),
    case Location of
        "http://passport.baidu.com/center" ->
            ok;
        "https://passport.baidu.com/v2/?login" ->
            need_login;
        _ ->
            {error, Location}
    end.

url_encode(Data) ->
    url_encode(Data,"").
url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, escape_uri(Key) ++ "=" ++ escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ escape_uri(Key) ++ "=" ++ escape_uri(Value)).

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(S) when is_atom(S) ->
    escape_uri(atom_to_list(S));
escape_uri(S) when is_integer(S) ->
    escape_uri(integer_to_list(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

timestamp() ->
    io_lib:format("~p~p~p", tuple_to_list(now())).
