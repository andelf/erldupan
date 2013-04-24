%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(dupan_util).

%% API
-export([url_encode/1, escape_uri/1, timestamp/1, timestamp/0, uri_unparse/1, set_nth/3]).
-import(lists, [flatten/1]).

%%%===================================================================
%%% API
%%%===================================================================

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

timestamp() ->
    timestamp(now()).
timestamp(Now) ->
    {A, B, C} = Now,
    io_lib:format("~p~p~p", [A, B, C div 1000]).

%% inverse of http_uri:parse/1
uri_unparse({http, [], Host, 80, Path, Query}) ->
    flatten(io_lib:format("http://~s~s~s", [Host, Path, Query]));
uri_unparse({Scheme, [], Host, Port, Path, Query}) ->
    flatten(io_lib:format("~p://~s:~i~s~s", [Scheme, Host, Port, Path, Query]));
uri_unparse({Scheme, UserInfo, Host, Port, Path, Query}) ->
    flatten(io_lib:format("~p://~s@~s:~i~s~s", [Scheme, UserInfo, Host, Port, Path, Query])).

%% lists:nth, then set
set_nth(I, New, List) ->
    set_nth(I, New, List, []).
set_nth(1, New, [_|Rest], Acc) ->
    lists:reverse(Acc) ++ [New|Rest];
set_nth(I, New, [E|Rest], Acc) when I > 1 ->
    set_nth(I-1, New, Rest, [E|Acc]).


%%%===================================================================
%%% Internal functions
%%%===================================================================
escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $A].                              % $a -> $A
