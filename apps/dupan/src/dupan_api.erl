%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(dupan_api).

%% API
-export([]).
-compile([export_all]).
-import(dupan_util, [timestamp/0]).
-compile([{parse_transform, lager_transform}]).

-define(API_URL, "http://pan.baidu.com/api/").

-record(file, {id,                              % fs_id
               path,
               name,
               mtime,
               ctime,
               size,
               md5,
               dlink}).

-record(dir, {id,                              % fs_id
              path,
              name,
              mtime,
              ctime}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
quota() ->
    Url = ?API_URL "quota?channel=chunlei&clienttype=0&web=1",
    Prop = dupan_httpc:get(Url),
    case proplists:get_value(<<"errno">>, Prop) of
        0 ->
            {ok,
             {proplists:get_value(<<"used">>, Prop),
              proplists:get_value(<<"total">>, Prop)}};
        Code ->
            {error, {code, Code}}
    end.

list_dir() ->
    list_dir("/").
list_dir(Dir) ->
    list_dir(Dir, 1).
list_dir(Dir, Page) ->
    Params = [{channel, "chunlei"},
              {clienttype, 0},
              {web, 1},
              {num, 100},
              {t, timestamp()},
              {page, Page},
              {dir, Dir},
              {"_", timestamp()}],
    Url = ?API_URL ++ "list?" ++ dupan_util:url_encode(Params),
    Prop = dupan_httpc:get(Url),
    list_dir_to_file_list(Prop).


%%%===================================================================
%%% Internal functions
%%%===================================================================
list_dir_to_file_list(Prop) ->
    case prop_get(<<"errno">>, Prop) of
        0 ->
            lists:map(fun({AFile}) ->
                              case prop_get(<<"isdir">>, AFile) of
                                  1 ->
                                      #dir{id=prop_get(<<"fs_id">>, AFile),
                                           path=binary_to_unicode(prop_get(<<"path">>, AFile)),
                                           name=binary_to_unicode(prop_get(<<"server_filename">>,  AFile)),
                                           mtime=prop_get(<<"server_mtime">>,  AFile),
                                           ctime=prop_get(<<"server_ctime">>,  AFile)};
                                  0 ->
                                      #file{id=prop_get(<<"fs_id">>, AFile),
                                            path=binary_to_unicode(prop_get(<<"path">>, AFile)),
                                            name=binary_to_unicode(prop_get(<<"server_filename">>,  AFile)),
                                            mtime=prop_get(<<"server_mtime">>,  AFile),
                                            ctime=prop_get(<<"server_ctime">>,  AFile),
                                            size=prop_get(<<"size">>, AFile),
                                            md5=prop_get(<<"md5">>, AFile),
                                            dlink=prop_get(<<"dlink">>, AFile)}
                              end
                      end, prop_get(<<"list">>, Prop));
        _ErrorCode ->
            []
    end.

prop_get(K, L) ->
    proplists:get_value(K, L).

binary_to_unicode(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin, utf8).
