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
-define(REST_URL, "http://pan.baidu.com/rest/2.0/services/").

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
    Prop = dupan_httpc:get_json(Url),
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
    Params = [{channel, "chunlei"}, {clienttype, 0},
              {web, 1}, {num, 100}, {t, timestamp()},
              {page, Page}, {dir, Dir},
              {"_", timestamp()}],
    Url = ?API_URL ++ "list?" ++ dupan_util:url_encode(Params),
    Prop = dupan_httpc:get_json(Url),
    list_dir_to_file_list(Prop).

list_task() ->
    Params = [{method, "list_task"}, {app_id, 250528},
              {"BDUSS", dupan_httpc:get_cookie("BDUSS")},
              {need_task_info, 1}, {status, 255},
              {t, timestamp()}],
    Url = ?REST_URL "cloud_dl?" ++ dupan_util:url_encode(Params),
    Prop = dupan_httpc:get_json(Url).

add_task(SrcUrl) ->
    add_task(SrcUrl, "/").
add_task(SrcUrl, SavePath) ->
    Url = "http://pan.baidu.com/rest/2.0/services/cloud_dl?",
    Params = [{method, "add_task"}, {app_id, 250528},
              {"BDUSS", dupan_httpc:get_cookie("BDUSS")},
              {source_url, SrcUrl}, {save_path, SavePath}],
    Body = dupan_util:url_encode(Params),
    {Json} = jiffy:decode(dupan_httpc:post_data(Url, Body)),
    Json.

%% http://pan.baidu.com/api/create?a=commit&channel=chunlei&clienttype=0&web=1
%% Request Method:POST
%% path:/Document
%% isdir:1
%% size:
%% block_list:[]
%% method:post
create(Path, dir) ->
    Url = "http://pan.baidu.com/api/create?a=commit&channel=chunlei&clienttype=0&web=1",
    Params = [{path, Path}, {isdir, 1}, {size, ""}, {block_list, "[]"},
              {method, "post"}],
    Body = dupan_util:url_encode(Params),
    {Json} = jiffy:decode(dupan_httpc:post_data(Url, Body)),
    json_to_dir(Json).

create(Path, IsDir, Size, Blocks) ->
    ok.

%% http://pan.baidu.com/api/filemanager?channel=chunlei&clienttype=0&web=1&opera=delete
%% Request Method:POST

delete(FileList) ->
    Url = "http://pan.baidu.com/api/filemanager?channel=chunlei&clienttype=0&web=1&opera=delete",
    Params = [{filelist, format_filelist(FileList)}],
    Body = dupan_util:url_encode(Params),
    io:format("body==> ~p~n", [Body]),
    {Json} = jiffy:decode(dupan_httpc:post_data(Url, Body)),
    case prop_get(<<"errno">>, Json) of
        0 ->
            ok;
        _Other ->
            {error, Json}
    end.




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

json_to_dir(ADir) when is_list(ADir) ->
    1 == prop_get(<<"isdir">>, ADir),
    Name = filename:basename(binary_to_unicode(prop_get(<<"path">>, ADir))),
    #dir{id=prop_get(<<"fs_id">>, ADir),
         path=binary_to_unicode(prop_get(<<"path">>, ADir)),
         name=Name,
         mtime=prop_get(<<"mtime">>,  ADir),
         ctime=prop_get(<<"ctime">>,  ADir)}.

json_to_file(AFile) when is_list(AFile) ->
    0 == prop_get(<<"isdir">>, AFile),
    Name = filename:basename(binary_to_unicode(prop_get(<<"path">>, AFile))),
    #file{id=prop_get(<<"fs_id">>, AFile),
          path=binary_to_unicode(prop_get(<<"path">>, AFile)),
          name=Name,
          mtime=prop_get(<<"mtime">>,  AFile),
          ctime=prop_get(<<"ctime">>,  AFile),
          size=prop_get(<<"size">>, AFile),
          md5=prop_get(<<"md5">>, AFile),
          dlink=prop_get(<<"dlink">>, AFile)}.

prop_get(K, L) ->
    proplists:get_value(K, L).

binary_to_unicode(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin, utf8).

format_filelist(List) ->
    format_filelist(List, "[").
format_filelist([], Acc) ->
    Acc ++ "]";
format_filelist([F|Rest], "[") when is_list(F) ->
    format_filelist(Rest, "[\"" ++ F ++ "\"");
format_filelist([F|Rest], Acc) when is_list(F) ->
    format_filelist(Rest, Acc ++ ",\"" ++ F ++ "\"");
format_filelist(File, _) when is_list(File) ->
    format_filelist([File]).
