%%%-------------------------------------------------------------------
%%% @author tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%% @copyright (C) 2009, tanmenglong
%%% @doc
%%% File utilities
%%% @end
%%% Created : 26 Dec 2009 by tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%%-------------------------------------------------------------------
-module(erlex_file).

-author("tanmenglong").

-export([file_size/1, mkdir/1, mkdir/2]).

%%--------------------------------------------------------------------
%% @doc
%% Equals to mkdir
%%
%% @spec mkdir(Path) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
mkdir(Path) ->
    case file:make_dir(Path) of
      N when N =:= ok; N =:= {error, eexist} -> ok;
      E -> E
    end.

%%--------------------------------------------------------------------
%% @doc
%% Equals to mkdir -p
%%
%% @spec mkdir(Path, Args) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
mkdir("/" ++ RelPath,
      "-p") -> %% absolute path
    do_mkdir("/", string:tokens(RelPath, "/"));
mkdir(RelPath,
      "-p") -> %% relative path
    do_mkdir("./", string:tokens(RelPath, "/")).

do_mkdir(BaseDir, [H | R] = DirList)
    when is_list(DirList) ->
    Dir = BaseDir ++ H,
    case mkdir(Dir) of
      {error, _} = E0 -> E0;
      ok -> do_mkdir(Dir ++ "/", R)
    end;
do_mkdir(_, []) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get file size
%%
%% @spec file_size(IoDevice) -> {ok, Size} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
file_size(IoDevice) ->
    Ret = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Ret.
