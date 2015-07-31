%%%-------------------------------------------------------------------
%%% @author tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%% @copyright (C) 2009, tanmenglong
%%% @doc
%%% Database helper
%%% @end
%%% Created : 24 Dec 2009 by tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%%-------------------------------------------------------------------
-module(erlex_db).

-author("tanmenglong").

-export([mnesia_delete/1, mnesia_next/2, mnesia_prev/2,
	 mnesia_query/1, mnesia_trans/1, mnesia_write/1]).

-include_lib("stdlib/include/qlc.hrl").

%% @doc Wrapper for mnesia:write/1
%% @spec mnesia_write(Data) -> ok | {error, Reason}
mnesia_write(Data) ->
    F = fun () -> mnesia:write(Data) end,
    case mnesia:transaction(F) of
      {atomic, _} -> ok;
      {aborted, Reason} -> {error, Reason}
    end.

%% @doc Wrapper for mnesia:delete_object/1
%% @spec mnesia_delete(Item) -> ok | {error, Reason}
mnesia_delete(Item) ->
    F = fun () -> mnesia:delete_object(Item) end,
    case mnesia:transaction(F) of
      {atomic, _} -> ok;
      {aborted, Reason} -> {error, Reason}
    end.

%% @doc Wrapper for mnesia:next/2
%% @spec mnesia_next(Tab, Key) -> Key | {error, Reason}
mnesia_next(Tab, Key) ->
    F = fun () -> mnesia:next(Tab, Key) end,
    case mnesia:transaction(F) of
      {atomic, Val} -> Val;
      {aborted, Reason} -> {error, Reason}
    end.

%% @doc Wrapper for mnesia:prev/2
%% @spec mnesia_prev(Tab, Key) -> ok | {error, Reason}
mnesia_prev(Tab, Key) ->
    F = fun () -> mnesia:prev(Tab, Key) end,
    case mnesia:transaction(F) of
      {atomic, _} -> ok;
      {aborted, Reason} -> {error, Reason}
    end.

%% @doc Execute a query
%% @spec mnesia_query(Q) -> {ok, Value} | {error, Reason}
mnesia_query(Q) -> mnesia_trans(fun () -> qlc:e(Q) end).

%% @doc Wrapper for mnesia:transaction/1
%% @spec mnesia_trans(Fun) -> {ok, Val} | {error, Reason}
mnesia_trans(Fun) when is_function(Fun) ->
    case mnesia:transaction(Fun) of
      {atomic, Value} -> {ok, Value};
      {aborted, Reason} -> {error, Reason}
    end.
