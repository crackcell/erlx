%%%-------------------------------------------------------------------
%%% @author tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%% @copyright (C) 2010, tanmenglong
%%% @doc
%%% List utilities
%%% @end
%%% Created :  8 Jan 2010 by tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%%-------------------------------------------------------------------
-module(erlex_list).

-author("tanmenglong").

-export([successor/2]).

%% @doc Find next item in a list
%% @spec successor(queue_id, current_item) -> NextItem |
%%                                            'end_of_list' |
%%                                            'not_exist'
successor(CurrentItem, [CurrentItem | R]) ->
    case R of
      [] -> end_of_list;
      [NextItem | _] -> NextItem
    end;
successor(CurrentItem, [_ | R]) ->
    successor(CurrentItem, R);
successor(_, []) -> not_exist.
