%%% author tanmenglong <>
%%% copyright tanmenglong, 2000-2010
%%% Common definations of functions
%%% Created :  9 Dec 2009 by tanmenglong <>

-define(b2l(P), erlang:binary_to_list(P)).
-define(l2b(P), erlang:list_to_binary(P)).
-define(tm2s(T), lists:flatten(io_lib:format("~p", [T]))).
-define(tm2b(T), list_to_binary(io_lib:format("~p", [T]))).
-define(l2i(L), erlang:list_to_integer(L)).
-define(i2l(I), erlang:integer_to_list(I)).
-define(t2l(T), erlang:tuple_to_list(T)).

-define(throw(Reason), erlang:raise(throw, Reason, erlang:get_stacktrace())).

-ifdef(DEBUG).
-define(debug(Log), erlex_log:debug(?tm2s(?MODULE) ++ "::"  ++ Log)).
-define(debug(Log, Data), erlex_log:debug(?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-define(debug(Logger, Log, Data), erlex_log:debug(Logger, ?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-else.
-define(debug(Log), ok).
-define(debug(Log, Data), ok).
-define(debug(Logger, Log, Data), ok).
-endif.

-define(info(Log), erlex_log:info(?tm2s(?MODULE) ++ "::"  ++ Log)).
-define(info(Log, Data), erlex_log:info(?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-define(info(Logger, Log, Data), erlex_log:info(Logger, ?tm2s(?MODULE) ++ "::"  ++ Log, Data)).

-define(warn(Log), erlex_log:warn(?tm2s(?MODULE) ++ "::"  ++ Log)).
-define(warn(Log, Data), erlex_log:warn(?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-define(warn(Logger, Log, Data), erlex_log:warn(Logger, ?tm2s(?MODULE) ++ "::"  ++ Log, Data)).

-define(error(Log), erlex_log:error(?tm2s(?MODULE) ++ "::"  ++ Log)).
-define(error(Log, Data), erlex_log:error(?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-define(error(Logger, Log, Data), erlex_log:error(Logger, ?tm2s(?MODULE) ++ "::"  ++ Log, Data)).

-define(fatal(Log), erlex_log:fatal(?tm2s(?MODULE) ++ "::"  ++ Log)).
-define(fatal(Log, Data), erlex_log:fatal(?tm2s(?MODULE) ++ "::"  ++ Log, Data)).
-define(fatal(Logger, Log, Data), erlex_log:fatal(Logger, ?tm2s(?MODULE) ++ "::"  ++ Log, Data)).


