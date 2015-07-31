%%%-------------------------------------------------------------------
%%% @author tanmenglong <tanmenglong@crackcell.rdev.kingsoft.net>
%%% @copyright (C) 2010, tanmenglong
%%% @doc
%%% Ring queue
%%% @end
%%% Created :  23 Feb 2010 by tanmenglong <crackcell@gmail.com>
%%%-------------------------------------------------------------------
-module(erlex_log).

-author("tanmenglong").

-export([conf/1]).

-export([warn/1, warn/2, warn/3]).

-export([info/1, info/2, info/3]).

-export([error/1, error/2, error/3]).

-export([fatal/1, fatal/2, fatal/3]).

-export([debug/1, debug/2, debug/3]).

conf(ConfFile) -> log4erl:conf(ConfFile).

warn(Log) -> log4erl:log(warn, Log).

%% If 1st parameter is atom, then it is Logger
warn(Logger, Log) when is_atom(Logger) ->
    log4erl:log(Logger, warn, Log, []);
warn(Log, Data) -> log4erl:log(warn, Log, Data).

warn(Logger, Log, Data) ->
    log4erl:log(Logger, warn, Log, Data).

info(Log) -> log4erl:log(info, Log).

info(Logger, Log) when is_atom(Logger) ->
    log4erl:log(Logger, info, Log, []);
info(Log, Data) -> log4erl:log(info, Log, Data).

info(Logger, Log, Data) ->
    log4erl:log(Logger, info, Log, Data).

error(Log) -> log4erl:log(error, Log).

error(Logger, Log) when is_atom(Logger) ->
    log4erl:log(Logger, error, Log, []);
error(Log, Data) -> log4erl:log(error, Log, Data).

error(Logger, Log, Data) ->
    log4erl:log(Logger, error, Log, Data).

fatal(Log) -> log4erl:log(fatal, Log).

fatal(Logger, Log) when is_atom(Logger) ->
    log4erl:log(Logger, fatal, Log, []);
fatal(Log, Data) -> log4erl:log(fatal, Log, Data).

fatal(Logger, Log, Data) ->
    log4erl:log(Logger, fatal, Log, Data).

debug(Log) -> log4erl:log(debug, Log).

debug(Logger, Log) when is_atom(Logger) ->
    log4erl:log(Logger, debug, Log, []);
debug(Log, Data) -> log4erl:log(debug, Log, Data).

debug(Logger, Log, Data) ->
    log4erl:log(Logger, debug, Log, Data).
