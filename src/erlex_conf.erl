%%%-------------------------------------------------------------------
%%% @author tanmenglong <>
%%% @copyright 2009, tanmenglong
%%% @doc
%%% Config loader
%%% @end
%%% Created :  9 Dec 2009 by tanmenglong <>
%%%-------------------------------------------------------------------
-module(erlex_conf).

-author("tanmenglong").

-behaviour(gen_server).

-include("erlex.hrl").

%% API
-export([add/2, get/1, get/2, put/2, replace/2, save/0,
	 start_link/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-define(SERVER, ?MODULE).

-define(TAB_NAME, '$erlex_conf_confs').

-record(state, {conf, tab}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
%% @spec start_link(Conf) -> {ok, Pid} | ignore | {error, Error}
start_link(Conf) when is_list(Conf) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE,
			  [Conf], []).

%% @doc Gets a conf value by the Key
%% @spec get(Key) -> {ok, Value} | {error, undefined}
get(Key) ->
    gen_server:call({global, ?SERVER}, {get, Key}).

%% @doc Gets a conf value by the key
%% @spec get(Key, Default) -> {ok, Value}
get(Key, Default) ->
    gen_server:call({global, ?SERVER}, {get, Key, Default}).

%% @doc Puts a key-value conf into memory
%% @spec put(Key, Value) -> ok
put(Key, Value) ->
    gen_server:call({global, ?SERVER}, {put, Key, Value}).

%% @doc Adds a value if the key is not there
%% @spec add(Key, Value) -> ok | {error, exist}
add(Key, Value) ->
    gen_server:call({global, ?SERVER}, {add, Key, Value}).

%% @doc Replaces an existing value
%% @spec replace(Key, Value) -> ok | {error, undefined}
replace(Key, Value) ->
    gen_server:call({global, ?SERVER},
		    {replace, Key, Value}).

%% @doc Saves conf into file
%% @spec save() -> ok | {error, Reason}
save() -> gen_server:call({global, ?SERVER}, {save}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initiates the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([Conf]) ->
    process_flag(trap_exit, true),
    ?info("init:conf[~p]", [Conf]),
    TabId = ets:new(?TAB_NAME, [set, private]),
    case load_conf(Conf, TabId) of
      ok ->
	  ?info("init:load conf succeed", []),
	  {ok, #state{conf = Conf, tab = TabId}};
      E ->
	  erlex_log:error("init:load conf failed[~p]", [E]),
	  {stop, E}
    end.

%% @doc Handling call messages
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
handle_call({get, Key}, _From,
	    #state{tab = TabId} = State) ->
    {reply, do_get(TabId, Key), State};
handle_call({get, Key, Default}, _From,
	    #state{tab = TabId} = State) ->
    Reply = case do_get(TabId, Key) of
	      {error, undefined} -> Default;
	      N -> N
	    end,
    {reply, Reply, State};
handle_call({put, Key, Value}, _From,
	    #state{tab = TabId} = State) ->
    {reply, do_put(TabId, Key, Value), State};
handle_call({add, Key, Value}, _From,
	    #state{tab = TabId} = State) ->
    Reply = case do_get(TabId, Key) of
	      {error, undefined} -> do_put(TabId, Key, Value);
	      _ -> {error, exist}
	    end,
    {reply, Reply, State};
handle_call({replace, Key, Value}, _From,
	    #state{tab = TabId} = State) ->
    Reply = case ets:member(TabId, Key) of
	      true -> do_put(TabId, Key, Value);
	      false -> {error, undefined}
	    end,
    {reply, Reply, State};
handle_call({save}, _From,
	    #state{conf = Conf, tab = TabId} = State) ->
    Reply = case file:open(Conf, [write]) of
	      {ok, FD} ->
		  %% catching exceptions ensures file's closing
		  catch save_to_file(ets:tab2list(TabId), FD),
		  file:close(FD);
	      E ->
		  erlex_log:error_details("save:open file failed[~p]",
					  [E]),
		  E
	    end,
    {reply, Reply, State}.

%% @doc Handling cast messages
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(_Msg, State) -> {noreply, State}.

%% @doc Handling all non call/cast messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info(_Info, State) -> {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) -> ok.

%% @doc Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Load conf from file
%% @spec load_conf(Conf, TabId) -> ok | {error, Reason}
load_conf(Conf, TabId) ->
    case file:consult(Conf) of
      {ok, ConfList} -> read_list(ConfList, TabId);
      E ->
	  erlex_log:error_details("load_conf:file error[~p]",
				  [E]),
	  E
    end.

%% @doc Put a conf list into ets table
%% @spec read_list(ConfList, TabId) -> ok | {error, Reason}
read_list([Conf | R], TabId) when is_tuple(Conf) ->
    ets:insert(TabId, Conf), read_list(R, TabId);
read_list([], _) -> ok;
read_list(_, _) ->
    erlex_log:error_details("read_list:invalid conf term",
			    []),
    {error, invalid_conf_term}.

%% @doc Internal GET function
%% @spec do_get(TabId, Key) -> {ok, Value} | {error, undefined}
do_get(TabId, Key) ->
    case ets:lookup(TabId, Key) of
      [{_, Value}] -> Value;
      _ -> {error, undefined}
    end.

%% @doc Puts a value into conf
%% @spec do_put(TabId, Key, Value) -> ok
do_put(TabId, Key, Value) ->
    ets:insert(TabId, {Key, Value}), ok.

%% @doc Saves conf into file
%% @spec save_to_file(ConfList, FD) -> ok | {error, Reason}
save_to_file([Conf | R], FD) ->
    io:format(FD, "~p.~n", [Conf]), save_to_file(R, FD);
save_to_file([], _) -> ok.
