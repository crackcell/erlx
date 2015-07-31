%%%-------------------------------------------------------------------
%%% @author tanmenglong <>
%%% @copyright 2009, tanmenglong
%%% @doc
%%% Test case for kerltools_config
%%% @end
%%% Created : 10 Dec 2009 by tanmenglong <>
%%%-------------------------------------------------------------------
-module(erlex_config_SUIT).
-author("tanmenglong").

-include_lib("eunit/include/eunit.hrl").

kerltools_config_put_test() ->
	{ok, _} = kerltools_config:start_link("../release/kerltools.config"),
	ok = kerltools_config:put(name, tml),
	{ok, tml} = kerltools_config:get(name).

kerltools_config_save_test() ->
    ok = kerltools_config:save().
    

    
    


