-module(rebar3_efmt).

-export([init/1]).


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_efmt_prv:init(State),
    {ok, State1}.
