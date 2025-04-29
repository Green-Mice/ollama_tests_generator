%%%-------------------------------------------------------------------
%% @doc ollama_tests_generator public API
%% @end
%%%-------------------------------------------------------------------

-module(ollama_tests_generator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ollama_tests_generator_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
