-module(traccar_protocol_app).
-author("Sergey Loguntsov").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  calypso_listener:register_protocol(traccar_protocol),
  Pid = spawn_link(fun() ->
    calypso_starter:empty_loop(fun() ->
      calypso_listener:register_protocol(traccar_protocol)
    end)
  end),
  { ok, Pid }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
