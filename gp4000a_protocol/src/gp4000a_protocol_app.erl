-module(gp4000a_protocol_app).
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
  Pid = spawn_link(fun() ->
    s508_device:init(),
    (fun Loop() ->
      receive
        _ -> Loop()
      end
    end)()
  end),
  { ok, Pid }.

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
