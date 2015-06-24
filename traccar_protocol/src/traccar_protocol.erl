%%
%% Sample protocol for Traccar Android client ( https://github.com/tananaev/traccar-client-android )
%%

-module(traccar_protocol).
-author("Sergey Loguntsov").

-behaviour(cl_protocol).
-export([
  start/2, stop/1,
  init/2, terminate/3,
  handle_frame_in/3, handle_frame_out/3, handle_info/3,
  get_device_login/1
]).

-include_lib("calypso_listener/include/logger.hrl").

-record(state, {}).

start(Port, Options) ->
  cl_tcp_transport:start_listener(?MODULE, Port, Options).
stop(Port) ->
  cl_tcp_transport:stop_listener(Port).

init(_Options, Protocol) ->
  ?INFO("Connected ~p", [ ?MODULE ]),
  { ok, #state{}, Protocol }.
terminate(Reason, _State, _Protocol) ->
  ?INFO("Disconnected ~p ~p", [ ?MODULE, Reason ]),
  ok.

get_device_login(_) -> undefined.

handle_frame_in(Data, State, Protocol) when size(Data) > 300 ->
  {{stop, bad_packet}, State, Protocol };
handle_frame_in(<<>>, State, Protocol) ->
  NewProtocol = cl_transport:set_rest(<<>>, Protocol),
  { ok, State, NewProtocol };
handle_frame_in(<<"$PGID,", B0/binary>> = B, State, Protocol) ->
  lager:info("Connect ~p", [ B ]),
  case binary:split(B0, <<"*">>) of
    [ Uid, Msg ] ->
      lager:info("UID ~p", [ Uid ]),
      NewProtocol0 = cl_transport:set_rest(Msg, Protocol),
      case cl_transport:set_device_login(Uid, NewProtocol0) of
        { ok, NewProtocol1, _ } ->
          { ok, State, NewProtocol1 };
        undefined -> throw({{stop, bad_device}, State, Protocol})
      end;
    _ ->
      lager:info("Bad packet ~p", [ B ]),
      { { stop, bad_packet }, State, Protocol }
  end;
handle_frame_in(<<"$TRCCR,", Bin/binary>> = B, State, Protocol) ->
  case binary:split(Bin, <<"\r\n">>) of
    [ Msg, Rest ] ->
      lager:info("Extended message ~p", [ B ]),
      [ _TimeBin , <<"A">>, YBin, XBin, SpeedBin, CourseBin, _ZBin, BatteryBin, _ ] = binary:split(Msg, <<",">>, [ global ]),
      %%<< YearBin:4/bytes, MonthBin:2/bytes, DayBin:2/bytes, HourBin:2/bytes, MinuteBin:2/bytes, SecondsBin:2/bytes, _/binary >> = TimeBin,
      Telemetry = cl_telemetry:new(calypso_time:now(), #{
        device_time => erlang:localtime(),
        gps => cl_gps:new({binary_to_float(XBin), binary_to_float(YBin)}),
        speed => binary_to_float(SpeedBin),
        direction => binary_to_float(CourseBin),
        battery_internal => binary_to_integer(BatteryBin)
      }),
      lager:info("Telemetry ~p", [ Telemetry ]),
      NewProtocol = cl_transport:set_telemetry(Telemetry, Protocol),
      handle_frame_in(Rest, State, NewProtocol);
    [ Msg ] ->
      lager:info("Frame_in ~p", [ B ]),
      {{ rest, Msg}, State, Protocol}
  end;
handle_frame_in(<<"$GPRMC,", Bin/binary>> = B, State, Protocol) ->
  case binary:split(Bin, <<"\r\n">>) of
    [ Msg, _Rest ] ->
      lager:info("Message ~p", [ Msg ]),
      {{rest, Msg}, State, Protocol };
    [ Msg ] ->
      lager:info("Frame_in ~p", [ B ]),
      {{rest, Msg}, State, Protocol }
  end;
handle_frame_in(<< _Char:1/bytes, B/binary>>, State, Protocol) ->
  handle_frame_in(B, State, Protocol).

handle_frame_out(_, State, Protocol) ->
  {{reply, { error, unsupported }}, State, Protocol }.

handle_info(_, State, Protocol) ->
  { ok, State, Protocol }.

