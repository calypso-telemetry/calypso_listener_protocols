-module(s508_protocol).
-author("begemot").

-behaviour(cl_protocol).

-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  start/2, stop/1,
  init/2, terminate/3,
  handle_frame_in/3, handle_frame_out/3, handle_info/3
]).

-record(state, {
  pseudo_ip :: binary()
}).

start(Port, Options) ->
  cl_tcp_transport:start_listener(?MODULE, Port, Options).
stop(Port) ->
  cl_tcp_transport:stop_listener(Port).


init(_Options, Protocol) ->
  ?INFO("Connected", []),
  { ok, #state{}, Protocol }.
terminate(Reason, _State, _Protocol) ->
  lager:info("Disconnected ~p", [ Reason ]),
  ok.

handle_frame_in(<<16#24, 16#24, Operation:8/unsigned-integer, Length0:16/unsigned-integer, Packet:Length0/binary, Rest/binary>> = Packet0, State, Protocol) when size(Packet0) < 500 ->
  Length = Length0 - 6,
  ?INFO("Operation ~p Length ~p", [ Operation, Length ]),
  case Packet of
    <<PseudoIp:4/binary, Body:Length/binary, _Calibration:1/binary, 16#0D>> ->
      { NewState0, NewProtocol0 } = case { State#state.pseudo_ip, PseudoIp} of
        { undefined, _ } ->
          ?INFO("Login as ~p, operation ~p, body ~p", [ s508_util:bin_to_hex(PseudoIp), Operation, Body ]),
          case cl_tcp:set_device_login(s508_util:bin_to_hex(PseudoIp), Protocol) of
            { ok, NewProtocol, Device } ->
              register(PseudoIp),
              Imei = cl_device:info(imei, Device),
              cl_tcp:register({ imei, Imei}),
              NState = State#state{
                pseudo_ip = PseudoIp
              },
              { NState, NewProtocol };
            _Any ->
              throw({{ stop, not_found }, State, Protocol})
          end;
        {A, A} ->
          { State, Protocol };
        PIP ->
          error(bad_pseudo_ip, [ PIP, PseudoIp ])
      end,
      NewProtocol1 = cl_tcp:set_rest(Rest, NewProtocol0),
      operation(Operation, Body, NewState0, NewProtocol1);
    _ ->
      handle_frame_in(Rest, State, Protocol)
  end;

handle_frame_in(<<16#24, N:1/binary, Rest/binary>>, State, Protocol) when N =/= <<16#24>> ->
  handle_frame_in(Rest, State, Protocol);

handle_frame_in(<<N:1/binary, Rest/binary>>, State, Protocol) when N =/= <<16#24>> ->
  handle_frame_in(Rest, State, Protocol);

handle_frame_in(Body, State, Protocol) ->
  {{rest, Body}, State, Protocol }.

handle_frame_out({raw, Binary}, State, Protocol) when is_binary(Binary) ->
  {{reply, Binary}, State, Protocol};

handle_frame_out({timing, #{ ignition_on := Interval1, ignition_off := Interval2 }} = Options , State, Protocol) ->
  Body = case maps:get(heartbeat, Options, undefined) of
    undefined ->
      <<Interval1:2/unsigned-integer, Interval2:2/unsigned-integer>>;
    Heartbeat when is_integer(Heartbeat) ->
      <<Interval1:2/unsigned-integer, Interval2:2/unsigned-integer, Heartbeat:2/unsigned-integer>>
  end,
  { ok, NewProtocol } = send_packet(16#34, Body, State, Protocol),
  { ok, State, NewProtocol };

handle_frame_out({set_timezone, #{ hours := Hours, minutes := Minutes } = Param}, State, Protocol) ->
  M = abs(Minutes),
  Body = case Hours>0 of
    true ->
      <<Hours:8/unsigned-integer, M:8/unsigned-integer>>;
    false ->
      <<(abs(Hours) bor 128):8/unsigned-integer, M:8/unsigned-integer>>
  end,
  NewProtocol0 = cl_tcp:change_device(fun(Device) ->
    Info = #{ timezone => Param },
    cl_device:info_merge(Info, Device)
  end, Protocol),
  { ok, NewProtocol1 } = send_packet(16#6C, Body, State, NewProtocol0),
  {{reply, ok}, State, NewProtocol1 };

handle_frame_out({ set_timing_interval, Timing1, Timing2 }, State, Protocol) ->
  handle_frame_out({ set_timing_interval, Timing1, Timing2, undefined }, State, Protocol);

handle_frame_out({ set_timing_interval, Timing1, Timing2, undefined }, State, Protocol) ->
  { ok, NewProtocol } = send_packet(16#34, <<Timing1:16/unsigned-integer, Timing2:16/unsigned-integer>>, State, Protocol),
  {{reply, ok}, State, NewProtocol };

handle_frame_out({ set_timing_interval, Timing1, Timing2, Timing3 }, State, Protocol) ->
  NewProtocol0 = cl_tcp:change_device(fun(Device) ->
    Info = #{
      timing_interval_on => Timing1,
      timing_interval_off => Timing2,
      heartbeat_interval => Timing3 },
    cl_device:info_merge(Info, Device)
  end, Protocol),
  { ok, NewProtocol1 } = send_packet(16#34, <<Timing1:16/unsigned-integer, Timing2:16/unsigned-integer, Timing3:8/unsigned-integer>>, State, NewProtocol0),
  {{reply, ok}, State, NewProtocol1 };

handle_frame_out({ set_distance_interval, Distance }, State, Protocol) ->
  NewProtocol0 = cl_tcp:change_device(fun(Device) ->
    Info = #{ fixed_distance_interval => Distance },
    cl_device:info_merge(Info, Device)
  end, Protocol),
  { ok, NewProtocol1 } = send_packet(16#35, <<Distance:16/unsigned-integer>>, State, NewProtocol0),
  {{reply, ok}, State, NewProtocol1 };

handle_frame_out(_, State, Protocol) ->
  {{reply, { error, unsupported }}, State, Protocol }.

handle_info(_, State, Protocol) ->
  { ok, State, Protocol }.

register(PseudoIp) ->
  gproc:add_local_name({ ?MODULE, PseudoIp }).

operation(16#21, <<>>, State, Protocol) ->
  { ok, State, Protocol };

operation(16#80, PositionData, State, Protocol ) ->
  case parse_position_data(PositionData) of
    { ok, Telemetry } ->
      NewProtocol = cl_tcp:set_telemetry(Telemetry, Protocol),
      {ok, State, NewProtocol };
    undefined -> { ok, State, Protocol }
  end;

operation(16#8E, PositionData, State, Protocol) ->
  case parse_position_data(PositionData) of
    { ok, Telemetry } ->
      NewProtocol = cl_tcp:set_telemetry(Telemetry, Protocol),
      {ok, State, NewProtocol };
    undefined -> { ok, State, Protocol }
  end;

operation(_, _, State, Protocol) ->
  { ok, State, Protocol }.

parse_position_data(<<BTimestamp:6/binary, BLatitude:4/binary, BLongitude:4/binary,
    BSpeed:2/binary, BDirection:2/binary, BPositionStatus:1/binary,
    BNoLoad:1/binary, BKeySwitch:1/binary, BAdData:4/binary, BMileAge:32/big-unsigned-integer>>) ->
  <<BYear:1/binary, BMonth:1/binary, BDay:1/binary, BHour:1/binary, BMinute:1/binary, BSec:1/binary>> = BTimestamp,
  [ Year, Month, Day, Hour, Minute, Sec ] = [ s508_bcd:decode({ unsigned, Value}) || Value <- [ BYear, BMonth, BDay, BHour, BMinute, BSec ] ],

  Speed = s508_bcd:decode({unsigned, BSpeed}),
  Direction = s508_bcd:decode({unsigned, BDirection}),
  << MarkFlag:1/bits, _:1/bits, _:2/bits, Satelites:4/unsigned-integer>> = BPositionStatus,
  IsMarkFlag = to_boolean(MarkFlag),
  X = decode_geo(BLongitude),
  Y = decode_geo(BLatitude),
  Gps = cl_gps:new({X, Y}),

  << _:1, 1:1, IsFullLoad:1, IsAntennaShort:1, IsAntennaOpen:1, AirConditionOn:1, BBreakSignal:1, BLowVoltage:1>> = BNoLoad,
  LowVoltage = to_boolean(BLowVoltage),

  IsIgnition = case BKeySwitch of
    <<0>> -> false;
    _ -> true
  end,

  <<OilResistance:16/big-unsigned-integer, BVoltageHigh:8/big-unsigned-integer, BVoltageLow:8/big-unsigned-integer>> = BAdData,

  Telemetry = cl_telemetry:new(calypso_time:now(), #{
    device_time => { {2000+Year, Month, Day}, {Hour, Minute, Sec }},
    speed => Speed,
    direction => Direction,
    gps => Gps,
    is_gps_valid => IsMarkFlag,
    is_low_voltage => LowVoltage,
    is_ignition_on => IsIgnition,
    satelites => Satelites,
    is_antenna_short => to_boolean(IsAntennaShort),
    is_antenna_open => to_boolean(IsAntennaOpen),
    is_aircondition_on => to_boolean(AirConditionOn),
    is_break_on => to_boolean(BBreakSignal),
    is_full_load => to_boolean(IsFullLoad),
    fuel => OilResistance / 10,
    voltage => BVoltageHigh + BVoltageLow / 100,
    mileage => BMileAge
  }),
  { ok, Telemetry };

parse_position_data(_) ->
  undefined.


decode_geo(Binary) when size(Binary) =:= 4 ->
  Value = s508_bcd:decode({signed, Binary }),
  Degree = Value div 100000,
  Minutes = (Value rem 100000) / 1000,
  Degree + Minutes / 60.

to_boolean(<<1:1>>) -> true;
to_boolean(<<1>>) -> true;
to_boolean(1) -> true;
to_boolean(<<0:1>>) -> false;
to_boolean(<<0>>) -> false;
to_boolean(0) -> false.

calibration(<<>>, Acc) -> Acc;
calibration(<<X:8/unsigned-integer, Rest/binary>>, Acc) ->
  calibration(Rest, Acc bxor X).

send_packet(_Type, _Body, #state{pseudo_ip = undefined}, _) ->
  throw({error, bad_pseudo_ip});
send_packet(Type, Body, State, Protocol) when is_integer(Type), is_binary(Body) ->
  Length = size(Body) + 6,
  Start = <<16#24, 16#24, Type:8/unsigned-integer, Length:16/unsigned-integer, (State#state.pseudo_ip):4/binary, Body/binary>>,
  Calibration = calibration(Start, 0),
  Packet = <<Start/binary, Calibration:8/unsigned-integer, 16#0D>>,
  cl_tcp:send(Protocol, Packet).
