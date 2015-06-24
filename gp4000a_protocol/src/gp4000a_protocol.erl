-module(gp4000a_protocol).
-author("Sergey Loguntsov").

-behaviour(cl_protocol).

-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  start/2, stop/1,
  init/2, terminate/3,
  handle_frame_in/3, handle_frame_out/3, handle_info/3,
  get_device_login/1
]).

-record(state, {
  tracker_id :: binary(),
  sn = 0 :: integer(),
  commands = []
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

get_device_login(<<16#24, TrackerId:5/binary, _/binary>>) ->
  { ok, TrackerId };
get_device_login(_) -> undefined.

handle_frame_in(<<16#24, TrackerId:5/binary, ProtocolVersion:4/big-unsigned-integer, DataType:4/big-unsigned-integer,
  Length:16/big-unsigned-integer, Body:Length/binary, _SN:1/binary, Rest/binary>> = Packet, State, Protocol) ->
  lager:info("GPS Data ~p", [ Packet ]),
  { NewState0, NewProtocol0 } = case State#state.tracker_id of
      undefined ->
          Tid = gp4000a_util:bin_to_hex(TrackerId),
          case cl_transport:set_device_login(Tid, Protocol) of
            { ok, NewProtocol, Device } ->
              register(Tid),
              Imei = cl_device:info(imei, Device),
              cl_transport:register({ imei, Imei}, Protocol),
              NState = State#state{
                tracker_id = TrackerId
              },
              { NState, NewProtocol };
            _Any ->
              throw({{ stop, not_found }, State, Protocol})
          end;
      Tid when Tid =:= TrackerId ->
        { State, Protocol };
      _ -> throw({{ stop, bad_packet}, State, Protocol })
    end,
    NewProtocol2 = cl_transport:set_rest(Rest, NewProtocol0),
    parse_body(ProtocolVersion, DataType, Body, NewState0, NewProtocol2);

handle_frame_in(<<$(, R/binary>> = Body, State, Protocol) ->
  lager:info("Command Data ~p", [ Body ]),
  case parse_command(Body) of
    { ok, Command, Params, Rest } ->
      NewProtocol = cl_transport:set_rest(Rest, Protocol),
      [ _TrackerId, <<"2">>, _CommandType, CommandSN | _Other ] = Params,
      NewState = case lists:keysearch(CommandSN, 1, State#state.commands) of
        { value, { _, Cmd, _, _}} ->
          lager:info("Command ~p answer ~p", [ Cmd, Command ]),
          State#state{
            commands = lists:keydelete(CommandSN, 1, State#state.commands)
          };
        false -> State
      end,
      { ok, NewState, NewProtocol };
    { unrecognize, _Command } ->
      {{rest, R}, State, Protocol }
  end;

handle_frame_in(<<N:1/binary, Rest/binary>>, State, Protocol) when N =/= <<16#24>> ->
  handle_frame_in(Rest, State, Protocol);

handle_frame_in(Body, State, Protocol) ->
  {{rest, Body}, State, Protocol }.

handle_frame_out({command, Command}, State, Protocol) when is_binary(Command) ->
  case run_command(Command, State, Protocol) of
    { ok, NewState, NewProtocol } ->
      {{reply, ok}, NewState, NewProtocol };
    { unrecognise, Command, NewState, NewProtocol } ->
      {{reply, { unrecognize, Command }}, NewState, NewProtocol}
  end;

handle_frame_out({raw, Binary}, State, Protocol) when is_binary(Binary) ->
  {[{reply, ok}, {send, Binary}], State, Protocol};

handle_frame_out(_, State, Protocol) ->
  {{reply, { error, unsupported }}, State, Protocol }.

handle_info(_, State, Protocol) ->
  { ok, State, Protocol }.

run_command(Command, State, Protocol) ->
  case parse_command(Command, <<>>) of
    { ok, Command, Params, _Rest } ->
      [ _TrackerId, <<"2">>, _CommandType, CommandSN | _Other ] = Params,
      NewState = State#state{
        commands = [ { CommandSN, Command, Params, undefined } | State#state.commands]
      },
      NewProtocol = cl_transport:send(Command, Protocol),
      { ok, NewState, NewProtocol };
    { unrecognize, Command } ->
      { unrecognize, Command, State, Protocol }
  end.

register(Login) ->
  gproc:add_local_name({ ?MODULE, Login}).

parse_command(<<$(, Body/binary>>) ->
  parse_command(Body, <<$(>>).

parse_command(<<>>, Command) ->
  { unrecognize, Command };

parse_command(<<$), Rest/binary>>, Command) ->
  Params = binary:split(Command, <<",">>, [ global ]),
  { ok, <<Command/binary, $)>>, Params, Rest };

parse_command(<<C:1/binary, Rest/binary>>, Command) ->
  parse_command(Rest, <<Command/binary, C/binary>>).


parse_body(2, 1, <<Body:27/binary>>, State, Protocol) ->
  { ok, Telemetry } = parse_binary_body(Body),
  NewProtocol = cl_transport:set_telemetry(Telemetry, Protocol),
  { ok, State, NewProtocol };

parse_body(2, 2, <<Body:27/binary>>, State, Protocol) ->
  { ok, Telemetry } = parse_binary_body(Body),
  NewProtocol = cl_transport:set_telemetry(Telemetry, Protocol),
  { ok, State, NewProtocol };

parse_body(2, 3, Body, State, Protocol) ->
  { ok, NewProtocol } = parse_body_list(Body, Protocol),
  { ok, State, NewProtocol };

parse_body(2, 4, Body, State, Protocol) ->
  { ok, NewProtocol } = parse_body_list(Body, Protocol),
  { ok, State, NewProtocol }.

parse_body_list(<<Body:27/binary, Rest/binary>>, Protocol) ->
  { ok, Telemetry } = parse_binary_body(Body),
  NewProtocol = cl_transport:set_telemetry(Telemetry, Protocol),
  parse_body_list(Rest, NewProtocol);

parse_body_list(<<>>, Protocol) ->
  { ok, Protocol }.

parse_binary_body(<<BDate:3/binary, BTime:3/binary, BLat:4/binary, BLong:36/bits, BLocating:4/bits, BSpeed:8/big-unsigned-integer, BDirection:8/big-unsigned-integer,
  BFuelHigh:1/binary, BStatus:4/binary, BMileage:4/binary, BFuelLow:1/binary>>) ->
  <<BDay:1/binary, BMonth:1/binary, BYear:1/binary, BHour:1/binary, BMinute:1/binary, BSec:1/binary>> = <<BDate/binary, BTime/binary>>,
  [ Year, Month, Day, Hour, Minute, Sec ] = try
    [ gp4000a_bcd:decode({unsigned, Value}) || Value <- [ BYear, BMonth, BDay, BHour, BMinute, BSec ] ]
  catch
    error:Reason ->
      error(Reason, [ BDate, BTime ])
  end,
  <<1:1, LongSign:1, LatSign:1, IsLocating:1>> = BLocating,

  X = bit_sign(LongSign)*decode_geo(BLong),
  Y = bit_sign(LatSign)*decode_geo(BLat),
  Gps = cl_gps:new({X, Y}),

  Speed = BSpeed * 1.85,
  Direction = BDirection * 2,
  <<Fuel:16/big-unsigned-integer>> = <<BFuelHigh/binary, BFuelLow/binary >>,
  <<Mileage:32/big-unsigned-integer>> = BMileage,

  <<BStatus1:1/binary, BStatus2:1/binary, BStatus3:1/binary, BStatus4:1/binary>> = BStatus,

  <<_:1, VehicleDoorOpen:1, AirConditionOn:1, _:1, _:1, BreakOn:1, EngineOn:1, IgnitionOn:1 >> = BStatus1,
  <<_:1, _:1, IsOverSpeed:1, GpsAntennaShortCircuit:1, GpsAntennaOpenCircuit:1, Sos:1, FuelCut:1, PowerInternalBattery:1>> = BStatus2,
  <<GsmSignal:2/big-unsigned-integer, GpsSignal:2/big-unsigned-integer, TowAlarm:1, FatigueDriving:1, _FlashRamNormaly:1, _FlashNormaly:1 >> = BStatus3,
  <<_:7, _MdtConnectionOn:1 >> = BStatus4,

  Telemetry = cl_telemetry:new(calypso_time:now(), #{
    device_time => { {2000+Year, Month, Day}, {Hour, Minute, Sec }},
    speed => Speed,
    direction => Direction,
    gps => Gps,
    is_gps_valid => to_boolean(IsLocating),
    fuel => Fuel,
    mileage => Mileage,
    gsm_signal_level => GsmSignal,
    gps_signal_level => GpsSignal,
    is_door_open => to_boolean(VehicleDoorOpen),
    is_air_condition_on => to_boolean(AirConditionOn),
    is_break_on => to_boolean(BreakOn),
    is_ignition_on => to_boolean(EngineOn),
    is_acc_on => to_boolean(IgnitionOn),
    is_over_speed => to_boolean(IsOverSpeed),
    is_gps_antenna_short => to_boolean(GpsAntennaShortCircuit),
    is_gps_antenna_open => to_boolean(GpsAntennaOpenCircuit),
    is_sos_alarm => to_boolean(Sos),
    is_fuel_cut => to_boolean(FuelCut),
    is_battery_works => to_boolean(PowerInternalBattery),
    is_tow_alarm => to_boolean(TowAlarm),
    is_driver_fatigue => to_boolean(FatigueDriving)
  }),
  { ok, Telemetry }.

decode_geo(Binary) ->
  Value = gp4000a_bcd:decode({unsigned, Binary}),
  Degree = Value div 1000000,
  Minutes = (Value - Degree * 1000000) / 10000,
  Degree + Minutes / 60.


bit_sign(Value) ->
  case to_boolean(Value) of
    true -> 1;
    false -> -1
  end.

to_boolean(<<1:1>>) -> true;
to_boolean(<<1>>) -> true;
to_boolean(1) -> true;
to_boolean(<<0:1>>) -> false;
to_boolean(<<0>>) -> false;
to_boolean(0) -> false.


