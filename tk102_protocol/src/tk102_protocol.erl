-module(tk102_protocol).
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
  ?INFO("Connected ~p", [ ?MODULE ]),
  { ok, #state{}, Protocol }.
terminate(Reason, _State, _Protocol) ->
  lager:info("Disconnected ~p ~p", [ ?MODULE, Reason ]),
  ok.

handle_frame_in(Binary, State, Protocol) ->
  [ Packet, Rest ] = binary:split(Binary, <<"\n\r">>),
  case binary:split(Packet, <<",">>, [ global ]) of
    [ BTime, _AdminMobile, <<"GPRMC">>, _CurrentTime, <<"A">>,
      Longtitude, LongtitudeDirection, Latitude, LatitudeDirection,
      BSpeed, BDirection, _Date, _, _, _CRC16_0, BIsGpsValid, Status,
      <<" imei:", BImei/binary>>, BCountOfSatelites, BHeight, <<"F:", PowerVoltage/binary>>, BIsCharging, _BodyLength, _CRC16_1,
      _MCC, _MNC, _LAC, _CellID | Properties
    ] ->

% [<<"150507224139">>,<<"39150161">>,<<"GPRMC">>,<<"194139.000">>,<<"A">>,<<"2609.3958">>,<<"N">>,<<"05031.1647">>,<<"E">>,<<"0.00">>,<<"0.00">>,<<"070515">>,<<>>,<<>>,<<"A*66">>,<<"F">>,<<>>,<<" imei:013226003474604">>,<<"05">>,<<"21.2">>,<<"F:4.28V">>,<<"1">>,<<"133">>,<<"495">>,<<"426">>,<<"02">>,<<"0960">>,<<"2F8C">>,<<"Oil=53%">>,<<"T=100">>,<<"RFID=">>]
%[<<"150507224139">>,<<"39150161">>,<<"GPRMC">>,<<"194139.000">>,<<"A">>,
% <<"2609.3958">>,<<"N">>,<<"05031.1647">>,<<"E">>,
% <<"0.00">>,<<"0.00">>,<<"070515">>,<<>>,<<>>,<<"A*66">>,<<"F">>,<<>>,<<" imei:013226003474604">>,<<"05">>,<<"21.2">>,<<"F:4.28V">>,<<"1">>,<<"133">>,<<"495">>,<<"426">>,<<"02">>,<<"0960">>,<<"2F8C">>,<<"Oil=53%">>,<<"T=100">>,<<"RFID=">>]
        NewProtocol0 = case cl_tcp:device(Protocol) of
          undefined ->
            case cl_tcp:set_device_login(BImei, Protocol) of
              { ok, Prot, _ } -> Prot;
              undefined -> throw({{stop, bad_device}, State, Protocol})
            end;
          _ -> Protocol
        end,
        << BYear:2/binary, BMonth:2/binary, BDay:2/binary, BHour:2/binary, BMinutes:2/binary, BSeconds:2/binary>> = BTime,
        Time = {
          { 2000 + binary_to_integer(BYear), binary_to_integer(BMonth), binary_to_integer(BDay) },
          { binary_to_integer(BHour), binary_to_integer(BMinutes), binary_to_integer(BSeconds)}
        },
        X = parse_coordinate(Longtitude, LongtitudeDirection),
        Y = parse_coordinate(Latitude, LatitudeDirection),
        Z = binary_to_float(BHeight),
        Gps = cl_gps:new({X, Y, Z}),
        Speed = binary_to_float(BSpeed) * 1.852,
        Direction = binary_to_float(BDirection),
        IsGpsValid = BIsGpsValid =:= <<"F">>,
        CountOfSatelites = binary_to_integer(BCountOfSatelites),
        [ BVoltage | _ ] = binary:split(PowerVoltage, <<"V">>),
        Voltage = binary_to_float(BVoltage),
        IsCharging = BIsCharging =:= <<"1">>,
        PropertyMap = maps:from_list(parse_status(Status) ++ lists:foldl(fun(Bin, Acc) ->
          case parse_property(Bin) of
            { Key, Value } -> [ {Key, Value } | Acc ];
            undefined -> Acc
          end
        end, [], Properties)),
        Data = maps:merge(PropertyMap, #{
          device_time => Time,
          gps => Gps,
          speed => Speed,
          direction => Direction,
          is_gps_valid => IsGpsValid,
          satelites => CountOfSatelites,
          voltage => Voltage,
          is_charging => IsCharging,
          sos_alarm => Status =:= <<"help me">>,
          is_battery_works => Status =:= <<"battery">>
        }),
        Telemetry = cl_telemetry:new(calypso_time:now(), Data),
        NewProtocol1 = cl_tcp:set_telemetry(Telemetry, NewProtocol0),
        {{rest, Rest}, State, NewProtocol1 };
    _ ->
      ?ERROR("Bad packet ~p", [ Packet ]),
      {{stop, bad_packet}, State, Protocol }
  end.

parse_status(<<"ACCStart">>) ->
  [ {is_ignition_on, true} ];
parse_status(<<"ACCStop">>) ->
  [ {is_ignition_on, false} ];
parse_status(_) -> [].


handle_frame_out({raw, Binary}, State, Protocol) when is_binary(Binary) ->
  {{reply, Binary}, State, Protocol};

handle_frame_out(_, State, Protocol) ->
  {{reply, { error, unsupported }}, State, Protocol }.

handle_info(_, State, Protocol) ->
  { ok, State, Protocol }.

parse_coordinate(Binary, Direction) ->
  D = binary_to_float(Binary),
  X = D / 100,
  Int = trunc(X),
  Result = Int + (X - Int) * 100 / 60,
  case Direction of
    <<"N">> -> Result;
    <<"W">> -> -Result;
    <<"S">> -> -Result;
    <<"E">> -> Result
  end.

parse_property(<<"Oil=">>) -> undefined;
parse_property(<<"Oil=", Rest/binary>>) ->
  [ Value | _ ] = binary:split(Rest, <<"%">>),
  { fuel, binary_to_integer(Value) };

parse_property(<<"T=">>) -> undefined;
parse_property(<<"T=", Temperature/binary>>) ->
  { temperature, binary_to_integer(Temperature) };

parse_property(<<"RFID=">>) -> undefined;
parse_property(<<"RFID=", RFID/binary>>) ->
  { rfid, binary_to_integer(RFID) };

parse_property(_) -> undefined.

