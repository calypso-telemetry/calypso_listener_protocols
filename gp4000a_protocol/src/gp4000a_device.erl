-module(gp4000a_device).
-author("Sergey Loguntsov").

-include_lib("calypso_core/include/cl_device.hrl").

%% API
-export([
  init/0
]).

-behaviour(cl_device).
-export([new/2, handle_update_info/2]).

-export([
  serialize/1, unserialise/1
]).

new(Id, Data) ->
  Imei = maps:get(imei, Data),
  true = is_integer(Imei),
  Default = #{
  },
  cl_device:new_internal(Id, ?MODULE, #{
    imei => Imei,
    login => integer_to_binary(Imei),
    is_active => maps:get(is_active, Data, undefined)
  }, Default).

handle_update_info(_Info, _Device) -> undefined.

-define(DEVICE_STAMP, <<"s508_device">>).

init() ->
  A = [ imei ], A,
  calypso_serialize_hooks:export_hook(?MODULE, fun ?MODULE:serialize/1),
  calypso_serialize_hooks:import_hook(?MODULE, fun ?MODULE:unserialise/1),
  ok.

serialize({device, Device}) ->
  case cl_device:module(Device) of
    ?MODULE -> { ok, maps:merge(cl_device:info(Device), #{ is => cl_device:id(Device), module => ?DEVICE_STAMP }) };
    _ -> undefined
  end.

unserialise({device, Info = #{ module := ?DEVICE_STAMP}}) ->
  { ok, cl_device:new_internal(maps:get(id, Info), ?MODULE, Info) };
unserialise(_) -> undefined.


