-module(traccar_device).
-author("begemot").


-behaviour(cl_device).
-export([
  new/2, handle_update_info/2
]).

new(Id, Data) ->
  Imei = maps:get(imei, Data),
  true = is_integer(Imei),
  Default = #{
  },
  cl_device:new_internal(Id, ?MODULE, #{
    imei => Imei,
    login => integer_to_binary(Imei)
  }, Default).

handle_update_info(_Info, _Device) -> undefined.
