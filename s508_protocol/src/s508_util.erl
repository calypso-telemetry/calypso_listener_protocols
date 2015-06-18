-module(s508_util).
-author("begemot").

%% API
-export([
  pseudo_ip/1,
  bin_to_hex/1,
  t/0
]).

pseudo_ip(SIM) when is_integer(SIM) ->
  Bin = integer_to_binary(SIM),
  << K0:8/unsigned-integer, A:2/binary, B:2/binary, C:2/binary, D:2/binary>> = binary:part(Bin,{byte_size(Bin), -9}),
  << _:4/bits, B1:1/bits, B2:1/bits, B3:1/bits, B4:1/bits >> = integer_to_binary(K0 - 48),
  Bin0 = <<
    case B1 of
      <<0:1>> -> binary_to_integer(A);
      <<1:1>> -> binary_to_integer(A) bor 128
    end,
    case B2 of
      <<0:1>> -> binary_to_integer(B);
      <<1:1>> -> binary_to_integer(B) bor 128
    end,
    case B3 of
      <<0:1>> -> binary_to_integer(C);
      <<1:1>> -> binary_to_integer(C) bor 128
    end,
    case B4 of
      <<0:1>> -> binary_to_integer(D);
      <<1:1>> -> binary_to_integer(D) bor 128
    end
  >>,
  bin_to_hex(Bin0).

bin_to_hex(Bin) ->
  bin_to_hex(Bin, <<>>).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(<<A:4/unsigned-integer, B:4/unsigned-integer, Rest/binary>>, Acc) ->
  Ab = if
    A < 10 -> A + $0;
    true -> A - 10 + $a
  end,
  Bb  = if
    B < 10 -> B + $0;
    true -> B - 10 + $a
  end,
  bin_to_hex(Rest, << Acc/binary, (Ab):8/unsigned-integer, (Bb):8/unsigned-integer>>).

t() ->
   <<"55e341bb">> = pseudo_ip(13585996559).
