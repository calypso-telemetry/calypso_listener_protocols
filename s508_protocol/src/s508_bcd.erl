-module(s508_bcd).
-export([
  encode/2, decode/2, decode/1
]).

-type bcd() :: binary().

-spec encode({unsigned|signed, integer()}, pos_integer()) -> bcd().

encode({unsigned, N}, Size) when N >= 0 ->
    List = integer_to_list(N),
    Digits = [ 0 || _ <- lists:seq(1, max(0, ( Size + Size rem 2 ) - length(List))) ] ++ [ Item - 48 || Item <- List ],
    << <<X:4>> || X <- Digits >>;

encode({signed, N}, Size) when N >= 0 ->
  encode({unsigned, N}, Size);

encode({signed, N}, Size) when N < 0 ->
  negotiate(encode({unsigned, -N}, Size)).

negotiate(<<D:8/unsigned-integer, Rest/binary>>) ->
   <<(D bxor 128):8/unsigned-integer, Rest/binary>>.

is_negotiate(<<D:8/unsigned-integer, _/binary>>) ->
  D band 128 > 0.

decode({ _,Binary} = Value) ->
  decode(Value, size(Binary)).

decode({unsigned, BCD}, Size) ->
    decode_unsigned(BCD, min(Size,size(BCD)), <<>>);

decode({signed, BCD}, Size) ->
  case is_negotiate(BCD) of
    false ->
      decode({unsigned, BCD}, Size);
    true ->
      -decode({unsigned, negotiate(BCD)}, Size)
  end.

decode_unsigned(_, 0, Text) ->
    binary_to_integer(Text);
decode_unsigned(<<N1:4, N2:4, Num/binary>>, Size, Text) ->
    decode_unsigned(Num, Size-1, <<Text/binary, (N1+16#30), (N2+16#30)>>).
