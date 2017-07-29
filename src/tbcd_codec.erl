-module(tbcd_codec).

%% API
-export([decode/1]).
-export([encode/1]).

%% 3GPP TS 29.002 version 12.7.0 Release 12
%% section 17.7.8, Common data types
%%
%% TBCD-STRING ::= OCTET STRING
%%    -- This type (Telephony Binary Coded Decimal String) is used to
%%    -- represent several digits from 0 through 9, *, #, a, b, c, two
%%    -- digits per octet, each digit encoded 0000 to 1001 (0 to 9),
%%    -- 1010 (*), 1011 (#), 1100 (a), 1101 (b) or 1110 (c); 1111 used
%%    -- as filler when there is an odd number of digits.
%%    -- bits 8765 of octet n encoding digit 2n
%%    -- bits 4321 of octet n encoding digit 2(n-1) +1
%%
-spec decode(Data :: binary()) -> binary().
decode(Data) ->
    << <<First, Second>> || <<Second:4, First:4>> <= Data >>.

-spec encode(Digits :: binary()) -> binary().
encode(Digits) ->
    encode(Digits, <<>>).

encode(<<First:8, Second:8, Rest/binary>>, Acc) ->
    encode(Rest, <<Acc/binary, Second:4, First:4>>);
encode(<<Last>>, Acc) ->
    <<Acc/binary, 0:4, Last:4>>;
encode(<<>>, Acc) -> Acc.
