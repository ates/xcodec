-module(prop_tbcd_codec).

-export([prop_decode_encode/0]).

-include_lib("proper/include/proper.hrl").

prop_decode_encode() ->
    ?FORALL(Data, list(range(1, 14)),
        begin
            EncDec = tbcd_codec:decode(tbcd_codec:encode(list_to_binary(Data))),
            EncDec =:= list_to_binary(Data)
        end
    ).
