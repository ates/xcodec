-module(prop_mtp3_codec).

-export([prop_decode_encode/0]).

-include("mtp3.hrl").
-include_lib("proper/include/proper.hrl").

-type mtp3() :: #mtp3{}.

-export_type([mtp3/0]).

prop_decode_encode() ->
    ?FORALL(Data, mtp3(), Data =:= mtp3_codec:decode(mtp3_codec:encode(Data))).
