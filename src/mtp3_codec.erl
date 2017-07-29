-module(mtp3_codec).

-export([decode/1]).
-export([encode/1]).

-include("mtp3.hrl").

-spec decode(Data :: binary()) -> #mtp3{}.
decode(Data) ->
    <<
        NI:2,
        Priority:2,
        SI:4,
        DPClo:8,
        OPClo:2,
        DPChi:6,
        OPCmid:8,
        SLS:4,
        OPChi:4,
        Payload/binary
    >> = Data,
    <<OPC:16>> = <<0:2, OPChi:4, OPCmid:8, OPClo:2>>,
    <<DPC:16>> = <<0:2, DPChi:6, DPClo:8>>,
	#mtp3{
        opc      = OPC,
        dpc      = DPC,
        ni       = NI,
        si       = SI,
        sls      = SLS,
        priority = Priority,
        payload  = Payload
    }.

-spec encode(MTP3 :: #mtp3{}) -> binary().
encode(MTP3) ->
    #mtp3{
        opc      = OPC,
        dpc      = DPC,
        ni       = NI,
        si       = SI,
        sls      = SLS,
        priority = Priority,
        payload  = Payload
    } = MTP3,
    <<0:2, OPChi:4, OPCmid:8, OPClo:2>> = <<OPC:16>>,
    <<0:2, DPChi:6, DPClo:8>> = <<DPC:16>>,
    <<
        NI:2,
        Priority:2,
        SI:4,
        DPClo:8,
        OPClo:2,
        DPChi:6,
        OPCmid:8,
        SLS:4,
        OPChi:4,
        Payload/binary
    >>.
