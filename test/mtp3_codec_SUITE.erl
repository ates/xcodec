-module(mtp3_codec_SUITE).

%% CT callbacks
-export([all/0, groups/0]).

%% tests
-export([decode_1/1]).
-export([encode_1/1]).

-include("mtp3.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PCAPFILE(File), filename:absname(File, ?config(data_dir, Config))).

all() ->
    [{group, decode}, {group, encode}].

groups() ->
    DecodeTests = [
        decode_1
    ],
    EncodeTests = [
        encode_1
    ],
    [
        {decode, [parallel], DecodeTests},
        {encode, [parallel], EncodeTests}
    ].

%% Tests
decode_1(Config) ->
    MTP3 = #mtp3{
        opc = 14424,
        dpc = 13541,
        sls = 15,
        ni  = 2,
        si  = 4,
        payload = <<3,0,1,2,66,0,4,3,11,16,38,72,19,0,4>>
    },
    Data = pcap_reader:payload(?PCAPFILE("mtp3_1.pcap"), 82, 20),
    MTP3 = mtp3_codec:decode(Data).

encode_1(Config) ->
    MTP3 = #mtp3{
        opc = 14424,
        dpc = 13541,
        sls = 15,
        ni  = 2,
        si  = 4,
        payload = <<3,0,1,2,66,0,4,3,11,16,38,72,19,0,4>>
    },
    Data = pcap_reader:payload(?PCAPFILE("mtp3_1.pcap"), 82, 20),
    Data = mtp3_codec:encode(MTP3).
