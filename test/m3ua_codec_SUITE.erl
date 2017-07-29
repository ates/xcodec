-module(m3ua_codec_SUITE).

%% CT callbacks
-export([all/0, groups/0]).

%% tests
-export([test_1/1]).
-export([test_2/1]).
-export([test_3/1]).
-export([test_4/1]).
-export([test_5/1]).

-include("mtp3.hrl").
-include("m3ua.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PCAPFILE(File), filename:absname(File, ?config(data_dir, Config))).

all() ->
    [{group, decode_encode}].

groups() ->
    Tests = [
        test_1,
        test_2,
        test_3,
        test_4,
        test_5
    ],
    [
        {decode_encode, [parallel], Tests}
    ].

%% Tests

%% M3UA_PARAM_STATUS, M3UA_PARAM_ROUTING_CTXT, M3UA_PARAM_ASP_ID
test_1(Config) ->
    M3UA = #m3ua{
        class = mgmt,
        type = notify,
        parameters = #{
            routing_ctxt => 1,
            status => #{information => 2, type => 1},
            asp_identifier => 77
        }
    },
    Data = pcap_reader:payload(?PCAPFILE("decode_1.pcap"), 62, 32),
    ct:log("Data: ~p, M3UA: ~p", [Data, M3UA]),
    M3UA = m3ua_codec:decode(Data),
    M3UA = m3ua_codec:decode(m3ua_codec:encode(M3UA)).

%% M3UA_PARAM_ERROR_CODE, M3UA_PARAM_DIAGNOSTIC_MSG
test_2(Config) ->
    M3UA = #m3ua{
        class = mgmt,
        type = error,
        parameters = #{
            diagnostic_msg => <<1,0,1,1,0,0,0,48,2,16,0,37,0,0,11,185,0,0,19,137,3,2,0,0,9,0,3,7,11,4,67,137,19,1,4,67,185,11,1,5>>,
            error_code => 9
        }
    },
    Data = pcap_reader:payload(?PCAPFILE("decode_2.pcap"), 64, 60),
    ct:log("Data: ~p, M3UA: ~p", [Data, M3UA]),
    M3UA = m3ua_codec:decode(Data),
    M3UA = m3ua_codec:decode(m3ua_codec:encode(M3UA)).

%% M3UA_PARAM_ROUTING_KEY, M3UA_PARAM_DPC, M3UA_PARAM_LOCAL_RK_ID, M3UA_PARAM_NETWORK_APP, M3UA_PARAM_OPC_LIST, M3UA_PARAM_SI, M3UA_PARAM_TMT
test_3(Config) ->
    M3UA = #m3ua{
        class = rkm,
        type = regreq,
        parameters = #{
            routing_key => #{
                dpc => [{255,111}],
                local_rk_identifier => 1,
                network_appearance => 5,
                opc_list => [{255,222}],
                si => [3,5],
                tmt => override
            }
        }
    },
    Data = pcap_reader:payload(?PCAPFILE("decode_3.pcap"), 62, 60),
    ct:log("Data: ~p, M3UA: ~p", [Data, M3UA]),
    M3UA = m3ua_codec:decode(Data),
    M3UA = m3ua_codec:decode(m3ua_codec:encode(M3UA)).

%% M3UA_PARAM_REGISTRATION_RESULT, M3UA_PARAM_REGISTRATION_STATUS
test_4(Config) ->
    M3UA = #m3ua{
        class = rkm,
        type = regrsp,
        parameters = #{
            registration_result => #{
                local_rk_identifier => 1,
                registration_status => 0,
                routing_ctxt => 1
            }
        }
    },
    Data = pcap_reader:payload(?PCAPFILE("decode_4.pcap"), 62, 36),
    ct:log("Data: ~p, M3UA: ~p", [Data, M3UA]),
    M3UA = m3ua_codec:decode(Data),
    M3UA = m3ua_codec:decode(m3ua_codec:encode(M3UA)).

%% M3UA_PARAM_PROTOCOL_DATA
test_5(Config) ->
    M3UA = #m3ua{
        class = xfr,
        type = data,
        parameters = #{
            protocol_data => #mtp3{
                opc = 3001,
                dpc = 5001,
                si  = 3,
                ni  = 2,
                priority = 0,
                payload = <<9,0,3,7,11,4,67,137,19,1,4,67,185,11,1,5,1,7,185,11,0>>
           }
       }
    },
    Data = pcap_reader:payload(?PCAPFILE("decode_5.pcap"), 80, 48),
    ct:log("Data: ~p, M3UA: ~p", [Data, M3UA]),
    M3UA = m3ua_codec:decode(Data),
    M3UA = m3ua_codec:decode(m3ua_codec:encode(M3UA)).
