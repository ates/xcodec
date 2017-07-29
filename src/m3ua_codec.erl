-module(m3ua_codec).

-export([decode/1]).
-export([encode/1]).
-export([format_error/1]).

-include("mtp3.hrl").
-include("m3ua.hrl").

-type message_class() :: mgmt | xfr | ssnm | aspsm | asptm | rkm.

-type message_type() ::
    error     |
    notify    |
    data      |
    duna      |
    dava      |
    daud      |
    scon      |
    dupu      |
    drst      |
    aspup     |
    aspdn     |
    beat      |
    aspup_ack |
    aspdn_ack |
    beat_ack  |
    aspac     |
    aspia     |
    aspac_ack |
    aspia_ack |
    regreq    |
    regrsp    |
    dereg_req |
    dereg_rsp.

-export_type([message_class/0]).
-export_type([message_type/0]).

-spec decode(Data :: binary()) -> #m3ua{}.
decode(Data) ->
    <<_Version:8, _Reserved:8, Class:8, Type:8, _Length:32, Payload/binary>> = Data,
    #m3ua{
        class      = decode_class(Class),
        type       = decode_type(Class, Type),
        parameters = decode_parameters(Payload, #{})
    }.

-spec encode(M3UA :: #m3ua{}) -> binary().
encode(#m3ua{class = Class, type = Type, parameters = Parameters}) ->
    Payload = maps:fold(fun encode_parameters/3, <<>>, Parameters),
    Length = byte_size(Payload) + 8,
    <<1:8, 0:8, (encode_class(Class)):8, (encode_type(Class, Type)):8, Length:32, Payload/binary>>.

%% Section 3.8.1
-spec format_error(pos_integer()) -> string().
format_error(1)  -> "Invalid Version";
format_error(3)  -> "Unsupported Message Class";
format_error(4)  -> "Unsupported Message Type";
format_error(5)  -> "Unsupported Traffic Mode Type";
format_error(6)  -> "Unexpected Message";
format_error(7)  -> "Protocol Error";
format_error(9)  -> "Invalid Stream Identifier";
format_error(13) -> "Refused - Management Blocking";
format_error(14) -> "ASP Identifier Required";
format_error(15) -> "Invalid ASP Identifier";
format_error(17) -> "Invalid Parameter Value";
format_error(18) -> "Parameter Field Error";
format_error(19) -> "Unexpected Parameter";
format_error(20) -> "Destination Status Unknown";
format_error(21) -> "Invalid Network Appearance";
format_error(22) -> "Missing Parameter";
format_error(25) -> "Invalid Routing Context";
format_error(26) -> "No Configured AS for ASP".

%% RFC 4666, Section 3.1.2
%% 0  Management (MGMT) Messages
%% 1  Transfer Messages
%% 2  SS7 Signalling Network Management (SSNM) Messages
%% 3  ASP State Maintenance (ASPSM) Messages
%% 4  ASP Traffic Maintenance (ASPTM) Message
-spec decode_class(0..4 | 9) -> message_class().
decode_class(0) -> mgmt;
decode_class(1) -> xfr;
decode_class(2) -> ssnm;
decode_class(3) -> aspsm;
decode_class(4) -> asptm;
decode_class(9) -> rkm.

-spec decode_type(Class :: 0..4 | 9, Type :: 0..6) -> message_type().
decode_type(Class, Type) ->
    case {Class, Type} of
        %% Management (MGMT) Messages
        {0, 0} -> error;
        {0, 1} -> notify;
        %% Transfer (XFR) Messages
        {1, 1} -> data;
        %% SS7 Signalling Network Management (SSNM) Messages
        {2, 1} -> duna;      %% Destination Unavailable (DUNA)
        {2, 2} -> dava;      %% Destination Available (DAVA)
        {2, 3} -> daud;      %% estination State Audit (DAUD)
        {2, 4} -> scon;      %% Signalling Congestion (SCON)
        {2, 5} -> dupu;      %% Destination User Part Unavailable (DUPU)
        {2, 6} -> drst;      %% Destination Restricted (DRST)
        %% ASP State Maintenance (ASPSM) Messages
        {3, 1} -> aspup;     %% ASP Up (ASPUP)
        {3, 2} -> aspdn;     %% ASP Down (ASPDN)
        {3, 3} -> beat;      %% Heartbeat (BEAT)
        {3, 4} -> aspup_ack; %% ASP Up Acknowledgement (ASPUP ACK)
        {3, 5} -> aspdn_ack; %% ASP Down Acknowledgement (ASPDN ACK)
        {3, 6} -> beat_ack;  %% Heartbeat Acknowledgement (BEAT ACK)
        %% ASP Traffic Maintenance (ASPTM) Messages
        {4, 1} -> aspac;     %% ASP Active (ASPAC)
        {4, 2} -> aspia;     %% ASP Inactive (ASPIA)
        {4, 3} -> aspac_ack; %% ASP Active Acknowledgement (ASPAC ACK)
        {4, 4} -> aspia_ack; %% ASP Inactive Acknowledgement (ASPIA ACK)
        %% Routing Key Management (RKM) Messages
        {9, 1} -> regreq;    %% Registration Request (REG REQ)
        {9, 2} -> regrsp;    %% Registration Response (REG RSP)
        {9, 3} -> dereg_req; %% Deregistration Request (DEREG REQ)
        {9, 4} -> dereg_rsp  %% Deregistration Response (DEREG RSP)
    end.

pad_bytes(Length) when Length rem 4 =:= 0 -> 0;
pad_bytes(Length) when Length rem 4 =/= 0 -> 4 - Length rem 4.

decode_parameters(<<Tag:16, Len:16, Payload/binary>>, Parameters) ->
    Length = Len - 4,
    PadLen = pad_bytes(Len),
    <<Option:Length/binary, 0:PadLen/integer-unit:8, Rest/binary>> = Payload,
    decode_parameters(Rest, decode_parameter(Tag, Option, Parameters));
decode_parameters(<<>>, Parameters) -> Parameters.

decode_parameter(?M3UA_PARAM_ROUTING_CTXT, <<Value:32>>, Parameters) ->
    Parameters#{routing_ctxt => Value};

decode_parameter(?M3UA_PARAM_DIAGNOSTIC_MSG, Value, Parameters) ->
    Parameters#{diagnostic_msg => Value};

decode_parameter(?M3UA_PARAM_HEARTBEAT_DATA, Value, Parameters) ->
    Parameters#{heartbeat_data => Value};

decode_parameter(?M3UA_PARAM_TMT, <<Value:32>>, Parameters) ->
    Parameters#{
        tmt =>
            case Value of
                1 -> override;
                2 -> loadshare;
                3 -> broadcast
            end
    };

decode_parameter(?M3UA_PARAM_ERROR_CODE, <<Value:32>>, Parameters) ->
    Parameters#{error_code => Value};

decode_parameter(?M3UA_PARAM_STATUS, <<Type:16, Information:16>>, Parameters) ->
    Parameters#{status => #{type => Type, information => Information}};

decode_parameter(?M3UA_PARAM_ASP_ID, <<Value:32>>, Parameters) ->
    Parameters#{asp_identifier => Value};

decode_parameter(?M3UA_PARAM_APC, Value, Parameters) ->
    Parameters#{affected_poind_code => decode_pcs(Value)};

decode_parameter(?M3UA_PARAM_CORRELATION_ID, <<Value:32>>, Parameters) ->
    Parameters#{correlation_id => Value};

decode_parameter(?M3UA_PARAM_NETWORK_APP, <<Value:32>>, Parameters) ->
    Parameters#{network_appearance => Value};

decode_parameter(?M3UA_PARAM_ROUTING_KEY, Value, Parameters) ->
    Parameters#{routing_key => decode_parameters(Value, #{})};

decode_parameter(?M3UA_PARAM_LOCAL_RK_ID, <<Value:32>>, Parameters) ->
    Parameters#{local_rk_identifier => Value};

decode_parameter(?M3UA_PARAM_DPC, Value, Parameters) ->
    Parameters#{dpc => decode_pcs(Value)};

decode_parameter(?M3UA_PARAM_OPC_LIST, Value, Parameters) ->
    Parameters#{opc_list => decode_pcs(Value)};

decode_parameter(?M3UA_PARAM_SI, Value, Parameters) ->
    Parameters#{si => [SI || <<SI:8>> <= Value]};

decode_parameter(?M3UA_PARAM_REGISTRATION_RESULT, Value, Parameters) ->
    Parameters#{registration_result => decode_parameters(Value, #{})};

decode_parameter(?M3UA_PARAM_REGISTRATION_STATUS, <<Value:32>>, Parameters) ->
    Parameters#{registration_status => Value};

decode_parameter(?M3UA_PARAM_PROTOCOL_DATA, Value, Parameters) ->
    <<OPC:32, DPC:32, SI:8, NI:8, Priority:8, SLS:8, Payload/binary>> = Value,
    MTP3 = #mtp3{
        opc 	 = OPC,
        dpc 	 = DPC,
        sls 	 = SLS,
        ni  	 = NI,
        si  	 = SI,
        priority = Priority,
        payload  = Payload
    },
    Parameters#{protocol_data => MTP3};

decode_parameter(Tag, Value, Parameters) ->
    Parameters#{Tag => Value}.

decode_pcs(Data) ->
    [{Mask, PC} || <<Mask:8, PC:24>> <= Data].

encode_class(mgmt)  -> 0;
encode_class(xfr)   -> 1;
encode_class(ssnm)  -> 2;
encode_class(aspsm) -> 3;
encode_class(asptm) -> 4;
encode_class(rkm)   -> 9.

encode_type(Class, Type) ->
    case {Class, Type} of
        {mgmt, error}      -> 0;
        {mgmt, notify}     -> 1;
        {xfr, data}        -> 1;
        {ssnm, duna}       -> 1;
        {ssnm, dava}       -> 2;
        {ssnm, daud}       -> 3;
        {ssnm, scon}       -> 4;
        {ssnm, dupu}       -> 5;
        {ssnm, drst}       -> 6;
        {aspsm, aspup}     -> 1;
        {aspsm, aspdn}     -> 2;
        {aspsm, beat}      -> 3;
        {aspsm, aspup_ack} -> 4;
        {aspsm, aspdn_ack} -> 5;
        {aspsm, beat_ack}  -> 6;
        {asptm, aspac}     -> 1;
        {asptm, aspia}     -> 2;
        {asptm, aspac_ack} -> 3;
        {asptm, aspia_ack} -> 4;
        {rkm, regreq}      -> 1;
        {rkm, regrsp}      -> 2;
        {rkm, dereg_req}   -> 3;
        {rkm, dereg_rsp}   -> 4
    end.

encode_parameters(Key, Value, Acc) ->
    {Tag, Data} =
        case Key of
            routing_ctxt ->
                {?M3UA_PARAM_ROUTING_CTXT, <<Value:32>>};
            diagnostic_msg ->
                {?M3UA_PARAM_DIAGNOSTIC_MSG, Value};
            heartbeat_data ->
                {?M3UA_PARAM_HEARTBEAT_DATA, Value};
            tmt ->
                TMT =
                    case Value of
                        override  -> 1;
                        loadshare -> 2;
                        broadcast -> 3
                    end,
                {?M3UA_PARAM_TMT, <<TMT:32>>};
            error_code ->
                {?M3UA_PARAM_ERROR_CODE, <<Value:32>>};
            status ->
                #{type := Type, information := Information} = Value,
                {?M3UA_PARAM_STATUS, <<Type:16, Information:16>>};
            asp_identifier ->
                {?M3UA_PARAM_ASP_ID, <<Value:32>>};
            affected_poind_code ->
                {?M3UA_PARAM_APC, << <<Mask:8, PC:24>> || {Mask, PC} <- Value >>};
            correlation_id ->
                {?M3UA_PARAM_CORRELATION_ID, <<Value:32>>};
            network_appearance ->
                {?M3UA_PARAM_NETWORK_APP, <<Value:32>>};
            routing_key ->
                {?M3UA_PARAM_ROUTING_KEY, maps:fold(fun encode_parameters/3, <<>>, Value)};
            local_rk_identifier ->
                {?M3UA_PARAM_LOCAL_RK_ID, <<Value:32>>};
            dpc ->
                {?M3UA_PARAM_DPC, << <<Mask:8, PC:24>> || {Mask, PC} <- Value >>};
            opc_list ->
                {?M3UA_PARAM_OPC_LIST, << <<Mask:8, PC:24>> || {Mask, PC} <- Value >>};
            si ->
                {?M3UA_PARAM_SI, << <<SI:8>> || SI <- Value >>};
            registration_result ->
                {?M3UA_PARAM_REGISTRATION_RESULT, maps:fold(fun encode_parameters/3, <<>>, Value)};
            registration_status ->
                {?M3UA_PARAM_REGISTRATION_STATUS, <<Value:32>>};
            protocol_data ->
                #mtp3{
                    ni       = NI,
                    si       = SI,
                    opc      = OPC,
                    dpc      = DPC,
                    sls      = SLS,
                    priority = Priority,
                    payload  = Payload
                } = Value,
                {?M3UA_PARAM_PROTOCOL_DATA, <<OPC:32, DPC:32, SI:8, NI:8, Priority:8, SLS:8, Payload/binary>>}
        end,
    DataLength = byte_size(Data),
    ParameterLength = 4 + DataLength,
    PadLen = pad_bytes(ParameterLength) * 8,
    <<Acc/binary, Tag:16, ParameterLength:16, Data:DataLength/bytes, 0:PadLen>>.
