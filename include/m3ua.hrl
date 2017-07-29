-record(m3ua, {
    class            :: m3ua_codec:message_class(),
    type             :: m3ua_codec:message_type(),
    parameters = #{} :: map()
}).

%% Common parameters
-define(M3UA_PARAM_INFO_STRING,    4).
-define(M3UA_PARAM_ROUTING_CTXT,   6).
-define(M3UA_PARAM_DIAGNOSTIC_MSG, 7).
-define(M3UA_PARAM_HEARTBEAT_DATA, 9).
-define(M3UA_PARAM_TMT,            11). %% Traffic Mode Type
-define(M3UA_PARAM_ERROR_CODE,     12).
-define(M3UA_PARAM_STATUS,         13).
-define(M3UA_PARAM_ASP_ID,         17). %% ASP Identifier
-define(M3UA_PARAM_APC,            18). %% Affected Point Code
-define(M3UA_PARAM_CORRELATION_ID, 19).

%% M3UA-Specific parameters
-define(M3UA_PARAM_NETWORK_APP,           512). %% Network Appearance
-define(M3UA_PARAM_USER_CAUSE,            516). %% User/Cause
-define(M3UA_PARAM_CONGESTION_IND,        517). %% Congestion Indications
-define(M3UA_PARAM_CONCERNED_DST,         518). %% Concerned Destination
-define(M3UA_PARAM_ROUTING_KEY,           519).
-define(M3UA_PARAM_REGISTRATION_RESULT,   520).
-define(M3UA_PARAM_DEREGISTRATION_RESULT, 521).
-define(M3UA_PARAM_LOCAL_RK_ID,           522). %% Local Routing Key Identifier
-define(M3UA_PARAM_DPC,                   523). %% Destination Point Code
-define(M3UA_PARAM_SI,                    524). %% Service Indicators
-define(M3UA_PARAM_OPC_LIST,              526). %% Originating Point Code List
-define(M3UA_PARAM_PROTOCOL_DATA,         528).
-define(M3UA_PARAM_REGISTRATION_STATUS,   530).
-define(M3UA_PARAM_DEREGISTRATION_STATUS, 531).
