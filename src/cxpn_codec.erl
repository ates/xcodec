-module(cxpn_codec).

-export([decode_cgpn/1]).
-export([encode_cgpn/1]).
-export([decode_cdpn/1]).
-export([encode_cdpn/1]).
-export([decode_cgpn_bcd/1]).
-export([encode_cgpn_bcd/1]).
-export([decode_cdpn_bcd/1]).
-export([encode_cdpn_bcd/1]).

-type cgpn() ::
    #{
        oe     := 0 | 1,   %% Odd/Even indicator
        nai    := 0..127,  %% Nature of address indicator
        ni     := 0 | 1,   %% Number Incomplete indicator
        npi    := 0..7,    %% Numbering plan indicator
        ap     := 0..3,    %% Address presentation restricted indicator
        si     := 0..3,    %% Screening indicator
        number := binary()
    }.

-type cdpn() ::
    #{
        oe     := 0 | 1,   %% Odd | Even
        nai    := 0..127,  %% Nature of address indicator
        inn    := 0 | 1,   %% Internal Network Number indicator
        npi    := 0..7,    %% Numbering plan indicator
        number := binary()
    }.

-type cgpn_bcd() ::
    #{
        ton    := 0..7,    %% Type of number
        npi    := 0..7,    %% Numbering plan indicator
        pi     := 0..3,    %% Presentation indicator
        si     := 0..3,    %% Screening indicator
        number := binary()
    }.

-type cdpn_bcd() ::
    #{
        ton    := 0..7,    %% Type of number
        npi    := 0..7,    %% Numbering plan indicator
        number := binary()
    }.

-export_type([cgpn/0]).
-export_type([cdpn/0]).
-export_type([cgpn_bcd/0]).
-export_type([cdpn_bcd/0]).

%% Q.763, section 3.10
-spec decode_cgpn(Data :: binary()) -> cgpn().
decode_cgpn(Data) ->
    <<OE:1, NAI:7, NI:1, NPI:3, AP:2, SI:2, TBCD/binary>> = Data,
    #{
        oe     => OE,
        nai    => NAI,
        ni     => NI,
        npi    => NPI,
        ap     => AP,
        si     => SI,
        number => df(tbcd_codec:decode(TBCD), OE)
    }.

-spec encode_cgpn(CgPN :: cgpn()) -> binary().
encode_cgpn(CgPN) ->
    #{
        oe     := OE,
        nai    := NAI,
        ni     := NI,
        npi    := NPI,
        ap     := AP,
        si     := SI,
        number := Number
    } = CgPN,
    <<OE:1, NAI:7, NI:1, NPI:3, AP:2, SI:2, (tbcd_codec:encode(af(Number, OE)))/binary>>.

%% Q.763, section 3.9
-spec decode_cdpn(Data :: binary()) -> cdpn().
decode_cdpn(Data) ->
    <<OE:1, NAI:7, INN:1, NPI:3, _Spare:4, TBCD/binary>> = Data,
    #{
        oe     => OE,
        nai    => NAI,
        inn    => INN,
        npi    => NPI,
        number => df(tbcd_codec:decode(TBCD), OE)
    }.

-spec encode_cdpn(CdPN :: cdpn()) -> binary().
encode_cdpn(CdPN) ->
    #{
        oe     := OE,
        nai    := NAI,
        inn    := INN,
        npi    := NPI,
        number := Number
    } = CdPN,
    <<OE:1, NAI:7, INN:1, NPI:3, 0:4, (tbcd_codec:encode(af(Number, OE)))/binary>>.

%% 3GPP TS 24.008 10.5.4.9
-spec decode_cgpn_bcd(Data :: binary()) -> cgpn_bcd().
decode_cgpn_bcd(Data) ->
    <<_Ext:1, TON:3, NPI:4, 1:1, PI:2, _Spare:3, SI:2, TBCD/binary>> = Data,
    #{
        ton    => TON,
        npi    => NPI,
        pi     => PI,
        si     => SI,
        number => tbcd_codec:decode(TBCD)
    }.

-spec encode_cgpn_bcd(CgPN :: cgpn_bcd()) -> binary().
encode_cgpn_bcd(CgPN) ->
    #{
        ton    := TON,
        npi    := NPI,
        pi     := PI,
        si     := SI,
        number := Number
    } = CgPN,
    <<0:1, TON:3, NPI:4, 1:1, PI:2, 0:3, SI:2, (tbcd_codec:encode(Number))/binary>>.

%% 3GPP TS 24.008 10.5.4.7
-spec decode_cdpn_bcd(Data :: binary()) -> cdpn_bcd().
decode_cdpn_bcd(Data) ->
    <<1:1, TON:3, NPI:4, TBCD/binary>> = Data,
    #{
        ton    => TON,
        npi    => NPI,
        number => tbcd_codec:decode(TBCD)
    }.

-spec encode_cdpn_bcd(CdPN :: cdpn_bcd()) -> binary().
encode_cdpn_bcd(CdPN) ->
    #{
        ton    := TON,
        npi    := NPI,
        number := Number
    } = CdPN,
    <<1:1, TON:3, NPI:4, (tbcd_codec:encode(Number))/binary>>.

%% Q.763, section 3.9 f
%% In case of an odd number of address signals,
%% the filler code 0000 is inserted after the last address signal.
df(Number, OE) ->
    Length = byte_size(Number) - 1,
    <<Digits:Length/binary-unit:8, Last:8>> = Number,
    case Last of
        0 when OE =:= 1 ->
            Digits;
        _ -> Number
    end.

af(Number, 1) -> <<Number/binary, 0:8>>;
af(Number, 0) -> Number.
