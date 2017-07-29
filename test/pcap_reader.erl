-module(pcap_reader).

%% API
-export([payload/3]).

-include_lib("pcapfile/include/pcapfile.hrl").

payload(File, Offset, Size) ->
    {ok, #pcap{records = Recs}} = pcapfile:read_file(File),
    #pcap_record{payload = <<_Header:Offset/binary-unit:8, Data:Size/binary-unit:8, _Rest/binary>> = Payload} = hd(Recs),
    ct:log("File: ~s, Offset: ~p, Size: ~p, PCAP payload: ~p", [File, Offset, Size, Payload]),
    Data.
