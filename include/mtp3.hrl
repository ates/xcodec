-record(mtp3, {
    opc = 0        :: non_neg_integer(),
    dpc = 0        :: non_neg_integer(),
    ni = 0         :: byte(),
    si = 0         :: 0..10,
    sls = 0        :: 0..15,
    priority = 0   :: byte(),
    payload = <<>> :: binary() 
}).
