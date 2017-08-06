-record(mtp3, {
    opc = 0        :: 0..16383, %% ITU OPC/DPC 14 bits
    dpc = 0        :: 0..16383,
    ni = 0         :: 0..3,
    si = 0         :: 0..10,
    sls = 0        :: 0..15,
    priority = 0   :: 0..3,
    payload = <<>> :: binary() 
}).
