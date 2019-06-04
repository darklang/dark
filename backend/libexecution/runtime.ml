open Core_kernel
open Types
open Types.RuntimeT

let error
    ?(bt : Caml.Printexc.raw_backtrace option)
    ?(actual = DIncomplete)
    ?(result = DIncomplete)
    ?(info = [])
    ?(expected = "")
    ?(workarounds = [])
    ?(long = "")
    (short : string) =
  Exception.raise_
    Code
    ?bt
    ~actual:(Dval.to_developer_repr_v0 actual)
    ~result:(Dval.to_developer_repr_v0 result)
    ~actual_tipe:(Dval.tipename actual)
    ~result_tipe:(Dval.tipename result)
    ~info
    ~expected
    ~workarounds
    ~long
    short
