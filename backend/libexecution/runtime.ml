open Core_kernel

let error
    ?(bt : Caml.Printexc.raw_backtrace option)
    ?(actual = Types.RuntimeT.DIncomplete Types.RuntimeT.SourceNone)
    ?(result = Types.RuntimeT.DIncomplete Types.RuntimeT.SourceNone)
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
    ~actual_tipe:(Dval.pretty_tipename actual)
    ~result_tipe:(Dval.pretty_tipename result)
    ~info
    ~expected
    ~workarounds
    ~long
    short
