open Core_kernel
open Types
open Types.RuntimeT

let error
    ?(bt:Caml.Printexc.raw_backtrace option)
    ?(actual=DIncomplete) ?(result=DIncomplete)
    ?(info=[])
    ?(expected="")
    ?(workarounds=[])
    ?(long="")
    (short: string) =
  Exception.raise_
    UserCode
    ?bt
    ~actual:(actual |> Dval.dval_to_pretty_json_string)
    ~result:(result |> Dval.dval_to_pretty_json_string)
    ~actual_tipe:(actual |> Dval.tipename)
    ~result_tipe:(result |> Dval.tipename)
    ~info
    ~expected
    ~workarounds
    ~long
    short






