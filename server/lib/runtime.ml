open Core
open Types
open Types.RuntimeT

let error
    ?(bt:Caml.Printexc.raw_backtrace option=None)
    ?(actual=DIncomplete) ?(result=DIncomplete)
    ?(info=[])
    ?(expected="")
    ?(workarounds=[])
    ?(long="")
    (short: string) =
  Exception.raise_
    "Runtime"
    ~bt
    ~actual:(actual |> Dval.dval_to_yojson |> Yojson.Safe.pretty_to_string)
    ~result:(result |> Dval.dval_to_yojson |> Yojson.Safe.pretty_to_string)
    ~actual_tipe:(actual |> Dval.tipename)
    ~result_tipe:(result |> Dval.tipename)
    ~info
    ~expected
    ~workarounds
    ~long
    short






