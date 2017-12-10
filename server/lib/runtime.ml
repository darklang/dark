open Core
open Types
open Types.RuntimeT

let error ?(actual=DIncomplete) ?(result=DIncomplete) ?(info=[]) ?(expected="") ?(workarounds=[]) ?(long="") (short: string) =
  Exception.DarkException
  { short = short
  ; long = long
  ; tipe = "Runtime"
  ; actual = actual |> Dval.dval_to_yojson |> Yojson.Safe.pretty_to_string
  ; actual_tipe = actual |> Dval.tipename
  ; result = result |> Dval.dval_to_yojson |> Yojson.Safe.pretty_to_string
  ; result_tipe = result |> Dval.tipename
  ; expected = expected
  ; info = info
  ; workarounds = workarounds
  }

let raise_error ?(actual=DIncomplete) ?(result=DIncomplete) ?(info=[]) ?(expected="") ?(workarounds=[]) ?(long="") (short: string) =
 raise (error ~actual ~result ~info ~expected ~workarounds ~long short)

exception TypeError of dval list





