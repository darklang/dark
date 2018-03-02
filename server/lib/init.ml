open Core

let has_inited : bool ref =
  ref false

let init () =
  if !has_inited
  then ()
  else
    let _ =
      match Sys.getenv "DARK_LOGLEVEL" with
      | None -> ()
      | Some s when String.Caseless.equal s "OFF" ->
        Log.level := `Off
      | Some s when String.Caseless.equal s "FATAL" ->
        Log.level := `Fatal
      | Some s when String.Caseless.equal s "ERROR" ->
        Log.level := `Error
      | Some s when String.Caseless.equal s "WARN" ->
        Log.level := `Warn
      | Some s when String.Caseless.equal s "INFO" ->
        Log.level := `Info
      | Some s when String.Caseless.equal s "DEBUG" ->
        Log.level := `Debug
      | Some s when String.Caseless.equal s "ALL" ->
        Log.level := `All
      | Some _ -> ()
    in
    has_inited := true
