module LibSerialization.Binary.Serializers.PT.Toplevel

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common
open Serializers.PT.Common

module Handler =
  module CronInterval =
    let write (w : BinaryWriter) (i : Handler.CronInterval) : unit =
      match i with
      | Handler.CronInterval.EveryDay -> w.Write 0uy
      | Handler.CronInterval.EveryWeek -> w.Write 1uy
      | Handler.CronInterval.EveryFortnight -> w.Write 2uy
      | Handler.CronInterval.EveryHour -> w.Write 3uy
      | Handler.CronInterval.Every12Hours -> w.Write 4uy
      | Handler.CronInterval.EveryMinute -> w.Write 5uy

    let read (r : BinaryReader) : Handler.CronInterval =
      match r.ReadByte() with
      | 0uy -> Handler.CronInterval.EveryDay
      | 1uy -> Handler.CronInterval.EveryWeek
      | 2uy -> Handler.CronInterval.EveryFortnight
      | 3uy -> Handler.CronInterval.EveryHour
      | 4uy -> Handler.CronInterval.Every12Hours
      | 5uy -> Handler.CronInterval.EveryMinute
      | b ->
        raise (BinaryFormatException(CorruptedData $"Invalid CronInterval tag: {b}"))


  module Spec =
    let write (w : BinaryWriter) (s : Handler.Spec) : unit =
      match s with
      | Handler.Spec.Worker name ->
        w.Write 0uy
        String.write w name
      | Handler.Spec.Cron(name, interval) ->
        w.Write 1uy
        String.write w name
        CronInterval.write w interval
      | Handler.Spec.REPL name ->
        w.Write 2uy
        String.write w name
      | Handler.Spec.HTTP(route, method) ->
        w.Write 3uy
        String.write w route
        String.write w method

    let read (r : BinaryReader) : Handler.Spec =
      match r.ReadByte() with
      | 0uy -> Handler.Worker(String.read r)
      | 1uy ->
        let name = String.read r
        let interval = CronInterval.read r
        Handler.Cron(name, interval)
      | 2uy -> Handler.REPL(String.read r)
      | 3uy ->
        let route = String.read r
        let method = String.read r
        Handler.HTTP(route, method)
      | b ->
        raise (BinaryFormatException(CorruptedData $"Invalid Handler.Spec tag: {b}"))


  let write (w : BinaryWriter) (h : Handler.T) : unit =
    w.Write h.tlid
    Expr.Expr.write w h.ast
    (Spec.write) w h.spec

  let read (r : BinaryReader) : Handler.T =
    let tlid = r.ReadUInt64()
    let ast = Expr.Expr.read r
    let spec = (Spec.read) r
    { tlid = tlid; ast = ast; spec = spec }


module DB =
  let write (w : BinaryWriter) (db : DB.T) : unit =
    w.Write db.tlid
    String.write w db.name
    w.Write db.version
    TypeReference.write w db.typ

  let read (r : BinaryReader) : DB.T =
    let tlid = r.ReadUInt64()
    let name = String.read r
    let version = r.ReadInt32()
    let typ = TypeReference.read r
    { tlid = tlid; name = name; version = version; typ = typ }


let write (w : BinaryWriter) (t : Toplevel.T) : unit =
  match t with
  | Toplevel.TLDB db ->
    w.Write 0uy
    DB.write w db
  | Toplevel.TLHandler handler ->
    w.Write 1uy
    Handler.write w handler

let read (r : BinaryReader) : Toplevel.T =
  match r.ReadByte() with
  | 0uy -> Toplevel.TLDB(DB.read r)
  | 1uy -> Toplevel.TLHandler(Handler.read r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid Toplevel tag: {b}"))
