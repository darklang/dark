module Executor.API

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT = LibExecution.ProgramTypes


open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type HttpContextExtensions() =

  [<Extension>]
  static member ReadVanillaJsonAsync<'T>(ctx : HttpContext) : Task<'T> =
    task {
      // use t = startTimer "read-vanilla-json-async" ctx
      use ms = new System.IO.MemoryStream()
      do! ctx.Request.Body.CopyToAsync(ms)
      let body = ms.ToArray() |> UTF8.ofBytesUnsafe
      // t.next "deserialize-json"
      try
        return Json.Vanilla.deserialize<'T> body
      with
      | e ->
        System.Console.WriteLine("Error deserializing json: " + e.Message)
        System.Console.WriteLine("JSON is: " + e.Message)
        e.Reraise()
        return Unchecked.defaultof<'T>
    }


module ExecuteText =
  // TODO add ClientTypes
  type Request = { code : string; symtable : Map<string, RT.Dval> }

  let post (ctx : HttpContext) : Task<LibExecution.RuntimeTypes.Dval> =
    task {
      let! ps = ctx.ReadVanillaJsonAsync<Request>()
      let expr = Parser.parseRTExpr ps.code
      let! result = Execute.execute expr ps.symtable
      return result
    }

module ExecuteJson =
  // TODO add ClientTypes
  type Request = { expr : PT.Expr; symtable : Map<string, RT.Dval> }

  let post (ctx : HttpContext) : Task<LibExecution.RuntimeTypes.Dval> =
    task {
      let! ps = ctx.ReadVanillaJsonAsync<Request>()
      let expr = PT2RT.Expr.toRT ps.expr
      let! result = Execute.execute expr ps.symtable
      return result
    }
