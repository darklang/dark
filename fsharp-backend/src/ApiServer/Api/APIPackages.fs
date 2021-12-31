module ApiServer.Packages

// API endpoints for Packages

open Microsoft.AspNetCore.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module Convert = LibExecution.OCamlTypes.Convert

module List =
  type T = List<OT.PackageManager.fn>

  let packages (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx

      t.next "load-functions"
      let! fns = Lazy.force LibBackend.PackageManager.cachedForAPI

      t.next "convert"
      return fns |> List.map Convert.pt2ocamlPackageManagerFn
    }


// | `POST, ["api"; canvas; "packages"; "upload_function"] when user.admin ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (upload_function ~execution_id ~user parent body))
