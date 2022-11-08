/// API endpoints to list and manage Packages
module ApiServer.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module ListV1 =
  type T = List<PT.Package.Fn>

  /// API endpoint to fetch a list of available Packages
  let packages (packages : List<PT.Package.Fn>) (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx

      t.next "convert"
      return packages
    }


// | `POST, ["api"; canvas; "packages"; "upload_function"] when user.admin ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (upload_function ~execution_id ~user parent body))
