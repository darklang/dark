/// API endpoints to list and manage Packages
module ApiServer.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module CTApi = ClientTypes.Api
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes

module ListV1 =
  /// API endpoint to fetch a list of available Packages
  let packages
    (packages : List<PT.Package.Fn>)
    (ctx : HttpContext)
    : Task<CTApi.Packages.ListV1.Response> =
    task {
      use t = startTimer "read-api" ctx

      t.next "convert"
      let packages = List.map CT2Program.Package.Fn.toCT packages
      return packages
    }


// | `POST, ["api"; canvas; "packages"; "upload_function"] when user.admin ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (upload_function ~execution_id ~user parent body))
