module LibCliExecution.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes

let packageManager : RT.PackageManager =
  // TODO: fetch from internet
  // TODO: fetch one by one
  // TODO: copy back to LibBackend/LibCloudExecution
  // TODO: deal with type changes
  { getType = (fun _ -> Ply None)
    getFn = (fun _ -> Ply None)
    init = uply { return () } }
