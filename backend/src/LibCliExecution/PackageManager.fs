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
  RT.PackageManager.Empty
