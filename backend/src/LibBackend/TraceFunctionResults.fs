module LibBackend.TraceFunctionResults

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module AT = LibExecution.AnalysisTypes
module RT = LibExecution.RuntimeTypes
module ReprHash = LibExecution.DvalReprInternalHash
module ReprRoundtrippable = LibExecution.DvalReprInternalRoundtrippable

// -------------------------
// External
// -------------------------

// The data we save to store this
type FunctionResultKey = tlid * RT.FQFnName.T * id * string

type FunctionResultValue = RT.Dval * NodaTime.Instant
