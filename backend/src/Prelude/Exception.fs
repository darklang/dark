module Exception

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

// ----------------------
// Exceptions
// We don't use the F# exception syntax as we want to allow wrapping inner exceptions
// ----------------------

type Metadata = List<string * obj>

/// An error within Dark itself - we need to rollbar this and address it.
///
/// Do not show to anyone, unless within an Analysis request.
type InternalException(message : string, metadata : Metadata, inner : exn) =
  inherit System.Exception(message, inner)
  member _.metadata = metadata
  new(msg : string) = InternalException(msg, [], null)
  new(msg : string, metadata : Metadata) = InternalException(msg, metadata, null)
  new(msg : string, inner : exn) = InternalException(msg, [], inner)


/// An error during code execution, which is the responsibility of the
/// User/Developer. The message can be shown to the developer. You can alternatively
/// use GrandUser exception in code which is used in both Libraries and in the
/// HttpFramework.
type CodeException(message : string, inner : exn) =
  inherit System.Exception(message, inner)
  new(msg : string) = CodeException(msg, null)

/// An editor exception is one which is caused by an invalid action on the part of
/// the Dark editor, such as an Redo or rename that isn't allowed.  We are
/// interested in these, as the editor should have caught this on the client and not
/// made the request. The message may be shown to the logged-in user, and should be
/// suitable for this.
type EditorException(message : string, inner : exn) =
  inherit System.Exception(message, inner)
  new(msg : string) = EditorException(msg, null)

// A pageable exception will cause the pager to go off! This is something that should
// never happen and is an indicator that the service is broken in some way.  The
// pager goes off because a pageable exception sets the `{ is_pageable: true }`
// metadata, which causes a honeycomb trigger that sets off PagerDuty.
type PageableException(message : string, metadata : Metadata, inner : exn) =
  inherit System.Exception(message, inner)
  member _.metadata = metadata


// This is for tracing
let mutable exceptionCallback = (fun (_e : exn) -> ())

let mutable sendRollbarError = (fun (_message : string) (_metadata : Metadata) -> ())


/// Returns a list of exceptions of this exception, and all nested inner
/// exceptions.
let rec getMessages (e : exn) : List<string> =
  if isNull e.InnerException then
    [ e.Message ]
  else
    e.Message :: getMessages e.InnerException

let toMetadata (e : exn) : Metadata =
  let thisMetadata =
    match e with
    | :? PageableException as e -> [ "is_pageable", true :> obj ] @ e.metadata
    | :? InternalException as e -> e.metadata
    | :? EditorException
    | :? CodeException
    | _ -> []
  thisMetadata

let rec nestedMetadata (e : exn) : Metadata =
  let innerMetadata =
    if not (isNull e.InnerException) then nestedMetadata e.InnerException else []
  let thisMetadata =
    match e with
    | :? PageableException as e -> [ "is_pageable", true :> obj ] @ e.metadata
    | :? InternalException as e -> e.metadata
    | :? EditorException
    | :? CodeException
    | _ -> []
  thisMetadata @ innerMetadata


let callExceptionCallback (e : exn) =
  try
    exceptionCallback e
  with e ->
    // We're completely screwed at this point
    System.Console.WriteLine "Exception calling callExceptionCallback"
    System.Console.WriteLine(e.Message)
    System.Console.WriteLine e.StackTrace


let raiseInternal (msg : string) (tags : Metadata) =
  let e = InternalException(msg, tags)
  callExceptionCallback e
  raise e


let unwrapOptionInternal (msg : string) (tags : Metadata) (o : Option<'a>) : 'a =
  match o with
  | Some v -> v
  | None -> raiseInternal msg tags

let unwrapResultInternal (tags : Metadata) (r : Result<'a, 'msg>) : 'a =
  match r with
  | Ok v -> v
  | Error msg -> raiseInternal (string msg) tags

let reraiseAsPageable (msg : string) (tags : Metadata) (e : exn) =
  let e = PageableException(msg, tags, e)
  callExceptionCallback e
  raise e

let unknownErrorMessage = "Unknown error"

let taskCatch (f : unit -> Task<'r>) : Task<Option<'r>> =
  task {
    try
      let! result = f ()
      return Some result
    with _ ->
      return None
  }

let catch (f : unit -> 'r) : Option<'r> =
  try
    Some(f ())
  with _ ->
    None

let catchError (f : unit -> 'r) : Result<'r, string> =
  try
    Ok(f ())
  with e ->
    Error e.Message


/// <summary>
/// This hack adds a `Reraise` method to exceptions, since
/// it's not normally possible to reraise exceptions within F# CEs.
/// </summary>
///
/// <remarks>
/// Not sure if this actually works
/// Sources:
/// - https://github.com/fsharp/fslang-suggestions/issues/660#issuecomment-382070639
/// - https://stackoverflow.com/questions/57383
/// </remarks>
let reraise (e : System.Exception) : 'a =
  (System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture e).Throw()
  Unchecked.defaultof<_>
