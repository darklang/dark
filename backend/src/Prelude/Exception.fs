module Exception

open System.Threading.Tasks

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


/// A condition of the MACHINE rather than of the program: the store is read-only, the disk is full, the
/// store belongs to another user, the policy file cannot be trusted, an approval names a version the store
/// no longer holds. Distinguished from `InternalException` because the response is opposite
/// -- there is nothing to fix in the code, nothing to report, and the person running the command can
/// resolve it themselves once told which of those it is. Carries a sentence written for them, so a caller
/// that catches this can print it and nothing else.
type StoreConditionException(message : string, metadata : Metadata, inner : exn) =
  inherit System.Exception(message, inner)
  member _.metadata = metadata
  new(msg : string) = StoreConditionException(msg, [], null)
  new(msg : string, metadata : Metadata) =
    StoreConditionException(msg, metadata, null)


/// Returns a list of exceptions of this exception, and all nested inner
/// exceptions.
let rec getMessages (e : exn) : List<string> =
  if isNull e.InnerException then
    [ e.Message ]
  else
    e.Message :: getMessages e.InnerException

let toMetadata (e : exn) : Metadata =
  match e with
  | :? InternalException as e -> e.metadata
  | _ -> []


let raiseInternal (msg : string) (tags : Metadata) =
  raise (InternalException(msg, tags))

/// See `StoreConditionException`. Not reported anywhere: this is the machine's state, not a defect.
let raiseStoreCondition (msg : string) (tags : Metadata) =
  raise (StoreConditionException(msg, tags))

/// The `StoreConditionException` inside <param e>, however it arrived: raised directly, or wrapped by
/// a task or an aggregate on the way out.
let rec findStoreCondition (e : exn) : Option<StoreConditionException> =
  match e with
  | :? StoreConditionException as s -> Some s
  | :? System.AggregateException as agg ->
    agg.InnerExceptions |> Seq.tryPick findStoreCondition
  | _ ->
    if isNull e.InnerException then None else findStoreCondition e.InnerException

let unwrapOptionInternal (msg : string) (tags : Metadata) (o : Option<'a>) : 'a =
  match o with
  | Some v -> v
  | None -> raiseInternal msg tags

let catch (f : unit -> 'r) : Option<'r> =
  try
    Some(f ())
  with _ ->
    None


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
