/// Values with reads in flight inside them (`docs/processes.md`, "Reads are concurrent").
///
/// Storing a read in a list, record, tuple, enum, dict value or closure does not wait for it:
/// the value is built as it is and handed out as a `DPromise` whose `Shell` is that value, and
/// whose task lands once the reads directly inside have. So a value that is not a `DPromise`
/// holds no read at any depth, and every check that refuses one still tests only the top.
module LibExecution.Pending

open System.Threading.Tasks

open Prelude
open RuntimeTypes

module VT = ValueType


/// A read inside a value failed: the read itself, so the error names it, not its container.
exception ReadFailed of read : Promise * failure : exn


let inline isPromise (dv : Dval) : bool =
  match dv with
  | DPromise _ -> true
  | _ -> false

/// Whether a read sits directly inside `dv`. One level is enough: a deeper container holding
/// one is itself a promise at this level. No allocation; it runs on every construction.
let holdsPromise (dv : Dval) : bool =
  match dv with
  | DList(_, items)
  | DEnum(_, _, _, _, items) -> List.exists isPromise items
  | DTuple(a, b, rest) -> isPromise a || isPromise b || List.exists isPromise rest
  | DDict(_, _, entries) -> Map.exists (fun _ v -> isPromise v) entries
  | DRecord(_, _, _, fields) -> Map.exists (fun _ v -> isPromise v) fields
  | DApplicable(AppLambda l) ->
    List.exists isPromise l.argsSoFar
    || Array.exists (fun (struct (_, v)) -> isPromise v) l.closedRegisters
  | DApplicable(AppNamedFn n) -> List.exists isPromise n.argsSoFar
  | _ -> false

/// The values directly inside `dv`, in order.
let private children (dv : Dval) : List<Dval> =
  match dv with
  | DList(_, items)
  | DEnum(_, _, _, _, items) -> items
  | DTuple(a, b, rest) -> a :: b :: rest
  | DDict(_, _, entries) -> Map.values entries
  | DRecord(_, _, _, fields) -> Map.values fields
  | DApplicable(AppLambda l) ->
    l.argsSoFar
    @ (l.closedRegisters |> Array.map (fun (struct (_, v)) -> v) |> List.ofArray)
  | DApplicable(AppNamedFn n) -> n.argsSoFar
  | _ -> []

/// A read that has landed as its value; anything else as it is.
let settled (dv : Dval) : Dval =
  match dv with
  | DPromise p when p.Task.IsCompletedSuccessfully -> p.Task.Result
  | _ -> dv

/// `dv` with its landed reads as their values. A list's element type (what a `DList` holds)
/// is worked out again, since a read counted as `Unknown` when it was built.
let private fill (dv : Dval) : Dval =
  match dv with
  | DList(elemType, items) ->
    let items = List.map settled items
    let merge (acc : ValueType) item =
      match VT.merge acc (Dval.toValueType item) with
      | Ok merged -> merged
      | Error() -> acc
    DList(List.fold merge elemType items, items)
  | DTuple(a, b, rest) -> DTuple(settled a, settled b, List.map settled rest)
  | DDict(kt, vt, entries) -> DDict(kt, vt, Map.map settled entries)
  | DRecord(src, rt, typeArgs, fields) ->
    DRecord(src, rt, typeArgs, Map.map settled fields)
  | DEnum(src, rt, typeArgs, case, fields) ->
    DEnum(src, rt, typeArgs, case, List.map settled fields)
  | DApplicable(AppLambda l) ->
    let closed =
      l.closedRegisters |> Array.map (fun (struct (r, v)) -> struct (r, settled v))
    DApplicable(
      AppLambda
        { l with argsSoFar = List.map settled l.argsSoFar; closedRegisters = closed }
    )
  | DApplicable(AppNamedFn n) ->
    DApplicable(AppNamedFn { n with argsSoFar = List.map settled n.argsSoFar })
  | _ -> dv

/// What a read's task failed with, as the read that failed. One already a `ReadFailed` (a
/// container's) passes as it is, so the innermost read is named.
let readFailure (p : Promise) (t : Task<Dval>) : exn =
  match t.Exception with
  | null -> ReadFailed(p, System.OperationCanceledException "a read was cancelled")
  | agg ->
    match agg.GetBaseException() with
    | ReadFailed _ as e -> e
    | ex -> ReadFailed(p, ex)

/// `dv` as it should be held: a promise of its own when a read sits directly inside it, named
/// by the first (what `ps` shows), landing as `rebuild` of it once they all have, the first to
/// fail in order failing it. Reads already landed are not carried: then it is just `rebuild`.
/// `rebuild` builds the value as its instruction would have from values, so its checks hold.
let built (rebuild : Dval -> Ply<Dval>) (dv : Dval) : Dval =
  let reads =
    if holdsPromise dv then
      children dv
      |> List.choose (fun v ->
        match v with
        | DPromise p -> Some p
        | _ -> None)
    else
      []
  match reads with
  | [] -> dv
  | first :: _ when
    List.forall (fun (p : Promise) -> p.Task.IsCompletedSuccessfully) reads
    ->
    let rebuilt = rebuild (fill dv)
    match Ply.trySync rebuilt with
    | ValueSome v -> v
    | ValueNone -> DPromise(Promise(Ply.toTask rebuilt, first.Fn, first.Site))
  | first :: _ ->
    let landed =
      task {
        for p in reads do
          let! t =
            p.Task.ContinueWith(
              (fun (t : Task<Dval>) -> t),
              TaskContinuationOptions.ExecuteSynchronously
            )
          if not t.IsCompletedSuccessfully then raise (readFailure p t)
        return! Ply.toTask (rebuild (fill dv))
      }
    DPromise(Promise(landed, first.Fn, first.Site, dv))

/// `built`, for a value that needs no checking again (a builtin's result, a list's tail).
let wrap (dv : Dval) : Dval = built (fun d -> Ply d) dv

/// What a pattern or a field access can look at without waiting: a shell, or `dv` itself.
let inline peek (dv : Dval) : Dval =
  match dv with
  | DPromise p when p.HasShell -> p.Shell
  | _ -> dv

let rec private mentionsTypeVariable (t : TypeReference) : bool =
  match t with
  | TVariable _ -> true
  | TList inner -> mentionsTypeVariable inner
  | TDict(k, v) -> mentionsTypeVariable k || mentionsTypeVariable v
  | TTuple(a, b, rest) ->
    mentionsTypeVariable a
    || mentionsTypeVariable b
    || List.exists mentionsTypeVariable rest
  | TCustomType(_, args) -> List.exists mentionsTypeVariable args
  | TFn _ -> true
  | _ -> false

/// `dv`, a builtin's result typed `t`, held as it should be when reads went in: every
/// container along `t` that holds one, innermost first, a promise of its own, landing as
/// `rebuild` of it. Only where `t` mentions a type variable can a read have ended up.
let rec wrapAlong
  (rebuild : Dval -> Ply<Dval>)
  (t : TypeReference)
  (dv : Dval)
  : Dval =
  if not (mentionsTypeVariable t) then
    dv
  else
    let again = wrapAlong rebuild
    match dv, t with
    | DList(vt, items), TList inner ->
      built rebuild (DList(vt, List.map (again inner) items))
    | DDict(kt, vt, entries), TDict(_, inner) ->
      built rebuild (DDict(kt, vt, Map.map (again inner) entries))
    | DTuple(a, b, rest), TTuple(ta, tb, trest) when
      List.length rest = List.length trest
      ->
      built rebuild (DTuple(again ta a, again tb b, List.map2 again trest rest))
    | _ -> built rebuild dv
