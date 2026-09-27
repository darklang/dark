/// Trait dispatch: picking the impl for a self type.
///
/// An impl is its own package item (`PT.TraitImpl`), and the store reads the ones
/// for a trait into `ImplCandidate`s. This module is the pure half
/// of dispatch: given the candidates for a trait and the self value's type, which
/// candidate applies. The interpreter does the rest (finding the self type, loading
/// the method's fn).
///
/// Matching is by the type's HEAD: `Point` matches `impl ... for Point`, `List<Int>`
/// matches `impl<'a: Show> ... for List<'a>`. Type arguments are compared only to
/// break a tie between two candidates with the same head, so `impl for Option<Int>`
/// and `impl for Option<String>` can coexist. A blanket impl (`for 'a`) matches
/// anything and loses to any specific one.
module LibExecution.Traits

open Prelude
open RuntimeTypes



/// The dispatch key of a type: what an impl is "for".
[<RequireQualifiedAccess>]
type Head =
  | Prim of string
  | Custom of FQTypeName.Package
  | List
  | Dict
  | Tuple
  | Fn
  | Stream
  | DB
  | Any // a blanket impl's `'a`

let private primHeadName (t : TypeReference) : Option<string> =
  match t with
  | TUnit -> Some "Unit"
  | TBool -> Some "Bool"
  | TInt8 -> Some "Int8"
  | TUInt8 -> Some "UInt8"
  | TInt16 -> Some "Int16"
  | TUInt16 -> Some "UInt16"
  | TInt32 -> Some "Int32"
  | TUInt32 -> Some "UInt32"
  | TInt64 -> Some "Int64"
  | TUInt64 -> Some "UInt64"
  | TInt128 -> Some "Int128"
  | TUInt128 -> Some "UInt128"
  | TInt -> Some "Int"
  | TFloat -> Some "Float"
  | TChar -> Some "Char"
  | TString -> Some "String"
  | TUuid -> Some "Uuid"
  | TDateTime -> Some "DateTime"
  | TBlob -> Some "Blob"
  | _ -> None

let headOfTypeReference (t : TypeReference) : Option<Head> =
  match t with
  | TCustomType({ resolved = Ok(FQTypeName.Package h) }, _) -> Some(Head.Custom h)
  | TCustomType({ resolved = Error _ }, _) -> None
  | TList _ -> Some Head.List
  | TDict _ -> Some Head.Dict
  | TTuple _ -> Some Head.Tuple
  | TFn _ -> Some Head.Fn
  | TStream _ -> Some Head.Stream
  | TDB _ -> Some Head.DB
  | TVariable _ -> Some Head.Any
  | prim -> primHeadName prim |> Option.map Head.Prim

let headOfKnownType (kt : KnownType) : Head =
  match kt with
  | KTUnit -> Head.Prim "Unit"
  | KTBool -> Head.Prim "Bool"
  | KTInt8 -> Head.Prim "Int8"
  | KTUInt8 -> Head.Prim "UInt8"
  | KTInt16 -> Head.Prim "Int16"
  | KTUInt16 -> Head.Prim "UInt16"
  | KTInt32 -> Head.Prim "Int32"
  | KTUInt32 -> Head.Prim "UInt32"
  | KTInt64 -> Head.Prim "Int64"
  | KTUInt64 -> Head.Prim "UInt64"
  | KTInt128 -> Head.Prim "Int128"
  | KTUInt128 -> Head.Prim "UInt128"
  | KTInt -> Head.Prim "Int"
  | KTFloat -> Head.Prim "Float"
  | KTChar -> Head.Prim "Char"
  | KTString -> Head.Prim "String"
  | KTUuid -> Head.Prim "Uuid"
  | KTDateTime -> Head.Prim "DateTime"
  | KTBlob -> Head.Prim "Blob"
  | KTStream _ -> Head.Stream
  | KTList _ -> Head.List
  | KTTuple _ -> Head.Tuple
  | KTFn _ -> Head.Fn
  | KTDB _ -> Head.DB
  | KTCustomType(FQTypeName.Package h, _) -> Head.Custom h
  | KTDict _ -> Head.Dict


/// Does a candidate's self type agree with the value's type beyond the head? Only
/// consulted to break a tie. A type variable agrees with anything; `Unknown` agrees
/// with anything; otherwise heads must match recursively.
let rec private argsAgree (t : TypeReference) (vt : ValueType) : bool =
  match t, vt with
  | TVariable _, _ -> true
  | _, ValueType.Unknown -> true
  | TList a, ValueType.Known(KTList b) -> argsAgree a b
  | TStream a, ValueType.Known(KTStream b) -> argsAgree a b
  | TDB a, ValueType.Known(KTDB b) -> argsAgree a b
  | TDict(k, v), ValueType.Known(KTDict(k', v')) -> argsAgree k k' && argsAgree v v'
  | TTuple(a, b, rest), ValueType.Known(KTTuple(a', b', rest')) ->
    argsAgree a a'
    && argsAgree b b'
    && List.length rest = List.length rest'
    && List.forall2 argsAgree rest rest'
  | TFn(args, ret), ValueType.Known(KTFn(args', ret')) ->
    NEList.length args = NEList.length args'
    && List.forall2 argsAgree (NEList.toList args) (NEList.toList args')
    && argsAgree ret ret'
  | TCustomType({ resolved = Ok(FQTypeName.Package h) }, targs),
    ValueType.Known(KTCustomType(FQTypeName.Package h', vargs)) ->
    h = h'
    && (List.length targs = List.length vargs && List.forall2 argsAgree targs vargs
        || List.isEmpty targs)
  | _, ValueType.Known kt ->
    match headOfTypeReference t with
    | Some head -> head = headOfKnownType kt
    | None -> false


type Selection =
  | Selected of ImplCandidate
  | NoImpl
  /// Rivals with no way to order them: every one of them came from outside the op log, so nothing
  /// says which was written later. The call reports them instead of picking.
  | Ambiguous of List<ImplCandidate>

/// Every candidate that applies to a self type.
///
/// An implementation applies when its head matches AND its type arguments do: `Show for
/// Option<Int>` must not answer `Show.show (Some "x")`. A blanket `for 'a` is the fallback,
/// whether nothing specific matched the head or nothing specific fit the arguments.
///
/// `select` narrows this to one. It is also what a receiver call (`p.show`) asks, because
/// there the question is which TRAITS apply, not which implementation: two traits that both
/// declare `show` are an ambiguity a stamp must not break.
let applicable
  (candidates : List<ImplCandidate>)
  (self : KnownType)
  : List<ImplCandidate> =
  let head = headOfKnownType self
  match
    candidates
    |> List.filter (fun c ->
      headOfTypeReference c.self = Some head
      && argsAgree c.self (ValueType.Known self))
  with
  | [] ->
    candidates |> List.filter (fun c -> headOfTypeReference c.self = Some Head.Any)
  | fitting -> fitting

/// The candidate for a self type, if exactly one applies.
let select (candidates : List<ImplCandidate>) (self : KnownType) : Selection =
  match applicable candidates self with
  | [] -> NoImpl
  | [ one ] -> Selected one
  | several ->
    // Two implementations for one type: the one written later runs, by the stamp of the op that
    // added it and the rule the op-fold uses for two bindings of one name. Both stay in the
    // store; `dark constraints` reports the pair, and deprecating one settles it.
    let stamped =
      several
      |> List.map (fun c ->
        let (Hash h) = c.source
        (c, c.stamp, h))
    match Lww.winnerOf stamped with
    | Some winner -> Selected winner
    | None -> Ambiguous several
