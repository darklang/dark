module LibExecution.Types

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module J = Prelude.Json

open RuntimeTypes

let empty =
  { builtIn = Map.empty; package = (fun _ -> Ply None); userProgram = Map.empty }

let find (name : TypeName.T) (types : Types) : Ply<Option<TypeDeclaration.T>> =
  match name with
  | FQName.BuiltIn b ->
    Map.tryFind b types.builtIn |> Option.map (fun t -> t.declaration) |> Ply
  | FQName.UserProgram user ->
    Map.tryFind user types.userProgram |> Option.map (fun t -> t.declaration) |> Ply
  | FQName.Package pkg ->
    types.package pkg |> Ply.map (Option.map (fun t -> t.declaration))
  | FQName.Unknown _ -> Ply None

// Swap concrete types for type parameters
let rec substitute
  (typeParams : List<string>)
  (typeArguments : List<TypeReference>)
  (typ : TypeReference)
  : TypeReference =
  let substitute = substitute typeParams typeArguments
  match typ with
  | TVariable v ->
    if typeParams.Length = typeArguments.Length then
      List.zip typeParams typeArguments
      |> List.find (fun (param, _) -> param = v)
      |> Option.map snd
      |> Exception.unwrapOptionInternal
        "No type argument found for type parameter"
        []
    else
      Exception.raiseInternal
        $"typeParams and typeArguments have different lengths"
        [ "typeParams", typeParams; "typeArguments", typeArguments ]


  | TUnit
  | TBool
  | TInt
  | TFloat
  | TChar
  | TString
  | TUuid
  | TBytes
  | TDateTime
  | TPassword -> typ

  | TList t -> TList(substitute t)
  | TTuple(t1, t2, rest) ->
    TTuple(substitute t1, substitute t2, List.map substitute rest)
  | TFn _ -> typ // TYPESTODO
  | TDB _ -> typ // TYPESTODO
  | TCustomType(typeName, typeArgs) ->
    TCustomType(typeName, List.map substitute typeArgs)
  | TDict t -> TDict(substitute t)



let rec getTypeReferenceFromAlias
  (types : Types)
  (typ : TypeReference)
  : Ply<TypeReference> =
  match typ with
  | TCustomType(typeName, typeArgs) ->
    uply {
      match! find typeName types with
      | Some({ definition = TypeDeclaration.Alias(TCustomType(innerTypeName, _)) }) ->
        return!
          getTypeReferenceFromAlias types (TCustomType(innerTypeName, typeArgs))
      | _ -> return typ
    }
  | _ -> Ply typ

// maybe move this file back to RT
module KnownType =
  /// This assumes that TVariables have already been substituted
  /// i.e. they're not allowed, and will raise an exception
  let rec fromFullySubstitutedTypeReference (t : TypeReference) : KnownType =
    let r = fromFullySubstitutedTypeReference
    let k t = t |> r |> Known

    match t with
    | TUnit -> KTUnit
    | TBool -> KTBool
    | TInt -> KTInt
    | TFloat -> KTFloat
    | TChar -> KTChar
    | TString -> KTString
    | TUuid -> KTUuid
    | TBytes -> KTBytes
    | TDateTime -> KTDateTime
    | TPassword -> KTPassword

    | TList t -> KTList(Known (r t))
    | TTuple(t1, t2, rest) -> KTTuple(k t1, k t2, List.map k rest)
    | TDict t -> KTDict(k t)

    | TFn(args, ret) -> KTFn(List.map k args, k ret)

    | TDB typ -> KTDB(k typ)

    | TCustomType(t, typeArgs) -> KTCustomType(t, List.map k typeArgs)

    | TVariable _ ->
      Exception.raiseInternal
        "TVariable should have been substituted before calling toValueType"
        []

module ValueType =
  /// This assumes that TVariables have already been substituted
  /// i.e. they're not allowed, and will raise an exception
  let rec fromTypeReference  (t : TypeReference) : ValueType =
    let r = fromTypeReference

    match t with
    | TUnit -> Known KTUnit
    | TBool -> Known KTBool
    | TInt -> Known KTInt
    | TFloat -> Known KTFloat
    | TChar -> Known KTChar
    | TString -> Known KTString
    | TUuid -> Known KTUuid
    | TBytes -> Known KTBytes
    | TDateTime -> Known KTDateTime
    | TPassword -> Known KTPassword

    | TList t -> KTList(r t) |> Known
    | TTuple(t1, t2, rest) -> KTTuple(r t1, r t2, List.map r rest)  |> Known
    | TDict t -> KTDict(r t) |> Known

    | TFn(args, ret) -> KTFn(List.map r args, r ret) |> Known

    | TDB typ -> KTDB(KnownType.fromFullySubstitutedTypeReference typ |> Known) |> Known

    | TCustomType(t, typeArgs) -> KTCustomType(t, List.map r typeArgs) |> Known

    | TVariable _ -> Unknown

