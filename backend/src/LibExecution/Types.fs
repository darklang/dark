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
module ValueType =
  /// This assumes that TVariables have already been substituted
  /// i.e. they're not allowed, and will raise an exception
  let rec fromFullySubstitutedTypeReference (t : TypeReference) : ValueType =
    let r = fromFullySubstitutedTypeReference
    let c t = t |> r |> Some

    match t with
    | TUnit -> VTUnit
    | TBool -> VTBool
    | TInt -> VTInt
    | TFloat -> VTFloat
    | TChar -> VTChar
    | TString -> VTString
    | TUuid -> VTUuid
    | TBytes -> VTBytes
    | TDateTime -> VTDateTime
    | TPassword -> VTPassword

    | TList t -> VTList(c t)
    | TTuple(t1, t2, rest) -> VTTuple(r t1, r t2, List.map r rest)
    | TDict t -> VTDict(c t)

    | TFn(args, ret) -> VTFn(List.map c args, c ret)

    | TDB typ -> VTDB(r typ)

    | TCustomType(t, typeArgs) -> VTCustomType(t, List.map c typeArgs)

    | TVariable _ ->
      Exception.raiseInternal
        "TVariable should have been substituted before calling toValueType"
        []
