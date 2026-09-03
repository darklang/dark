module LibExecution.EffectsToDarkTypes

open Prelude
open RuntimeTypes

module E = LibExecution.Effects
module D = LibExecution.DvalDecoder
module VT = LibExecution.ValueType

/// The Dark `LanguageTools.Permissions.Effect` enum, one case per runtime
/// effect with the same case name.
module Effect =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.effect ())

  let knownType () = KTCustomType(typeName (), [])

  /// The DU case name, which is also the Dark case name and the source
  /// spelling in a `:{Http, Clock}` row.
  let caseName (effect : E.Effect) : string = sprintf "%A" effect

  let toDT (effect : E.Effect) : Dval =
    DEnum(typeName (), typeName (), [], caseName effect, [])

  let fromDT (dval : Dval) : E.Effect =
    match dval with
    | DEnum(_, _, _, case, []) ->
      match E.all |> List.tryFind (fun effect -> caseName effect = case) with
      | Some effect -> effect
      | None -> Exception.raiseInternal "Unknown effect" [ "case", case ]
    | _ -> Exception.raiseInternal "Invalid Permissions.Effect" [ "dval", dval ]

/// A `List<Effect>`; sets have no Dark form, and the order is `Effects.all`.
let knownType () : KnownType = KTList(VT.known (Effect.knownType ()))

let toDT (effects : Set<E.Effect>) : Dval =
  DList(
    VT.known (Effect.knownType ()),
    E.all
    |> List.filter (fun effect -> Set.contains effect effects)
    |> List.map Effect.toDT
  )

let fromDT (dval : Dval) : Set<E.Effect> = dval |> D.list Effect.fromDT |> Set.ofList
