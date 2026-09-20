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
  /// spelling in a `:{Http, Clock}` row. Custom effects carry their name as a
  /// field instead, so they have no case name of their own.
  let caseName (effect : E.Effect) : string = sprintf "%A" effect

  let toDT (effect : E.Effect) : Dval =
    match effect with
    | E.Effect.Custom name ->
      DEnum(typeName (), typeName (), [], "Custom", [ DString name ])
    | wellKnown -> DEnum(typeName (), typeName (), [], caseName wellKnown, [])

  let fromDT (dval : Dval) : E.Effect =
    match dval with
    | DEnum(_, _, _, "Custom", [ DString name ]) ->
      match E.custom name with
      | Some effect -> effect
      | None -> Exception.raiseInternal "Invalid custom effect name" [ "name", name ]
    | DEnum(_, _, _, case, []) ->
      match E.all |> List.tryFind (fun effect -> caseName effect = case) with
      | Some effect -> effect
      | None -> Exception.raiseInternal "Unknown effect" [ "case", case ]
    | _ -> Exception.raiseInternal "Invalid Permissions.Effect" [ "dval", dval ]

/// A `List<Effect>`; sets have no Dark form. Well-known effects come first, in
/// `Effects.all` order, then custom ones sorted by name -- so the rendering of a set
/// is stable whoever declared what is in it.
let knownType () : KnownType = KTList(VT.known (Effect.knownType ()))

let toDT (effects : Set<E.Effect>) : Dval =
  let wellKnown = E.all |> List.filter (fun effect -> Set.contains effect effects)

  let custom =
    effects
    |> Set.toList
    |> List.choose (fun effect ->
      match effect with
      | E.Effect.Custom name -> Some(name, effect)
      | _ -> None)
    |> List.sortBy fst
    |> List.map snd

  DList(VT.known (Effect.knownType ()), wellKnown @ custom |> List.map Effect.toDT)

let fromDT (dval : Dval) : Set<E.Effect> = dval |> D.list Effect.fromDT |> Set.ofList
