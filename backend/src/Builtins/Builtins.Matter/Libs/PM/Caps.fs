/// Builtins behind `dark caps` — the MINIMAL core for viewing + controlling this instance's grant.
///
/// The grant is a structured `Capabilities` record stored LOCALLY in `~/.darklang/capabilities.bin`
/// (never synced), default NONE. These builtins exchange the STRUCTURED value across the F#/Dark
/// boundary (via `CapabilitiesToDarkTypes`) — F# never parses or renders the grant-spec language; that
/// all lives in `.dark` (`LanguageTools.Capabilities.parse`/`render`). `pmCapsGet` reads the grant,
/// `pmCapsSet` REPLACES it; `pmFnEffectiveCaps` is the static analyzer behind `dark caps needed-for
/// <fn>`. The runtime gate that ENFORCES the grant lives in the interpreter (`Capabilities.covers`).
module Builtins.Matter.Libs.PM.Caps

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module Builtin = LibExecution.Builtin
module C2DT = LibExecution.CapabilitiesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution

open Builtin.Shortcuts


/// The structured `Capabilities` Dark type, as a TypeReference (builtin param/return).
let private capsType : TypeReference =
  TCustomType(NR.ok (C2DT.Capabilities.typeName ()), [])


let fns : List<BuiltInFn> =
  [ { name = fn "pmCapsGet" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = capsType
      description =
        "This instance's current capability grant, as a structured <type Capabilities> value (NONE — the
         strict default — is the all-empty grant). The CORE read primitive: `.dark` renders it and
         composes adjustments over it."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            return LibDB.CapabilityGrants.getGrant () |> C2DT.Capabilities.toDT
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmCapsSet" 0
      typeParams = []
      parameters = [ Param.make "caps" capsType "The full grant to replace with" ]
      returnType = TUnit
      description =
        "REPLACE the whole grant with exactly this <type Capabilities> — the CORE write primitive. The
         all-empty grant sets NONE. `.dark` parses/validates spec strings into the structure and builds
         grant/revoke/clear/profile on top of get+set."
      fn =
        (function
        | _, _, _, [ caps ] ->
          uply {
            C2DT.Capabilities.fromDT caps |> LibDB.CapabilityGrants.setGrant
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmFnEffectiveCaps" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "The package-fn hash to analyze" ]
      returnType = capsType
      description =
        "The capabilities a package fn (transitively) needs, as a structured <type Capabilities> value
         (behind `dark caps needed-for <fn>`). See `LibDB.PackageCaps`."
      fn =
        (function
        | exeState, _, _, [ DString hashStr ] ->
          uply {
            // a builtin's declared caps, by flat name — built from the runtime's builtin map (no name
            // magic; the need lives on each `BuiltInFn`).
            let capsByName =
              exeState.fns.builtIn
              |> Map.toList
              |> List.map (fun (k, b) -> k.name, b.capabilities)
              |> Map.ofList
            let capsFor (name : string) : LibExecution.Capabilities.Capabilities =
              Map.tryFind name capsByName
              |> Option.defaultValue LibExecution.Capabilities.noCaps
            let! caps = LibDB.PackageCaps.effectiveCaps capsFor hashStr
            return C2DT.Capabilities.toDT caps
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
