module Builtins.Language.Libs.LanguageTools

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution


let builtinValue () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinValue ())

let builtinFnParam () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFnParam ())
let builtinFn () = FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFn ())

let builtinFnPurity () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.builtinFnPurity ())

let platform () = FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.platform ())

let purityToDT (p : Previewable) : Dval =
  let typeName = builtinFnPurity ()
  let caseName =
    match p with
    | Pure -> "Pure"
    | ImpurePreviewable -> "ImpurePreviewable"
    | Impure -> "Impure"
  DEnum(typeName, typeName, [], caseName, [])

let fns () : List<BuiltInFn> =
  [ { name = fn "platformsInstalled" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (platform ()), []) |> TList
      description =
        "The platforms this runtime was composed from: what each contributes, and the whole set "
        + "of effects its builtins may perform. Empty if the host did not record them."
      fn =
        (function
        | exeState, _, _, [| DUnit |] ->
          let rows =
            exeState.platforms
            |> List.sortBy _.name
            |> List.map (fun p ->
              // The union over the platform's builtins PLUS what they can ask for from inside
              // their own bodies. The static declarations alone understate a platform whose
              // builtins decide by argument, which is what `dynamicEffects` exists to carry.
              let effects =
                p.builtins.fns.Values
                |> Seq.fold (fun acc f -> Set.union acc f.callEffects) p.dynamicEffects
                |> Set.toList
                |> List.map LibExecution.Effects.name
                |> List.sort
                |> List.map DString
              // The names as well as the count. `dark builtins --by-platform` groups by them, and
              // deriving that grouping beats the hand-maintained prefix table it replaces, which
              // had already drifted (it listed `print` and `debug` as modules).
              let fnNames =
                p.builtins.fns.Keys
                |> Seq.map _.name
                |> Seq.sort
                |> Seq.map DString
                |> List.ofSeq
              let fields =
                [ "name", DString p.name
                  "version", DInt64(int64 p.version)
                  "fingerprint",
                  DString(LibExecution.Platform.Platform.fingerprint p)
                  // Only an external platform has one. A linked platform is part of this binary,
                  // so there is no file to name.
                  "artifactHash",
                  (LibDB.InstalledPlatforms.artifactHashOf p.name
                   |> Option.map DString
                   |> Dval.option KTString)
                  "description", DString p.description
                  "effects", DList(VT.string, effects)
                  "fnCount", DInt64(int64 p.builtins.fns.Count)
                  "fns", DList(VT.string, fnNames)
                  "requiresStore", DBool p.requiresStore ]
              DRecord(platform (), platform (), [], Map fields))
          DList(VT.customType (platform ()) [], rows) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "getAllBuiltinValues" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (builtinValue ()), []) |> TList
      description =
        "Returns a list of the Builtin values (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [| DUnit |] ->
          let vals =
            exeState.values.builtIn
            |> Dictionary.toSortedList
            |> List.map (fun (name, (data : BuiltInValue)) ->
              let fields =
                [ "name", RT2DT.FQValueName.Builtin.toDT name
                  "description", DString data.description
                  "returnType", RT2DT.TypeReference.toDT data.typ ]

              DRecord(builtinValue (), builtinValue (), [], Map fields))

          DList(VT.customType (builtinValue ()) [], vals) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "getAllBuiltinFns" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (builtinFn ()), []) |> TList
      description =
        "Returns a list of the Builtin functions (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [| DUnit |] ->
          let fns =
            exeState.fns.builtIn
            |> Dictionary.toSortedList
            |> List.map (fun (name, data) ->
              let parameters =
                data.parameters
                |> List.map (fun p ->
                  let fields =
                    [ "name", DString p.name
                      "type", RT2DT.TypeReference.toDT p.typ ]
                  DRecord(builtinFnParam (), builtinFnParam (), [], Map fields))
                |> Dval.list (KTCustomType(builtinFnParam (), []))

              // Names rather than the structured `Effect`, matching how a platform's effects
              // already reach Dark. A name is what a policy rule is written in and what the
              // listing prints, so the structured form would be converted straight back.
              let effects =
                data.callEffects
                |> Set.toList
                |> List.map LibExecution.Effects.name
                |> List.sort
                |> List.map DString
                |> Dval.list KTString

              let fields =
                [ "name", RT2DT.FQFnName.Builtin.toDT name
                  "description", DString data.description
                  "parameters", parameters
                  "returnType", RT2DT.TypeReference.toDT data.returnType
                  "purity", purityToDT data.previewable
                  "effects", effects ]

              DRecord(builtinFn (), builtinFn (), [], Map fields))

          DList(VT.customType (builtinFn ()) [], fns) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
