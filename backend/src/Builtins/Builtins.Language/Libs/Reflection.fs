module Builtins.Language.Libs.Reflection

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Exe = LibExecution.Execution
module Dval = LibExecution.Dval
module Traits = LibExecution.Traits
module Interpreter = LibExecution.Interpreter
module PackageRefs = LibExecution.PackageRefs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


let fns () : List<BuiltInFn> =
  [ { name = fn "toRepr" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "value" (TVariable "a") "The value to convert." ]
      returnType = TString
      description = "Returns a string representation of the given <param value>"
      fn =
        (function
        | exeState, _, _, [| value |] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            return DString repr
          }
        | _ -> incorrectArgs ())
      // Impure though it performs nothing: it shows type names as the branch binds them now,
      // so a rename changes the answer.
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "inspectImpl" 0
      typeParams = []
      parameters =
        [ Param.make
            "value"
            (TCustomType(
              { originalName = []
                resolved =
                  Ok(
                    FQTypeName.fqPackage (
                      LibExecution.PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
                    )
                  ) },
              []
            ))
            "A reflected value." ]
      returnType = TypeReference.option TString
      description =
        "What the <type Stdlib.Inspect> impl of the reflected <param value>'s type returns for "
        + "it, or {{None}} when the type has none. The value printer asks this of every record "
        + "and enum."
      fn =
        (function
        | exeState,
          vm,
          _,
          [| DEnum(_, _, _, ("DRecord" | "DEnum"), _ :: runtimeTypeName :: _) as reflected |] ->
          uply {
            let none = Dval.optionNone KTString
            match RT2DT.FQTypeName.fromDT runtimeTypeName with
            | FQTypeName.Package typeHash ->
              let! candidates =
                exeState.fns.implCandidates
                  exeState.branchId
                  (Hash(PackageRefs.Trait.Stdlib.inspect ()))
              // Rebuilding the value is the costly part, so only for a type with an impl.
              let forThisType =
                candidates
                |> List.filter (fun c ->
                  Traits.headOfTypeReference c.self = Some(
                    Traits.Head.Custom typeHash
                  ))
              if List.isEmpty forThisType then
                return none
              else
                let value = RT2DT.Dval.fromDT reflected
                match Dval.toValueType value with
                | ValueType.Known self ->
                  match Traits.select forThisType self with
                  | Traits.Selected c ->
                    match Map.tryFind "inspect" c.methods with
                    | Some fnHash ->
                      let impl =
                        AppNamedFn
                          { name = FQFnName.Package fnHash
                            typeSymbolTable = TST.empty
                            typeArgs = []
                            approvalName = None
                            access = None
                            argsSoFar = []
                            boundImpls = [] }
                      return!
                        Interpreter.requestApply vm impl value ValueNone (fun text ->
                          Ply(Dval.optionSome KTString text))
                    | None -> return none
                  | _ -> return none
                | ValueType.Unknown -> return none
          }
        | _, _, _, [| _ |] -> Ply(Dval.optionNone KTString)
        | _ -> incorrectArgs ())
      // Impure, as `toRepr`: it runs an impl, and which one depends on the branch.
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "reflect" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "dv" (TVariable "a") "The value to reflect on." ]
      returnType =
        TCustomType(
          { originalName = []
            resolved =
              Ok(
                FQTypeName.fqPackage (
                  LibExecution.PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
                )
              ) },
          []
        )
      description =
        "Returns a meta representation of the real underlying dval, as a <type "
        + "LanguageTools.RuntimeTypes.Dval>. This is what lets a value be turned "
        + "back into source (see <fn "
        + "LanguageTools.RuntimeTypesToProgramTypes.dvalToExpr>)."
      fn =
        (function
        | _, _, _, [| dv |] ->
          dv |> LibExecution.RuntimeTypesToDarkTypes.Dval.toDT |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
