/// Command-line arguments and parsed scripts.
module Builtins.CliHost.Utils

open Prelude

module PT = LibExecution.ProgramTypes

/// Split options from the command or script path.
/// Keep arguments after the path unchanged, including flags.
module ScriptArguments =
  let split (args : List<string>) : List<string> * List<string> =
    let rec loop options remaining =
      match remaining with
      | "--" :: rest -> List.rev ("--" :: options), rest
      | ("--branch" | "--entry-point" as flag) :: value :: rest ->
        loop (value :: flag :: options) rest
      | flag :: rest when flag.StartsWith "-" -> loop (flag :: options) rest
      | rest -> List.rev options, rest
    loop [] args

module CliScript =
  type Definitions =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn> }

  type PTCliScriptModule =
    {
      types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn>
      traits : List<PT.Trait.Trait>
      impls : List<PT.TraitImpl.TraitImpl>
      submodules : Definitions
      /// Eval expressions or script functions that take `args`.
      exprs : List<PT.Expr>
    }
