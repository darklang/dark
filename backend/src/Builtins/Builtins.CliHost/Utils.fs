/// Command-line arguments and parsed scripts.
module Builtins.CliHost.Utils

open Prelude

module PT = LibExecution.ProgramTypes

/// Separate options from the command or script path that follows them.
/// Leave everything after that point unchanged, even arguments that look like flags.
/// Both CLI startup and the Dark script runner use this rule.
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
      /// Eval expressions, or internal entry functions for a script.
      exprs : List<PT.Expr>
    }
