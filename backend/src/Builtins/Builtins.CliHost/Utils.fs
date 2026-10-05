/// The shape `Cli.fs` carries a parsed script in, between parsing it and authoring it. There is
/// no Dark codec for it: Dark parses scripts itself and builds `Parser.CliScript.PTCliScriptModule`.
module Builtins.CliHost.Utils

open Prelude

module PT = LibExecution.ProgramTypes

module CliScript =
  type Definitions =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn> }

  type PTCliScriptModule =
    { types : List<PT.PackageType.PackageType>
      values : List<PT.PackageValue.PackageValue>
      fns : List<PT.PackageFn.PackageFn>
      traits : List<PT.Trait.Trait>
      impls : List<PT.TraitImpl.TraitImpl>
      submodules : Definitions
      exprs : List<PT.Expr> }
