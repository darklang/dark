/// The shape `Cli.fs` carries a parsed script in, between parsing it and authoring it.
///
/// It used to carry a `toDT`/`fromDT` pair for it as well, 150 lines of codec with no caller: Dark
/// parses scripts itself and builds its own `Parser.CliScript.PTCliScriptModule`. This branch
/// extended both halves of that codec with trait and impl arms before anyone noticed.
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
