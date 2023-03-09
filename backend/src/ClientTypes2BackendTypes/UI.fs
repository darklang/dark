module ClientTypes2BackendTypes.UI

module RT = LibExecution.RuntimeTypes
module CTUI = ClientTypes.UI
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime

module Functions =
  module Param =
    let toCT (p : RT.Param) : CTUI.Functions.Param =
      { name = p.name
        ``type`` = CT2Runtime.DType.toCT p.typ
        args = p.blockArgs
        description = p.description }

  module Previewable =
    let toCT (p : RT.Previewable) : CTUI.Functions.Previewable =
      match p with
      | RT.Pure -> CTUI.Functions.Pure
      | RT.ImpurePreviewable -> CTUI.Functions.ImpurePreviewable
      | RT.Impure -> CTUI.Functions.Impure

  module StdlibFnName =
    let toCT (fnName : RT.FQFnName.StdlibFnName) : CTUI.Functions.StdlibFnName =
      { function_ = fnName.function_
        module_ = fnName.module_
        version = fnName.version }

  module Deprecation =
    let toCT (p : RT.Deprecation) : CTUI.Functions.Deprecation =
      match p with
      | RT.NotDeprecated -> CTUI.Functions.NotDeprecated
      | RT.RenamedTo f -> CTUI.Functions.RenamedTo(StdlibFnName.toCT f)
      | RT.ReplacedBy f -> CTUI.Functions.ReplacedBy(StdlibFnName.toCT f)
      | RT.DeprecatedBecause str -> CTUI.Functions.DeprecatedBecause str

  module SqlSpec =
    let toCT (s : RT.SqlSpec) : CTUI.Functions.SqlSpec =
      match s with
      | RT.NotYetImplemented -> CTUI.Functions.Unknown
      | RT.NotQueryable -> CTUI.Functions.NotQueryable
      | RT.QueryFunction -> CTUI.Functions.QueryFunction
      | RT.SqlUnaryOp str -> CTUI.Functions.SqlUnaryOp str
      | RT.SqlBinOp str -> CTUI.Functions.SqlBinOp str
      | RT.SqlFunction str -> CTUI.Functions.SqlFunction str
      | RT.SqlFunctionWithPrefixArgs (name, args) ->
        CTUI.Functions.SqlFunctionWithPrefixArgs(name, args)
      | RT.SqlFunctionWithSuffixArgs (name, args) ->
        CTUI.Functions.SqlFunctionWithSuffixArgs(name, args)
      | RT.SqlCallback2 _ -> CTUI.Functions.SqlCallback2

  module BuiltInFn =
    let toCT (fn : RT.BuiltInFn) : CTUI.Functions.BuiltInFn =
      { name = StdlibFnName.toCT fn.name
        parameters = List.map Param.toCT fn.parameters
        description = fn.description
        returnType = CT2Runtime.DType.toCT fn.returnType
        previewable = Previewable.toCT fn.previewable
        deprecated = Deprecation.toCT fn.deprecated
        sqlSpec = SqlSpec.toCT fn.sqlSpec }
