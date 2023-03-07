/// Types corresponding to the JSON injected from UI.fs into ui.html of `client`
module ClientTypes.UI

module Functions =
  type Param =
    { name : string
      ``type`` : Runtime.DType
      args : List<string>
      description : string }

  type Previewable =
    | Pure
    | ImpurePreviewable
    | Impure

  type StdlibFnName = { module_ : string; function_ : string; version : int }

  type Deprecation =
    | NotDeprecated
    | RenamedTo of StdlibFnName
    | ReplacedBy of StdlibFnName
    | DeprecatedBecause of string

  type SqlSpec =
    | Unknown
    | NotQueryable
    | QueryFunction
    | SqlUnaryOp of string
    | SqlBinOp of string
    | SqlFunction of string
    | SqlFunctionWithPrefixArgs of string * List<string>
    | SqlFunctionWithSuffixArgs of string * List<string>
    | SqlCallback2

  type BuiltInFn =
    { name : StdlibFnName
      parameters : List<Param>
      returnType : Runtime.DType
      description : string
      previewable : Previewable
      deprecated : Deprecation
      sqlSpec : SqlSpec }
