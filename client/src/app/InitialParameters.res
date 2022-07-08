open Prelude

// The parameters passed from appsupport.js.

let parameter = (j): parameter => {
  open Json.Decode
  {
    paramName: field("name", string, j),
    paramTipe: field("tipe", \">>"(string, Runtime.str2tipe), j),
    paramBlock_args: field("block_args", list(string), j),
    paramOptional: field("optional", bool, j),
    paramDescription: field("description", string, j),
  }
}

let function_ = (j): function_ => {
  open Json.Decode
  let previewSafetyVariants = variants(list{("Safe", variant0(Safe)), ("Unsafe", variant0(Unsafe))})

  {
    fnName: field("name", string, j),
    fnParameters: field("parameters", list(parameter), j),
    fnDescription: field("description", string, j),
    fnReturnTipe: field("return_type", \">>"(string, Runtime.str2tipe), j),
    fnPreviewSafety: field("preview_safety", previewSafetyVariants, j),
    fnDeprecated: field("deprecated", bool, j),
    fnInfix: field("infix", bool, j),
    fnIsSupportedInQuery: field("is_supported_in_query", bool, j),
    fnOrigin: Builtin,
  }
}

type t = {
  complete: list<Types.function_>,
  canvasName: string,
  userContentHost: string,
  environment: string,
  csrfToken: string,
  isAdmin: bool,
  buildHash: string,
  username: string,
}

let fromString = (strJ: string): t => {
  open Json.Decode
  let j = Json.parseOrRaise(strJ)
  {
    canvasName: field("canvasName", string, j),
    complete: field("complete", list(function_), j),
    userContentHost: field("userContentHost", string, j),
    environment: field("environment", string, j),
    csrfToken: field("csrfToken", string, j),
    isAdmin: field("isAdmin", bool, j),
    buildHash: field("buildHash", string, j),
    username: field("username", string, j),
  }
}
