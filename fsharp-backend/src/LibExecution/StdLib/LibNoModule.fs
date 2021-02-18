module LibExecution.StdLib.LibNoModule

open Prelude

module DvalRepr = LibExecution.DvalRepr
open LibExecution.RuntimeTypes

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"



let fns : List<BuiltInFn> =
  [ { name = FQFnName.stdlibName "" "toString" 0
      description =
        "Returns a string representation of `v`, suitable for displaying to a user. Redacts passwords."
      parameters = [ Param.make "a" (TVariable "a") "" ]
      returnType = TStr
      fn =
        (function
        | _, [ a ] -> a |> DvalRepr.toEnduserReadableTextV0 |> DStr |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    // ; { name = fn "" "toRepr" 0
//   ; parameters = [Param.make "v" TAny ""]
//   ; returnType = TStr
//   ; description =
//       "Returns an adorned string representation of `v`, suitable for internal developer usage. Not designed for sending to end-users, use toString instead. Redacts passwords."
//   ; fn =
//         (function
//         | _, [a] ->
//             DStr (Dval.to_developer_repr_v0 a)
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "" "equals" 0
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, [ a; b ] -> (Value(DBool(a = b))) //FSTODO: use equal_dval
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "="
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "" "notEquals" 0
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, [ a; b ] -> Value(DBool(not (a = b)))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated } ]
// ; { name = fn "" "assoc" 0
//   ; parameters = [Param.make "obj" TObj ""; Param.make "key" TStr ""; Param.make "val" TAny ""]
//   ; returnType = TObj
//   ; description = "Return a copy of `obj` with the `key` set to `val`."
//   ; fn =
//         (function
//         | _, [DObj o; DStr k; v] ->
//             DObj (Map.set o (Unicode_string.to_string k) v)
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "" "dissoc" 0
//   ; parameters = [Param.make "obj" TObj ""; Param.make "key" TStr ""]
//   ; returnType = TObj
//   ; description = "Return a copy of `obj` with `key` unset."
//   ; fn =
//         (function
//         | _, [DObj o; DStr k] ->
//             DObj (Map.remove o (Unicode_string.to_string k))
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "" "toForm" 0
//   ; parameters = [Param.make "obj" TObj ""; Param.make "submit" TStr ""]
//   ; returnType = TStr
//   ; description =
//       "For demonstration only. Returns a HTML form with the labels and types described in `obj`. `submit` is the form's action."
//   ; fn =
//         (function
//         | _, [DObj o; DStr uri] ->
//             let fmt =
//               format_of_string
//                 "<form action=\"%s\" method=\"post\">\n%s\n<input type=\"submit\" value=\"Save\">\n</form>"
//             in
//             let to_input (k, v) =
//               let label =
//                 Printf.sprintf "<label for=\"%s\">%s:</label>" k k
//               in
//               let input =
//                 Printf.sprintf
//                   "<input id=\"%s\" type=\"text\" name=\"%s\">"
//                   k
//                   k
//               in
//               label ^ "\n" ^ input
//             in
//             let inputs =
//               o
//               |> Map.to_alist
//               |> List.map to_input
//               |> String.concat "\n"
//             in
//             DStr
//               (Printf.sprintf fmt (Unicode_string.to_string uri) inputs)
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "Error" "toString" 0
//   ; parameters = [Param.make "err" TError ""]
//   ; returnType = TStr
//   ; description = "Return a string representing the error"
//   ; fn =
//         (function
//         | _, [DError (_, err)] ->
//             DStr err
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "AWS" "urlencode" 0
//   ; parameters = [Param.make "str" TStr ""]
//   ; returnType = TStr
//   ; description = "Url encode a string per AWS' requirements"
//   ; fn =
//         (function
//         | _, [DStr str] ->
//             str
//             |> Unicode_string.to_string
//             |> Stdlib_util.AWS.url_encode
//             |> DStr
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "Twitter" "urlencode" 0
//   ; parameters = [Param.make "s" TStr ""]
//   ; returnType = TStr
//   ; description = "Url encode a string per Twitter's requirements"
//   ; fn =
//         (function
//         | _, [DStr s] ->
//             s
//             |> Unicode_string.to_string
//             |> Uri.pct_encode `Userinfo
//             |> DStr
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
// ; previewable = Pure
//   ; deprecated = NotDeprecated }
