module LibExecutionStdLib.LibNoModule

open Prelude
open System

module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
open LibExecution.RuntimeTypes

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"



let fns : List<BuiltInFn> =
  [ { name = fn "" "toString" 0
      description =
        "Returns a string representation of <param v>, suitable for displaying to a
         user. Redacts passwords."
      parameters = [ Param.make "v" (TVariable "a") "" ]
      returnType = TStr
      fn =
        (function
        | _, [ a ] ->
          a |> DvalReprLegacyExternal.toEnduserReadableTextV0 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "" "toRepr" 0
      parameters = [ Param.make "v" varA "" ]
      returnType = TStr
      description =
        "Returns an adorned string representation of <param v>, suitable for internal
         developer usage. Not designed for sending to end-users, use <fn toString>
         instead.  Redacts passwords."
      fn =
        (function
        | _, [ a ] -> Ply(DStr(DvalReprDeveloper.toRepr a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause "Not intended for external use" }


    { name = fn "" "equals" 0
      parameters = [ Param.make "a" varA ""; Param.make "b" varA "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, [ a; b ] ->
          // TODO: support fn value equality
          (a = b) |> DBool |> Ply
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
        | _, [ a; b ] -> Ply(DBool(not (a = b)))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "" "toForm" 0
      parameters = [ Param.make "obj" (TDict varA) ""; Param.make "submit" TStr "" ]
      returnType = TStr
      description =
        "For demonstration only. Returns a HTML form with the labels and types
         described in <param obj>. <param submit> is the form's action."
      fn =
        (function
        | _, [ DObj o; DStr uri ] ->
          let toInput (k, v) =
            let label = Printf.sprintf "<label for=\"%s\">%s:</label>" k k

            let input =
              Printf.sprintf "<input id=\"%s\" type=\"text\" name=\"%s\">" k k

            label + "\n" + input

          let inputs = o |> Map.toList |> List.map toInput |> String.concat "\n"

          Ply(
            DStr(
              Printf.sprintf
                "<form action=\"%s\" method=\"post\">\n%s\n<input type=\"submit\" value=\"Save\">\n</form>"
                uri
                inputs
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause "It is just a demo function" }


    { name = fn "Error" "toString" 0 // CLEANUP can’t actually call this
      parameters = [ Param.make "err" TError "" ]
      returnType = TStr
      description = "Return a string representing the error"
      fn =
        (function
        | _, [ DError (_, err) ] -> Ply(DStr err)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause "It is no longer allowed to use errors as arguments" }


    { name = fn "AWS" "urlencode" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TStr
      description = "Url encode a string per AWS' requirements"
      fn =
        (function
        | _, [ DStr s ] ->
          // Based on the original OCaml implementation which was slightly modified from
          // https://github.com/mirage/ocaml-cohttp/pull/294/files (to use
          // Buffer.add_string instead of add_bytes); see also
          // https://github.com/mirage/ocaml-uri/issues/65. It's pretty much a straight
          // up port from the Java example at
          // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html,
          // which calls it UriEncode
          let sb = new Text.StringBuilder()

          // Percent encode the path as s3 wants it. Uri doesn't
          // encode $, or the other sep characters in a path.
          // If upstream allows that we can nix this function
          let bytes = UTF8.toBytes s
          let n = Array.length bytes

          let is_hex (ch : byte) =
            (ch >= byte 'A' && ch <= byte 'Z')
            || (ch >= byte 'a' && ch <= byte 'z')
            || (ch >= byte '0' && ch <= byte '9')

          let is_special (ch : byte) =
            ch = byte '_'
            || ch = byte '-'
            || ch = byte '~'
            || ch = byte '.'
            || ch = byte '/'


          for i = 0 to n - 1 do
            let (c : byte) = bytes[i]

            if ((is_hex c) || (is_special c)) then
              sb.Append(char c) |> ignore<Text.StringBuilder>
            elif (bytes[i] = byte '%') then
              // We're expecting already escaped strings so ignore the escapes
              if i + 2 < n then
                if is_hex bytes[i + 1] && is_hex bytes[i + 2] then
                  sb.Append(char c) |> ignore<Text.StringBuilder>
                else
                  sb.Append "%25" |> ignore<Text.StringBuilder>
            else
              sb.Append(c |> char |> int |> sprintf "%%%X")
              |> ignore<Text.StringBuilder>

          sb |> string |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Twitter" "urlencode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Url encode a string per Twitter's requirements"
      fn =
        (function
        | _, [ DStr s ] -> s |> Uri.EscapeDataString |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
