open Prelude

(* The parameters passed from appsupport.js. *)

let parameter j : parameter =
  let open Json_decode_extended in
  { paramName = field "name" string j
  ; paramTipe = field "tipe" (string >> Runtime.str2tipe) j
  ; paramBlock_args = field "block_args" (list string) j
  ; paramOptional = field "optional" bool j
  ; paramDescription = field "description" string j }


let function_ j : function_ =
  let open Json_decode_extended in
  { fnName = field "name" string j
  ; fnParameters = field "parameters" (list parameter) j
  ; fnDescription = field "description" string j
  ; fnReturnTipe = field "return_type" (string >> Runtime.str2tipe) j
  ; fnPreviewExecutionSafe = field "preview_execution_safe" bool j
  ; fnDeprecated = field "deprecated" bool j
  ; fnInfix = field "infix" bool j }


type t =
  { complete : Types.function_ list
  ; canvasName : string
  ; userContentHost : string
  ; environment : string
  ; csrfToken : string
  ; isAdmin : bool
  ; buildHash : string
  ; username : string }

let fromString (strJ : string) : t =
  let open Json_decode_extended in
  let j = Json.parseOrRaise strJ in
  { canvasName = field "canvasName" string j
  ; complete = field "complete" (list function_) j
  ; userContentHost = field "userContentHost" string j
  ; environment = field "environment" string j
  ; csrfToken = field "csrfToken" string j
  ; isAdmin = field "isAdmin" bool j
  ; buildHash = field "buildHash" string j
  ; username = field "username" string j }
