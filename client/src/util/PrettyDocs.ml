open Prelude
module Regex = Util.Regex

let tagEx = "^(.*)\\<(\\w+)\\s(.+)\\>(.*)$"

let codeEx = "^(.*)\\{\\{(.+)\\}\\}(.*)$"

let codeClass = "code"

let nestedTag = Regex.regex "\\<\\w+\\s[^>]*<\\w+\\s[^<]*\\>.*\\>"

let nestedCodeBlock = Regex.regex "\\{\\{.*\\{\\{.*\\}\\}.*\\}\\}"

let validTags = ["param"; "fn"; "var"; "type"; "return"; "err"; "cmd"]

type parseResult =
  | ParseSuccess of msg Html.html list
  | ParseFail of (string * string) list

(* input:string * errorMsg:string *)

let txt (s : string) : msg Html.html = Html.text s

let tag (cls : string) (content : msg Html.html list) : msg Html.html =
  Html.span [Html.class' cls] content


let justErrors results =
  results
  |> List.filterMap ~f:(fun res ->
         match res with ParseFail err -> Some err | ParseSuccess _ -> None)
  |> List.flatten


let rec convert_ (s : string) : parseResult =
  if s = "" (* Base case *)
  then ParseSuccess []
  else if Regex.contains ~re:nestedTag s
  then ParseFail [(s, "contains nested tags")]
  else if Regex.contains ~re:nestedCodeBlock s
  then ParseFail [(s, "contains nested code blocks")]
  else
    match Regex.captures ~re:(Regex.regex ~flags:"" codeEx) s with
    | [_; before; inside; after] ->
      ( match (convert_ before, convert_ inside, convert_ after) with
      | ( ParseSuccess beforeNodes
        , ParseSuccess insideNodes
        , ParseSuccess afterNodes ) ->
          ParseSuccess (beforeNodes @ (tag codeClass insideNodes :: afterNodes))
      | beforeRes, insideRes, afterRes ->
          let errors = [beforeRes; insideRes; afterRes] |> justErrors in
          ParseFail errors )
    | _ ->
      ( match Regex.captures ~re:(Regex.regex ~flags:"" tagEx) s with
      | [_; before; tagType; tagData; after]
        when List.member ~value:tagType validTags ->
          let tagNode = tag tagType [txt tagData] in
          ( match (convert_ before, convert_ after) with
          | ParseSuccess beforeNodes, ParseSuccess afterNodes ->
              ParseSuccess (beforeNodes @ (tagNode :: afterNodes))
          | beforeRes, afterRes ->
              let errors = [beforeRes; afterRes] |> justErrors in
              ParseFail errors )
      | [_; _; tagType; tagData; _] ->
          ParseFail
            [ ( "<" ^ tagType ^ " " ^ tagData ^ ">"
              , "'" ^ tagType ^ "' is not a valid tag type" ) ]
      | _ ->
          (* Not a rich format just render as plain text *)
          ParseSuccess [txt s] )


let convert (s : string) : msg Html.html list =
  match convert_ s with ParseSuccess code -> code | ParseFail _ -> [txt s]
