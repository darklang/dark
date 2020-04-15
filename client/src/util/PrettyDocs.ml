open Prelude
module Regex = Util.Regex

let tagEx = "^(.*)\\<(\\w+)\\s(.+)\\>(.*)$"

let codeEx = "^(.*)\\{\\{(.+)\\}\\}(.*)$"

let linkEx = "^(.*)\\[(.+)\\]\\((http[s]?\\://.+)\\)(.*)$"

let codeClass = "code"

let nestedTag = Regex.regex {|<\w+[^>]*<|}

let nestedCodeBlock = Regex.regex {|{{[^}]+{{|}

let validTags = ["param"; "fn"; "var"; "type"; "err"; "cmd"]

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
  let hasCodeBlock (input : string) : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"" codeEx) input with
    | [_; before; inside; after] ->
        Some
          ( match (convert_ before, convert_ inside, convert_ after) with
          | ( ParseSuccess beforeNodes
            , ParseSuccess insideNodes
            , ParseSuccess afterNodes ) ->
              ParseSuccess
                (beforeNodes @ (tag codeClass insideNodes :: afterNodes))
          | beforeRes, insideRes, afterRes ->
              let errors = [beforeRes; insideRes; afterRes] |> justErrors in
              ParseFail errors )
    | _ ->
        None
  in
  let hasTag input : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"" tagEx) input with
    | [_; before; tagType; tagData; after]
      when List.member ~value:tagType validTags ->
        let tagNode = tag tagType [txt tagData] in
        Some
          ( match (convert_ before, convert_ after) with
          | ParseSuccess beforeNodes, ParseSuccess afterNodes ->
              ParseSuccess (beforeNodes @ (tagNode :: afterNodes))
          | beforeRes, afterRes ->
              let errors = [beforeRes; afterRes] |> justErrors in
              ParseFail errors )
    | [_; _; tagType; tagData; _] ->
        Some
          (ParseFail
             [ ( "<" ^ tagType ^ " " ^ tagData ^ ">"
               , "'" ^ tagType ^ "' is not a valid tag type" ) ])
    | _ ->
        None
  in
  let hasLink input : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"" linkEx) input with
    | [_; before; linkName; linkUrl; after] ->
        Some
          ( match (convert_ before, convert_ after) with
          | ParseSuccess beforeNodes, ParseSuccess afterNodes ->
              let link =
                Html.a
                  [Html.href linkUrl; Html.target "_blank"]
                  [Html.text linkName]
              in
              ParseSuccess (beforeNodes @ (link :: afterNodes))
          | beforeRes, afterRes ->
              let errors = [beforeRes; afterRes] |> justErrors in
              ParseFail errors )
    | _ ->
        None
  in
  if s = "" (* Base case *)
  then ParseSuccess []
  else if Regex.contains ~re:nestedTag s
  then ParseFail [(s, "contains nested tags")]
  else if Regex.contains ~re:nestedCodeBlock s
  then ParseFail [(s, "contains nested code blocks")]
  else
    hasCodeBlock s
    |> Option.orElse (hasLink s)
    |> Option.orElse (hasTag s)
    (* If it has no richtext markup, just render as plain text: *)
    |> Option.withDefault ~default:(ParseSuccess [txt s])


let convert (s : string) : msg Html.html list =
  match convert_ s with ParseSuccess code -> code | ParseFail _ -> [txt s]
