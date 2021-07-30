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
  | ParseFail of (* input * errorMsg *) (string * string) list

let txt (s : string) : msg Html.html = Html.text s

let tag (cls : string) (content : msg Html.html list) : msg Html.html =
  Html.span [Html.class' cls] content


let link (name : string) (url : string) : msg Html.html =
  Html.a [Html.href url; Html.target "_blank"] [Html.text name]


let justErrors results =
  results
  |> List.filterMap ~f:(fun res ->
         match res with ParseFail err -> Some err | ParseSuccess _ -> None)
  |> List.flatten


(** [convert_ s] attempts to parse [s] into html. If it succeeds, it returns the result wrapped in ParseSuccess.
  Otherwise, it returns the input string and the error message as the errors list in ParseFail. *)
let rec convert_ (s : string) : parseResult =
  let tryParseAsCodeBlock (input : string) : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"s" codeEx) input with
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
  let tryParseAsTag input : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"s" tagEx) input with
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
  let tryParseAsLink input : parseResult option =
    match Regex.captures ~re:(Regex.regex ~flags:"s" linkEx) input with
    | [_; before; linkName; linkUrl; after] ->
        Some
          ( match (convert_ before, convert_ after) with
          | ParseSuccess beforeNodes, ParseSuccess afterNodes ->
              let linkNode = link linkName linkUrl in
              ParseSuccess (beforeNodes @ (linkNode :: afterNodes))
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
    tryParseAsCodeBlock s
    |> Option.orElse (tryParseAsLink s)
    |> Option.orElse (tryParseAsTag s)
    (* If it has no richtext markup, just render as plain text: *)
    |> Option.unwrap ~default:(ParseSuccess [txt s])


let convert (s : string) : msg Html.html list =
  match convert_ s with ParseSuccess code -> code | ParseFail _ -> [txt s]
