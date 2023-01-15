open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg
type msg = AppTypes.msg

let tagEx = "^(.*)\\<(\\w+)\\s(.+?)\\>(.*)$"

let codeEx = "^(.*)\\{\\{(.+)\\}\\}(.*)$"

let linkEx = "^(.*)\\[(.+)\\]\\((http[s]?\\://.+)\\)(.*)$"

let nestedTag = Regex.regex(`<\\\w+[^>]*<`)

let nestedCodeBlock = Regex.regex(`{{[^}]+{{`)

let typ = %twc("text-green")
let param = %twc("text-purple1")
let fn = %twc("text-purple1")
let var = %twc("text-purple1")
let err = %twc("text-red pb-3.5")
let cmd = %twc("text-pink whitespace-nowrap")
let code = %twc("bg-grey1 py-0 px-1.5")
let default = %twc("text-white1")

let validTags = list{"param", "fn", "var", "type", "err", "cmd"}

type parseResult =
  | ParseSuccess(list<Html.html<msg>>)
  | ParseFail(/* input * errorMsg */ list<(string, string)>)

let txt = (s: string): Html.html<msg> => Html.text(s)

let tag = (cls: string, content: list<Html.html<msg>>): Html.html<msg> =>
  Html.span(list{Attrs.class(cls)}, content)

let link = (name: string, url: string): Html.html<msg> =>
  Html.a(list{Attrs.href(url), Attrs.target("_blank")}, list{Html.text(name)})

let justErrors = results =>
  results
  |> List.filterMap(~f=res =>
    switch res {
    | ParseFail(err) => Some(err)
    | ParseSuccess(_) => None
    }
  )
  |> List.flatten

@ocaml.doc(" [convert_ s] attempts to parse [s] into html. If it succeeds, it returns the result wrapped in ParseSuccess.
  Otherwise, it returns the input string and the error message as the errors list in ParseFail. ")
let rec convert_ = (s: string): parseResult => {
  let tryParseAsCodeBlock = (input: string): option<parseResult> =>
    switch Regex.captures(~re=Regex.regex(~flags="s", codeEx), input) {
    | list{_, before, inside, after} =>
      Some(
        switch (convert_(before), convert_(inside), convert_(after)) {
        | (ParseSuccess(beforeNodes), ParseSuccess(insideNodes), ParseSuccess(afterNodes)) =>
          ParseSuccess(
            Belt.List.concatMany([beforeNodes, list{tag(code, insideNodes)}, afterNodes]),
          )
        | (beforeRes, insideRes, afterRes) =>
          let errors = list{beforeRes, insideRes, afterRes} |> justErrors
          ParseFail(errors)
        },
      )
    | _ => None
    }

  let tryParseAsTag = (input): option<parseResult> =>
    switch Regex.captures(~re=Regex.regex(~flags="s", tagEx), input) {
    | list{_, before, tagType, tagData, after} if List.member(~value=tagType, validTags) =>
      let tagNode = switch tagType {
      | "type" => tag(typ, list{txt(tagData)})
      | "param" => tag(param, list{txt(tagData)})
      | "fn" => tag(fn, list{txt(tagData)})
      | "var" => tag(var, list{txt(tagData)})
      | "cmd" => tag(cmd, list{txt(tagData)})
      | _ => tag(default, list{txt(tagData)})
      }
      Some(
        switch (convert_(before), convert_(after)) {
        | (ParseSuccess(beforeNodes), ParseSuccess(afterNodes)) =>
          ParseSuccess(Belt.List.concat(beforeNodes, list{tagNode, ...afterNodes}))
        | (beforeRes, afterRes) =>
          let errors = list{beforeRes, afterRes} |> justErrors
          ParseFail(errors)
        },
      )
    | list{_, _, tagType, tagData, _} =>
      Some(
        ParseFail(list{
          (
            "<" ++ (tagType ++ (" " ++ (tagData ++ ">"))),
            "'" ++ (tagType ++ "' is not a valid tag type"),
          ),
        }),
      )
    | _ => None
    }

  let tryParseAsLink = (input): option<parseResult> =>
    switch Regex.captures(~re=Regex.regex(~flags="s", linkEx), input) {
    | list{_, before, linkName, linkUrl, after} =>
      Some(
        switch (convert_(before), convert_(after)) {
        | (ParseSuccess(beforeNodes), ParseSuccess(afterNodes)) =>
          let linkNode = link(linkName, linkUrl)
          ParseSuccess(Belt.List.concat(beforeNodes, list{linkNode, ...afterNodes}))
        | (beforeRes, afterRes) =>
          let errors = list{beforeRes, afterRes} |> justErrors
          ParseFail(errors)
        },
      )
    | _ => None
    }

  if s == "" /* Base case */ {
    ParseSuccess(list{})
  } else if Regex.contains(~re=nestedTag, s) {
    ParseFail(list{(s, "contains nested tags")})
  } else if Regex.contains(~re=nestedCodeBlock, s) {
    ParseFail(list{(s, "contains nested code blocks")})
  } else {
    tryParseAsCodeBlock(s)
    |> Option.orElse(tryParseAsLink(s))
    |> Option.orElse(tryParseAsTag(s))
    |> // If it has no richtext markup, just render as plain text:
    Option.unwrap(~default=ParseSuccess(list{txt(s)}))
  }
}

let convert = (s: string): list<Html.html<msg>> =>
  switch convert_(s) {
  | ParseSuccess(code) => code
  | ParseFail(_) => list{txt(s)}
  }
