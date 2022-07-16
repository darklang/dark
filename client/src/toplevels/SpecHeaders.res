open Prelude

// Dark
module B = BlankOr
module P = Pointer

let spaceOf = (hs: PT.Handler.Spec.t): handlerSpace => {
  let spaceOfStr = s => {
    let lwr = String.toUppercase(s)
    switch lwr {
    | "HTTP" => HSHTTP
    | "CRON" => HSCron
    | "WORKER" => HSWorker
    | "REPL" => HSRepl
    | _ => HSDeprecatedOther
    }
  }

  switch hs.space {
  | Blank(_) => HSDeprecatedOther
  | F(_, s) => spaceOfStr(s)
  }
}

let replaceEventModifier = (
  search: id,
  replacement: blankOr<string>,
  hs: PT.Handler.Spec.t,
): PT.Handler.Spec.t => {...hs, modifier: B.replace(search, replacement, hs.modifier)}

let replaceEventName = (
  search: id,
  replacement: blankOr<string>,
  hs: PT.Handler.Spec.t,
): PT.Handler.Spec.t => {
  ...hs,
  name: B.replace(search, replacement, hs.name),
}

let replaceEventSpace = (
  search: id,
  replacement: blankOr<string>,
  hs: PT.Handler.Spec.t,
): PT.Handler.Spec.t => {...hs, space: B.replace(search, replacement, hs.space)}

let replace = (
  search: id,
  replacement: blankOr<string>,
  hs: PT.Handler.Spec.t,
): PT.Handler.Spec.t =>
  hs
  |> replaceEventModifier(search, replacement)
  |> replaceEventName(search, replacement)
  |> replaceEventSpace(search, replacement)

let blankOrData = (spec: PT.Handler.Spec.t): list<blankOrData> => list{
  PEventSpace(spec.space),
  PEventModifier(spec.modifier),
  PEventName(spec.name),
}

let firstBlank = (spec: PT.Handler.Spec.t): option<id> =>
  spec |> blankOrData |> List.filter(~f=Pointer.isBlank) |> List.head |> Option.map(~f=Pointer.toID)

let lastBlank = (spec: PT.Handler.Spec.t): option<id> =>
  spec
  |> blankOrData
  |> List.filter(~f=Pointer.isBlank)
  |> List.reverse
  |> List.head
  |> Option.map(~f=Pointer.toID)
