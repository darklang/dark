open Prelude

// Dark
module B = BlankOr
module P = Pointer

let spaceOf = (hs: handlerSpec): handlerSpace => {
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
  search: ID.t,
  replacement: blankOr<string>,
  hs: handlerSpec,
): handlerSpec => {...hs, modifier: B.replace(search, replacement, hs.modifier)}

let replaceEventName = (
  search: ID.t,
  replacement: blankOr<string>,
  hs: handlerSpec,
): handlerSpec => {...hs, name: B.replace(search, replacement, hs.name)}

let replaceEventSpace = (
  search: ID.t,
  replacement: blankOr<string>,
  hs: handlerSpec,
): handlerSpec => {...hs, space: B.replace(search, replacement, hs.space)}

let replace = (search: ID.t, replacement: blankOr<string>, hs: handlerSpec): handlerSpec =>
  hs
  |> replaceEventModifier(search, replacement)
  |> replaceEventName(search, replacement)
  |> replaceEventSpace(search, replacement)

let blankOrData = (spec: handlerSpec): list<blankOrData> => list{
  PEventSpace(spec.space),
  PEventModifier(spec.modifier),
  PEventName(spec.name),
}

let firstBlank = (spec: handlerSpec): option<ID.t> =>
  spec |> blankOrData |> List.filter(~f=Pointer.isBlank) |> List.head |> Option.map(~f=Pointer.toID)

let lastBlank = (spec: handlerSpec): option<ID.t> =>
  spec
  |> blankOrData
  |> List.filter(~f=Pointer.isBlank)
  |> List.reverse
  |> List.head
  |> Option.map(~f=Pointer.toID)
