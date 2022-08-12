open Prelude

// Dark
module B = BlankOr
module P = Pointer

module Spec = PT.Handler.Spec
module CronInterval = Spec.CronInterval

// HACK: the code for headers was really written around the blankOr paradigm. So to
// avoid code changes getting out of control, let's simply convert it back into
// blankOrs, do the operations, and change it back
type triple = (BlankOr.t<string>, BlankOr.t<string>, option<BlankOr.t<string>>)

let spaceOf = (hs: Spec.t): handlerSpace => {
  switch hs {
  | HTTP(_) => HSHTTP
  | HTTPBytes(_) =>
    // HttpBytesHandlerTODO do we need a separate handlerspace?
    HSHTTP
  | Cron(_) => HSCron
  | REPL(_) => HSRepl
  | Worker(_) => HSWorker
  | OldWorker(_)
  | UnknownHandler(_) =>
    HSDeprecatedOther
  }
}

let toBlankOrs = (spec: Spec.t): triple => {
  (Spec.space(spec), Spec.name(spec), Spec.modifier(spec))
}

let fromBlankOrs = ((space, name, mod): triple): Spec.t => {
  let ids: Spec.IDs.t = {
    moduleID: B.toID(space),
    nameID: B.toID(name),
    modifierID: Option.map(~f=B.toID, mod)->Belt.Option.getWithDefault(gid()),
  }
  let name = B.toString(name)
  let mod = B.optionToString(mod)
  switch B.toString(space)->String.toLowercase {
  | "http" => Spec.HTTP(name, mod, ids)
  | "cron" => Spec.Cron(name, mod |> CronInterval.fromString, ids)
  | "repl" => Spec.REPL(name, ids)
  | "worker" => Spec.Worker(name, ids)
  | "" => Spec.UnknownHandler(name, mod, ids)
  | space => Spec.OldWorker(space, name, ids)
  }
}

let replaceEventModifier = (
  search: id,
  replacement: BlankOr.t<string>,
  (space, name, mod): triple,
): triple => {
  (space, name, Option.map(~f=mod => B.replace(search, replacement, mod), mod))
}

let replaceEventName = (
  search: id,
  replacement: BlankOr.t<string>,
  (space, name, mod): triple,
): triple => {
  (space, B.replace(search, replacement, name), mod)
}

let replaceEventSpace = (
  search: id,
  replacement: BlankOr.t<string>,
  (space, name, mod): triple,
): triple => {
  (B.replace(search, replacement, space), name, mod)
}

let replace = (search: id, replacement: BlankOr.t<string>, hs: Spec.t): Spec.t =>
  hs
  |> toBlankOrs
  |> replaceEventModifier(search, replacement)
  |> replaceEventName(search, replacement)
  |> replaceEventSpace(search, replacement)
  |> fromBlankOrs

let blankOrData = (spec: PT.Handler.Spec.t): list<blankOrData> => {
  let (space, name, mod) = toBlankOrs(spec)
  switch mod {
  | Some(mod) => list{PEventSpace(space), PEventModifier(mod), PEventName(name)}
  | None => list{PEventSpace(space), PEventName(name)}
  }
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
