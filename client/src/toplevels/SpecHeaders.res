open Prelude

// Dark
module B = BlankOr
module P = Pointer

module Spec = PT.Handler.Spec
module CronInterval = Spec.CronInterval

// HACK: the code for headers was really written around the blankOr paradigm. So to
// avoid code changes getting out of control, let's simply convert it back into
// blankOrs, do the operations, and change it back
type triple = (BlankOr.t<string>, BlankOr.t<string>, BlankOr.t<string>)

let spaceOf = (hs: Spec.t): handlerSpace => {
  switch hs {
  | HTTP(_) => HSHTTP
  | Cron(_) => HSCron
  | REPL(_) => HSRepl
  | Worker(_) => HSWorker
  | OldWorker(_)
  | UnknownHandler(_) =>
    HSDeprecatedOther
  }
}

let toBlankOrs = (spec: Spec.t): triple => {
  let ids = Spec.ids(spec)
  (
    Spec.space(spec)->B.fromOptionID(ids.moduleID),
    Spec.name(spec)->B.fromStringID(ids.nameID),
    Spec.modifier(spec)->B.fromOptionID(ids.modifierID),
  )
}

let fromBlankOrs = ((space, name, mod): triple): Spec.t => {
  let ids: Spec.IDs.t = {moduleID: B.toID(space), nameID: B.toID(name), modifierID: B.toID(mod)}
  switch B.toString(space)->String.toLowercase {
  | "http" => Spec.HTTP(B.toString(name), B.toString(mod), ids)
  | "cron" => Spec.Cron(B.toString(name), B.toString(mod) |> CronInterval.fromString, ids)
  | "repl" => Spec.REPL(B.toString(name), ids)
  | "worker" => Spec.Worker(B.toString(name), ids)
  | "" => Spec.UnknownHandler(B.toString(name), B.toString(mod), ids)
  | space => Spec.OldWorker(space, B.toString(name), ids)
  }
}

let replaceEventModifier = (
  search: id,
  replacement: BlankOr.t<string>,
  (space, name, mod): triple,
): triple => {
  (space, name, B.replace(search, replacement, mod))
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
  list{PEventSpace(space), PEventModifier(mod), PEventName(name)}
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
