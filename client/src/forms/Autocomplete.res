open Prelude

// Dark
module P = Pointer
module RT = Runtime
module TL = Toplevel
module B = BlankOr
module Regex = Util.Regex
module TD = TLID.Dict

// ----------------------------
// Focus
// ----------------------------
// show the prev 5
// obvi this should use getClientBoundingBox, but that's tough in Elm
let height = (i: int): int =>
  if i < 4 {
    0
  } else {
    14 * (i - 4)
  }

let focusItem = (i: int): Tea.Cmd.t<msg> =>
  Tea_task.attempt(
    _ => IgnoreMsg("autocomplete-focus"),
    Tea_task.nativeBinding(_ => {
      open Webapi.Dom
      switch Document.getElementById("autocomplete-holder", document) {
      | Some(el) => Element.setScrollTop(el, i |> height |> float_of_int)
      | None => ()
      }
    }),
  )

// ----------------------------
// display
// ----------------------------
let asName = (aci: autocompleteItem): string =>
  switch aci {
  | ACOmniAction(ac) =>
    switch ac {
    | NewDB(maybeName) =>
      switch maybeName {
      | Some(name) => "New DB named " ++ name
      | None => "New DB"
      }
    | NewWorkerHandler(maybeName) =>
      switch maybeName {
      | Some(name) => "New Worker named " ++ name
      | None => "New Worker "
      }
    | NewFunction(maybeName) =>
      switch maybeName {
      | Some(name) => "New Function named " ++ name
      | None => "New Function"
      }
    | NewHTTPHandler(maybeName) =>
      switch maybeName {
      | Some(name) => "New HTTP handler named " ++ name
      | None => "New HTTP handler"
      }
    | NewCronHandler(maybeName) =>
      switch maybeName {
      | Some(name) => "New Cron named " ++ name
      | None => "New Cron"
      }
    | NewReplHandler(maybeName) =>
      switch maybeName {
      | Some(name) => "New REPL named " ++ name
      | None => "New REPL"
      }
    | Goto(_, _, desc, _) => desc
    }
  | ACHTTPModifier(name) => name
  | ACHTTPRoute(name) => name
  | ACWorkerName(name) => name
  | ACReplName(name) => name
  | ACCronName(name) => name
  | ACCronTiming(timing) => timing
  | ACEventSpace(space) => space
  | ACDBColType(tipe) => tipe
  | ACParamTipe(tipe) => RT.tipe2str(tipe)
  | ACDBName(name) => name
  | ACDBColName(name)
  | ACEventModifier(name)
  | ACFnName(name)
  | ACParamName(name)
  | ACTypeName(name)
  | ACTypeFieldName(name) => name
  | ACReturnTipe(tipe) | ACTypeFieldTipe(tipe) => RT.tipe2str(tipe)
  }

let asTypeString = (item: autocompleteItem): string =>
  switch item {
  | ACOmniAction(_) => ""
  | ACHTTPModifier(_) => "method"
  | ACHTTPRoute(_) => "route"
  | ACWorkerName(_) => "worker name"
  | ACReplName(_) => "REPL name"
  | ACCronName(_) => "cron job"
  | ACCronTiming(_) => "interval"
  | ACEventSpace(_) => "event space"
  | ACDBColType(_) => "type"
  | ACParamTipe(_) => "param type"
  | ACDBName(_) => "name"
  | ACDBColName(_) => "column name"
  | ACEventModifier(_) => "event modifier"
  | ACFnName(_) => "function name"
  | ACParamName(_) => "param name"
  | ACTypeName(_) => "type name"
  | ACTypeFieldName(_) => "type field name"
  | ACReturnTipe(tipe) | ACTypeFieldTipe(tipe) =>
    switch tipe {
    | TUserType(_, v) => "version " ++ string_of_int(v)
    | _ => "builtin"
    }
  }

let asTypeClass = (item: autocompleteItem): string =>
  switch item {
  | ACOmniAction(NewDB(_))
  | ACOmniAction(NewFunction(_))
  | ACOmniAction(NewHTTPHandler(_))
  | ACOmniAction(NewWorkerHandler(_))
  | ACOmniAction(NewCronHandler(_))
  | ACOmniAction(NewReplHandler(_))
  | ACOmniAction(Goto(_, _, _, true)) => "found-in"
  | ACOmniAction(Goto(_, _, _, false)) => "jump-to"
  | _ => ""
  }

let asString = (aci: autocompleteItem): string => asName(aci) ++ asTypeString(aci)

// ----------------------------
// External: utils
// ----------------------------

// Return different type if possible
let highlighted = (a: autocomplete): option<autocompleteItem> =>
  List.getAt(~index=a.index, a.completions)

let getValue = (a: autocomplete): string =>
  switch highlighted(a) {
  | Some(item) => asName(item)
  | None => a.value
  }

let rec containsOrdered = (needle: string, haystack: string): bool =>
  switch String.uncons(needle) {
  | Some(c, newneedle) =>
    let char = String.fromChar(c)
    String.includes(~substring=char, haystack) &&
    containsOrdered(
      newneedle,
      haystack |> String.split(~on=char) |> List.drop(~count=1) |> String.join(~sep=char),
    )
  | None => true
  }

// ------------------------------------
// Validators
// ------------------------------------

/*
  urls
  From https://www.w3.org/Addressing/URL/5_URI_BNF.html
  path = void | xpalphas [ / path ]
  xalpha = alpha | digit | safe | extra | escape
  xalphas = xalpha [ xalphas ]
  xpalpha = xalpha | +
  xpalphas = xpalpha [ xpalpha ]
  alpha = [a-zA-Z]
  digit = [0-9]
  safe = $ | - | _ | @ | . | &
  extra = ! | * | <doublequote> | ' | ( | ) | ,
  reserved = = | ; | / | # | ? | : | space
  escape = % hex hex
*/
// let urlPathSafeCharacters = "[-a-zA-Z0-9$_@.&!*\"'(),%/]"
// let nonUrlPathSafeCharacters = "[^-a-zA-Z0-9$_@.&!*\"'(),%/]"
// let urlPathValidator = "[-a-zA-Z0-9$_@.&!*\"'(),%/]+"

// allow : for parameter names. TODO: do better job parsing here

let nonEventNameSafeCharacters = "[^-a-zA-Z0-9$_@.&!*\"'(),%/:]"

let httpNameValidator = "/[-a-zA-Z0-9$_@.&!*\"'(),%/:]*"

let eventNameValidator = "[-a-zA-Z0-9$_@.&!*\'(),%/:]+"

let varnameValidator = "[a-z_][a-zA-Z0-9_]*"

let dbColTypeValidator = "\\[?[A-Z]\\w+\\]?"

let dbColNameValidator = "\\w+"

let dbNameValidator = "[A-Z][a-zA-Z0-9_]*"

let eventModifierValidator = "[a-zA-Z_][\\sa-zA-Z0-9_]*"

let httpVerbValidator = "[A-Z]+"

let eventSpaceValidator = "(CRON|HTTP|REPL|WORKER)"

let cronValidatorPattern = "(Daily|Weekly|Fortnightly|Every 1hr|Every 12hrs|Every 1min)"

let fieldNameValidator = ".+"

let fnNameValidator = "[a-z][a-zA-Z0-9_]*"

let packageFnNameValidator = "[a-z][a-zA-Z0-9/:_]*"

/* NB: disallowing inital-capitals also defends against having a collision
 * between a function param name and a db name */
let paramNameValidator = "[a-z][a-zA-Z0-9_]*"

let typeNameValidator = dbNameValidator

let paramTypeValidator = "[A-Za-z0-9_]*"

// Adding a more clear error message for invalid cron intervals
let cronIntervalValidator = (name: string): option<string> =>
  if Regex.exactly(~re=cronValidatorPattern, name) {
    None
  } else {
    Some(name ++ " is an invalid CRON interval")
  }

let assertValid = (pattern, value): string => {
  asserT("Failed validator", Regex.exactly(~re=pattern, value), ~debug=(pattern, value))
  value
}

let validateFunctionName = (fnName: string): option<string> => {
  let pattern = fnNameValidator
  if Regex.exactly(~re=pattern, fnName) {
    None
  } else {
    Some("Function name must match /" ++ (pattern ++ "/"))
  }
}

let validateHttpNameValidVarnames = (httpName: string) => {
  let route_variables = (route: string): list<string> =>
    route
    |> String.split(~on="/")
    |> List.filter(~f=x => String.length(x) > 0)
    |> List.filter(~f=x => String.startsWith(~prefix=":", x))
    |> List.map(~f=x => String.dropLeft(~count=1, x))

  if route_variables(httpName) |> List.all(~f=v => Regex.exactly(~re=varnameValidator, v)) {
    None
  } else {
    Some("route variables must match /" ++ (varnameValidator ++ "/"))
  }
}

let validateFnParamNameFree = (tl: toplevel, oldParam: blankOr<string>, value: string): option<
  string,
> =>
  switch tl {
  | TLFunc(fn) =>
    let params = UserFunctions.allParamData(fn) |> List.filterMap(~f=p =>
      switch p {
      | PParamName(pd) if pd == oldParam => None
      | PParamName(F(_, n)) => Some(n)
      | _ => None
      }
    )

    if List.member(~value, params) {
      Some("`" ++ (value ++ "` is already declared. Use another name."))
    } else {
      None
    }
  | _ => None
  }

// ------------------------------------
// Omniactions
// ------------------------------------

let rec stripCharsFromFront = (disallowed: string, s: string): string =>
  switch String.uncons(s) {
  | None => s
  | Some(c, rest) =>
    let needle = String.fromChar(c)
    if Regex.contains(~re=Regex.regex(disallowed), needle) {
      stripCharsFromFront(disallowed, rest)
    } else {
      s
    }
  }

let stripChars = (disallowed: string, s: string): string =>
  Regex.replace(~re=Regex.regex(disallowed), ~repl="", s)

let removeExtraSlashes = (s: string): string => {
  let s = Regex.replace(~re=Regex.regex("/+"), ~repl="/", s)
  let s = if s != "/" && String.endsWith(~suffix="/", s) {
    String.dropRight(~count=1, s)
  } else {
    s
  }

  s
}

let cleanEventName = (s: string): string =>
  s |> stripChars(nonEventNameSafeCharacters) |> removeExtraSlashes

let cleanHTTPname = (s: string): string =>
  "/" ++ s |> stripChars(nonEventNameSafeCharacters) |> removeExtraSlashes

let cleanDBName = (s: string): string =>
  s |> stripChars("[^a-zA-Z0-9_]") |> stripCharsFromFront("[^a-zA-Z]") |> String.capitalize

let qNewDB = (s: string): omniAction => {
  let name = cleanDBName(s)
  if name == "" {
    NewDB(None)
  } else {
    NewDB(Some(assertValid(dbNameValidator, name)))
  }
}

let qFunction = (s: string): omniAction => {
  let name =
    s |> stripChars("[^a-zA-Z0-9_]") |> stripCharsFromFront("[^a-zA-Z]") |> String.uncapitalize

  if name == "" {
    NewFunction(None)
  } else {
    NewFunction(Some(assertValid(fnNameValidator, name)))
  }
}

let qWorkerHandler = (s: string): omniAction => {
  let name = s |> cleanEventName |> String.uncapitalize
  if name == "" {
    NewWorkerHandler(None)
  } else {
    NewWorkerHandler(Some(assertValid(eventNameValidator, name)))
  }
}

let qCronHandler = (s: string): omniAction => {
  let name = s |> cleanEventName |> String.uncapitalize
  if name == "" {
    NewCronHandler(None)
  } else {
    NewCronHandler(Some(assertValid(eventNameValidator, name)))
  }
}

let qReplHandler = (s: string): omniAction => {
  let name = s |> cleanEventName |> String.uncapitalize
  if name == "" {
    NewReplHandler(None)
  } else {
    NewReplHandler(Some(assertValid(eventNameValidator, name)))
  }
}

let qHTTPHandler = (s: string): omniAction => {
  let name = cleanEventName(s)
  if name == "" {
    NewHTTPHandler(None)
  } else if String.startsWith(~prefix="/", name) {
    NewHTTPHandler(Some(assertValid(httpNameValidator, name)))
  } else {
    NewHTTPHandler(Some(assertValid(httpNameValidator, "/" ++ name)))
  }
}

let handlerDisplayName = (h: handler): string => {
  let space =
    h.spec.space |> B.toOption |> Option.map(~f=x => x ++ "::") |> Option.unwrap(~default="")

  let name = h.spec.name |> B.toOption |> Option.unwrap(~default="")
  let modi =
    h.spec.modifier
    |> B.toOption
    |> Option.map(~f=x =>
      if x == "_" {
        ""
      } else {
        " - " ++ x
      }
    )
    |> Option.unwrap(~default="")

  space ++ (name ++ modi)
}

let fnDisplayName = (f: userFunction): string =>
  f.ufMetadata.ufmName |> B.toOption |> Option.unwrap(~default="undefinedFunction")

let foundHandlerOmniAction = (h: handler): omniAction => {
  let name = "Found in " ++ handlerDisplayName(h)
  Goto(FocusedHandler(h.hTLID, None, true), h.hTLID, name, true)
}

let foundFnOmniAction = (f: userFunction): omniAction => {
  let name = "Found in function " ++ fnDisplayName(f)
  Goto(FocusedFn(f.ufTLID, None), f.ufTLID, name, true)
}

let qSearch = (m: model, s: string): list<omniAction> =>
  if String.length(s) > 3 {
    let maxResults = 20
    let results = Map.toList(m.searchCache) |> List.filterMap(~f=((tlid, code)) =>
      if String.includes(~substring=s, code) {
        Map.get(~key=tlid, m.handlers)
        |> Option.map(~f=foundHandlerOmniAction)
        |> Option.orElse(Map.get(~key=tlid, m.userFunctions) |> Option.map(~f=foundFnOmniAction))
      } else {
        None
      }
    )

    if List.length(results) > maxResults {
      List.take(~count=maxResults, results)
    } else {
      results
    }
  } else {
    list{}
  }

let isDynamicItem = (item: autocompleteItem): bool =>
  switch item {
  | ACOmniAction(Goto(_, _, _, dyna)) => dyna
  | ACOmniAction(_) => true
  | ACEventSpace(_) => false // false because we want the static items to be first
  | ACHTTPRoute(_) => false
  | ACWorkerName(_) => true
  | ACDBName(_) => true
  | _ => false
  }

let isStaticItem = (item: autocompleteItem): bool => !isDynamicItem(item)

let toDynamicItems = (
  m: model,
  space: option<handlerSpace>,
  target: option<target>,
  q: string,
): list<autocompleteItem> =>
  switch target {
  | None =>
    // omnicompletion
    let standard = list{
      qHTTPHandler(q),
      qNewDB(q),
      qFunction(q),
      qWorkerHandler(q),
      qCronHandler(q),
      qReplHandler(q),
      ...qSearch(m, q),
    }

    List.map(~f=o => ACOmniAction(o), standard)
  | Some(_, PEventName(_)) =>
    switch space {
    | Some(HSHTTP) => list{ACHTTPRoute(cleanHTTPname(q))}
    | Some(HSCron) =>
      if q == "" {
        list{}
      } else {
        list{ACCronName(cleanEventName(q))}
      }
    | Some(HSRepl) =>
      if q == "" {
        list{}
      } else {
        list{ACReplName(cleanEventName(q))}
      }
    | _ =>
      if q == "" {
        list{}
      } else {
        list{ACWorkerName(cleanEventName(q))}
      }
    }
  | Some(_, PDBName(_)) =>
    if q === "" {
      list{}
    } else {
      list{ACDBName(cleanDBName(q))}
    }
  | _ => list{}
  }

let withDynamicItems = (
  m: model,
  target: option<target>,
  query: string,
  acis: list<autocompleteItem>,
): list<autocompleteItem> => {
  let space =
    target
    |> Option.map(~f=Tuple2.first)
    |> Option.andThen(~f=TL.get(m))
    |> Option.andThen(~f=TL.spaceOf)

  let new_ = toDynamicItems(m, space, target, query)
  let withoutDynamic = List.filter(~f=isStaticItem, acis)
  List.uniqueBy(~f=asName, Belt.List.concat(new_, withoutDynamic))
}

let tlGotoName = (tl: toplevel): string =>
  switch tl {
  | TLHandler(h) => "Jump to handler: " ++ handlerDisplayName(h)
  | TLDB(db) => "Jump to DB: " ++ (db.dbName |> B.toOption |> Option.unwrap(~default="Unnamed DB"))
  | TLPmFunc(_) | TLFunc(_) => recover("can't goto function", ~debug=tl, "<invalid state>")
  | TLTipe(_) => recover("can't goto tipe ", ~debug=tl, "<invalid state>")
  }

let tlDestinations = (m: model): list<autocompleteItem> => {
  let tls =
    m
    |> TL.structural
    |> Map.values
    |> List.sortBy(~f=tlGotoName)
    |> List.map(~f=tl => Goto(TL.asPage(tl, true), TL.id(tl), tlGotoName(tl), false))

  let ufs = m.userFunctions |> Map.filterMapValues(~f=fn => {
    let name = "Jump to function: " ++ fnDisplayName(fn)
    Some(Goto(FocusedFn(fn.ufTLID, None), fn.ufTLID, name, false))
  })

  List.map(~f=x => ACOmniAction(x), Belt.List.concat(tls, ufs))
}

// ------------------------------------
// Create the list
// ------------------------------------

/* Types from Types.tipe that aren't included:
- TCharacter: TODO include once Characters are more easily add-able within code
- TNull: trying to get rid of this, so don't spread it
- TIncomplete: makes no sense to pass to a function
- TError: makes no sense to pass to a function
- TResp: these aren't really exposed to users as real things, but maybe should?
- TErrorRail: doesn't make sense pass to function
- TDbList: only for DB schemas
- TUserType: added later
- TTuple: currently awkward to support parameterized types. TODO
 */
let allowedParamTipes = list{
  DType.TInt,
  TStr,
  TBool,
  TFloat,
  TObj,
  TList,
  TAny,
  TBlock,
  TDB,
  TDate,
  TPassword,
  TUuid,
  TOption,
  TResult,
  TBytes,
}

let allowedReturnTipes = allowedParamTipes

let allowedDBColTipes = {
  let builtins = list{"String", "Int", "Boolean", "Float", "Password", "Date", "UUID", "Dict"}

  let compounds = List.map(~f=s => "[" ++ (s ++ "]"), builtins)
  Belt.List.concat(builtins, compounds)
}

let allowedUserTypeFieldTipes = list{DType.TStr, TInt, TBool, TFloat, TDate, TPassword, TUuid}

let generate = (m: model, a: autocomplete): autocomplete => {
  let space =
    a.target
    |> Option.map(~f=Tuple2.first)
    |> Option.andThen(~f=TL.get(m))
    |> Option.andThen(~f=TL.spaceOf)

  let entries = switch a.target {
  | Some(_, p) =>
    switch P.typeOf(p) {
    // autocomplete HTTP verbs if the handler is in the HTTP event space
    | EventModifier =>
      switch space {
      | Some(HSHTTP) => list{
          ACHTTPModifier("GET"),
          ACHTTPModifier("POST"),
          ACHTTPModifier("PUT"),
          ACHTTPModifier("DELETE"),
          ACHTTPModifier("PATCH"),
          ACHTTPModifier("OPTIONS"),
        }
      | Some(HSCron) => list{
          ACCronTiming("Daily"),
          ACCronTiming("Weekly"),
          ACCronTiming("Fortnightly"),
          ACCronTiming("Every 1hr"),
          ACCronTiming("Every 12hrs"),
          ACCronTiming("Every 1min"),
        }
      | None | Some(HSRepl) | Some(HSDeprecatedOther) | Some(HSWorker) => list{}
      }
    | EventName =>
      switch space {
      | Some(HSHTTP) =>
        let fourOhFourList =
          m.f404s
          |> List.uniqueBy(~f=f404 => f404.path)
          |> List.sortBy(~f=f404 => f404.path)
          |> List.filterMap(~f=f404 =>
            if f404.path !== "/" {
              Some(ACHTTPRoute(cleanHTTPname(f404.path)))
            } else {
              None
            }
          )

        fourOhFourList
      | _ => list{}
      }
    | EventSpace => // Other spaces aren't allowed anymore
      list{ACEventSpace("HTTP"), ACEventSpace("CRON"), ACEventSpace("WORKER"), ACEventSpace("REPL")}
    | DBColType => List.map(~f=x => ACDBColType(x), allowedDBColTipes)
    | ParamTipe =>
      let userTypes = m.userTipes |> Map.filterMapValues(~f=UserTypes.toTUserType)

      Belt.List.concat(allowedParamTipes, userTypes) |> List.map(~f=t => ACParamTipe(t))
    | TypeFieldTipe => allowedUserTypeFieldTipes |> List.map(~f=t => ACTypeFieldTipe(t))
    | FnReturnTipe =>
      let userTypes = m.userTipes |> Map.filterMapValues(~f=UserTypes.toTUserType)

      Belt.List.concat(allowedReturnTipes, userTypes) |> List.map(~f=t => ACReturnTipe(t))
    | DBName | DBColName | FnName | ParamName | TypeName | TypeFieldName => list{}
    }
  | _ => list{}
  }

  let items = if a.target == None {
    tlDestinations(m)
  } else {
    entries
  }
  {...a, allCompletions: items}
}

let filter = (list: list<autocompleteItem>, query: string): list<autocompleteItem> => {
  let lcq = query |> String.toLowercase
  let stringify = i =>
    if 1 >= String.length(lcq) {
      asName(i)
    } else {
      asString(i)
    } |> Regex.replace(~re=Regex.regex(`⟶`), ~repl="->")

  // HACK: dont show Gotos when the query is ""
  let list = List.filter(list, ~f=x =>
    switch x {
    | ACOmniAction(Goto(_)) => query != ""
    | _ => true
    }
  )

  // split into different lists
  let (dynamic, candidates0) = List.partition(~f=isDynamicItem, list)
  let (candidates1, notSubstring) = List.partition(
    ~f=\">>"(\">>"(stringify, String.toLowercase), String.includes(~substring=lcq)),
    candidates0,
  )

  let (startsWith, candidates2) = List.partition(
    ~f=\">>"(stringify, String.startsWith(~prefix=query)),
    candidates1,
  )

  let (startsWithCI, candidates3) = List.partition(
    ~f=\">>"(\">>"(stringify, String.toLowercase), String.startsWith(~prefix=lcq)),
    candidates2,
  )

  let (substring, substringCI) = List.partition(
    ~f=\">>"(stringify, String.includes(~substring=query)),
    candidates3,
  )

  let (stringMatch, _notMatched) = List.partition(
    ~f=\">>"(\">>"(asName, String.toLowercase), containsOrdered(lcq)),
    notSubstring,
  )

  let allMatches =
    list{dynamic, startsWith, startsWithCI, substring, substringCI, stringMatch} |> List.flatten

  allMatches
}

let refilter = (m: model, query: string, old: autocomplete): autocomplete => {
  // add or replace the literal the user is typing to the completions
  let fudgedCompletions = withDynamicItems(m, old.target, query, old.allCompletions)

  let newCompletions = filter(fudgedCompletions, query)
  let allCompletions = newCompletions
  let newCount = List.length(allCompletions)
  let index = // Clear the highlight conditions
  if (
    (query == "" &&
      (// when we had previously highlighted something due to any actual match
      (old.index != -1 && old.value != query) ||
        // or this condition previously held and nothing has changed
        old.index == -1)) ||
      // if nothing matches, highlight nothing
      newCount == 0
  ) {
    -1
  } else {
    0
  }

  {
    ...old,
    index: index,
    completions: newCompletions,
    value: query,
    prevValue: old.value,
  }
}

let regenerate = (m: model, a: autocomplete): autocomplete => generate(m, a) |> refilter(m, a.value)

// ----------------------------
// Autocomplete state
// ----------------------------
let reset = (m: model): autocomplete => {
  {...Defaults.defaultModel.complete, visible: true} |> regenerate(m)
}

let init = m => reset(m)

let numCompletions = (a: autocomplete): int => List.length(a.completions)

let selectDown = (a: autocomplete): autocomplete => {
  let max_ = numCompletions(a)
  let max = max(max_, 1)
  let new_ = mod(a.index + 1, max)
  {...a, index: new_}
}

let selectUp = (a: autocomplete): autocomplete => {
  let max = numCompletions(a) - 1
  {
    ...a,
    index: if a.index <= 0 {
      max
    } else {
      a.index - 1
    },
  }
}

// Implementation:
// n The autocomplete list should include:
// y all imported functions
// y restricted by types that are allowed
// y allowed field names
// n library names
// y case-insensitive
// n order by most likely, offer other alternatives below
// n slight typos
// n slight typeos
// y Press enter to select
// y Press right to fill as much as is definitive
//
let setQuery = (m: model, q: string, a: autocomplete): autocomplete => refilter(m, q, a)

let appendQuery = (m: model, str: string, a: autocomplete): autocomplete => {
  let q = a.value ++ str
  setQuery(m, q, a)
}

let documentationForItem = (aci: autocompleteItem): option<list<Vdom.t<'a>>> => {
  let p = (text: string) => Html.p(list{}, list{Html.text(text)})
  let simpleDoc = (text: string) => Some(list{p(text)})
  switch aci {
  | ACOmniAction(_) => None
  | ACHTTPModifier(verb) => simpleDoc("Make this handler match the " ++ (verb ++ " HTTP verb"))
  | ACCronTiming(timing) => simpleDoc("Request this handler to trigger " ++ timing)
  | ACEventSpace("HTTP") => simpleDoc("This handler will respond to HTTP requests")
  | ACEventSpace("CRON") => simpleDoc("This handler will periodically trigger")
  | ACEventSpace("WORKER") => simpleDoc("This handler will run emitted events in the background")
  | ACEventSpace("REPL") => simpleDoc("This handler allows you run code in it")
  | ACEventSpace(_) =>
    simpleDoc(
      "This handler is deprecated. You should create a new WORKER handler, copy the code over, and change your `emit` calls to point to the new WORKER",
    )
  | ACReplName(name) => simpleDoc("A REPL named " ++ name)
  | ACWorkerName(name) => simpleDoc("Respond to events emitted to " ++ name)
  | ACCronName(_) => simpleDoc("Name of your CRON job")
  | ACHTTPRoute(name) => simpleDoc("Handle HTTP requests made to " ++ name)
  | ACDBName(name) => simpleDoc("Set the DB's name to " ++ name)
  | ACDBColType(tipe) => simpleDoc("This field will be a " ++ tipe)
  | ACParamTipe(tipe) => simpleDoc("This parameter will be a " ++ RT.tipe2str(tipe))
  | ACTypeFieldTipe(tipe) => simpleDoc("This parameter will be a " ++ RT.tipe2str(tipe))
  | ACDBColName(name) => simpleDoc("Set the DB's column name to" ++ name)
  | ACEventModifier(name) => simpleDoc("Set event modifier to " ++ name)
  | ACFnName(fnName) => simpleDoc("Set function name to " ++ fnName)
  | ACParamName(paramName) => simpleDoc("Set param name to " ++ paramName)
  | ACTypeName(typeName) => simpleDoc("Set type name to " ++ typeName)
  | ACReturnTipe(_) | ACTypeFieldName(_) => None
  }
}

let setTarget = (m: model, t: option<target>, a: autocomplete): autocomplete =>
  {...a, target: t} |> regenerate(m)

let setVisible = (visible: bool, a: autocomplete): autocomplete => {...a, visible: visible}

// ------------------------------------
// Commands
// ------------------------------------

let update = (m: model, mod_: autocompleteMod, a: autocomplete): autocomplete =>
  switch mod_ {
  | ACSetQuery(str) => setQuery(m, str, a)
  | ACAppendQuery(str) => appendQuery(m, str, a)
  | ACReset => reset(m)
  | ACSelectDown => selectDown(a)
  | ACSelectUp => selectUp(a)
  | ACSetTarget(target) => setTarget(m, target, a)
  | ACRegenerate => regenerate(m, a)
  | ACSetVisible(visible) => setVisible(visible, a)
  }

/* Checks to see if autocomplete or command palette is opened
 * but not omnibox since it's not scrollable
 */
let isOpened = (ac: autocomplete): bool => Option.isSome(ac.target)

let isOmnibox = (ac: autocomplete): bool => ac.target == None && ac.visible
