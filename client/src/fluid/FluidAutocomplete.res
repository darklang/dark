open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module RT = RuntimeTypes
module TL = Toplevel

module FT = FluidTypes
module Msg = AppTypes.Msg

type model = AppTypes.model

@ppx.deriving(show) type rec t = FT.AutoComplete.t

@ppx.deriving(show) type rec item = FT.AutoComplete.item

@ppx.deriving(show) type rec data = FT.AutoComplete.data

type props = {functions: Functions.t}

@ppx.deriving(show) type rec tokenInfo = FluidTypes.TokenInfo.t

let focusItem = (i: int): AppTypes.cmd =>
  Tea_task.attempt(
    _ => Msg.IgnoreMsg("fluid-autocomplete-focus"),
    Tea_task.nativeBinding(_ => {
      open Webapi.Dom
      open Native.Ext
      let container = Document.getElementById(document, "fluid-dropdown")
      let nthChild = querySelector(
        "#fluid-dropdown ul li:nth-child(" ++ (string_of_int(i + 1) ++ ")"),
      )

      switch (container, nthChild) {
      | (Some(el), Some(li)) =>
        let cRect = getBoundingClientRect(el)
        let cBottom = rectBottom(cRect)
        let cTop = rectTop(cRect)
        let liRect = getBoundingClientRect(li)
        let liBottom = rectBottom(liRect)
        let liTop = rectTop(liRect)
        let liHeight = rectHeight(liRect)
        if liBottom +. liHeight > cBottom {
          let offset = float_of_int(offsetTop(li))
          let padding = rectHeight(cRect) -. liHeight *. 2.0
          Element.setScrollTop(el, offset -. padding)
        } else if liTop -. liHeight < cTop {
          let offset = float_of_int(offsetTop(li))
          Element.setScrollTop(el, offset -. liHeight)
        } else {
          ()
        }
      | (_, _) => ()
      }
    }),
  )

// ----------------------------
// display
// ----------------------------
let asName = (aci: item): string =>
  switch aci {
  | FACFunction(fn) => FQFnName.toString(fn.name)
  | FACField(name) => name
  | FACVariable(name, _) => name
  | FACDatastore(name) => name
  | FACSecret(name, _) => name
  | FACLiteral(lit) =>
    switch lit {
    | LNull => "null"
    | LBool(true) => "true"
    | LBool(false) => "false"
    }
  | FACConstructorName(name, _) => name
  | FACKeyword(k) =>
    switch k {
    | KLet => "let"
    | KIf => "if"
    | KLambda => "lambda"
    | KMatch => "match"
    | KPipe => "|>"
    }
  | FACMatchPattern(_, p) =>
    switch p {
    | FT.AutoComplete.FMPABlank => "blank"
    | FMPAVariable(name) | FMPAConstructor(name, _) => name
    | FMPABool(v) => string_of_bool(v)
    | FMPANull => "null"
    | FMPATuple => "tuple"
    }
  | FACCreateFunction(name, _, _) => "Create new function: " ++ name
  }

/* Return the string types of the item's arguments and return types. If the
 * item is not a function, the return type will still be used, and might not be
 * a real type, sometimes it's a hint such as "variable". */
let asTypeStrings = (item: item): (list<string>, string) =>
  switch item {
  | FACFunction(f) =>
    f.parameters
    |> List.map(~f=(x: RuntimeTypes.BuiltInFn.Param.t) => x.typ)
    |> List.map(~f=DType.tipe2str)
    |> (s => (s, DType.tipe2str(f.returnType)))
  | FACField(_) => (list{}, "field")
  | FACVariable(_, odv) =>
    odv
    |> Option.map(~f=(dv: RT.Dval.t) => dv |> RT.Dval.toType |> DType.tipe2str)
    |> Option.unwrap(~default="variable")
    |> (r => (list{}, r))
  | FACSecret(_, dv) => (list{}, dv |> RT.Dval.toType |> DType.tipe2str)
  | FACDatastore(_) => (list{}, "datastore")
  | FACMatchPattern(_, FMPAVariable(_)) => (list{}, "variable")
  | FACConstructorName(name, _) | FACMatchPattern(_, FMPAConstructor(name, _)) =>
    if name == "Just" {
      (list{"any"}, "option")
    } else if name == "Nothing" {
      (list{}, "option")
    } else if name == "Ok" || name == "Error" {
      (list{"any"}, "result")
    } else {
      (list{}, "unknown")
    }
  | FACLiteral(lit) =>
    let typ = switch lit {
    | LNull => "null"
    | LBool(_) => "bool"
    }

    (list{}, typ ++ " literal")
  | FACMatchPattern(_, FMPABool(_)) => (list{}, "boolean literal")
  | FACKeyword(_) => (list{}, "keyword")
  | FACMatchPattern(_, FMPANull) => (list{}, "null")
  | FACMatchPattern(_, FMPATuple) => (list{}, `tuple (a, b)`)
  | FACMatchPattern(_, FMPABlank) => (list{}, `blank ___`)
  | FACCreateFunction(_) => (list{}, "")
  }

// Used for matching, not for displaying to users
let asMatchingString = (aci: item): string => {
  let (argTypes, returnType) = asTypeStrings(aci)
  let typeString = String.join(~sep=", ", argTypes) ++ (" -> " ++ returnType)
  asName(aci) ++ typeString
}

// ----------------------------
// Utils
// ----------------------------

let isVariable = (aci: item): bool =>
  switch aci {
  | FACVariable(_) => true
  | _ => false
  }

let isField = (aci: item): bool =>
  switch aci {
  | FACField(_) => true
  | _ => false
  }

let isFnCall = (aci: item): bool =>
  switch aci {
  | FACFunction(_) => true
  | _ => false
  }

let isCreateFn = (aci: item): bool =>
  switch aci {
  | FACCreateFunction(_) => true
  | _ => false
  }

let item = (data: data): item => data.item

// ----------------------------
// External: utils
// ----------------------------

/* Return the item that is highlighted (at a.index position in the
 * list), along with whether that it is a valid autocomplete option right now. */
let highlightedWithValidity = (a: t): option<data> =>
  Option.andThen(a.index, ~f=index => List.getAt(~index, a.completions))

/* Return the item that is highlighted (at a.index position in the
 * list). */
let highlighted = (a: t): option<item> =>
  highlightedWithValidity(a) |> Option.map(~f=(d: data) => d.item)

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
// Type checking
// ------------------------------------

// Return the value being piped into the token at ti, if there is one
let findPipedDval = (m: model, tl: toplevel, ti: tokenInfo): option<RT.Dval.t> => {
  let id =
    TL.getAST(tl)
    |> Option.andThen(~f=AST.pipePrevious(FluidToken.tid(ti.token)))
    |> Option.map(~f=FluidExpression.toID)

  let tlid = TL.id(tl)
  Analysis.getSelectedTraceID(m, tlid)
  |> Option.andThen2(id, ~f=Analysis.getLiveValue(m))
  |> Option.andThen(~f=dv =>
    switch dv {
    | RT.Dval.DIncomplete(_) => None
    | _ => Some(dv)
    }
  )
}

// Return the fields of the object being referenced at ti, if there is one
let findFields = (m: model, tl: toplevel, ti: tokenInfo): list<string> => {
  let tlid = TL.id(tl)
  let id = switch ti.token {
  | TFieldOp(_, lhsID, _)
  | TFieldName(_, lhsID, _, _)
  | TFieldPartial(_, _, lhsID, _, _) => lhsID
  | _ => FluidToken.tid(ti.token)
  }

  Analysis.getSelectedTraceID(m, tlid)
  |> Option.andThen(~f=Analysis.getLiveValue(m, id))
  |> Option.map(~f=dv =>
    switch dv {
    | RT.Dval.DObj(dict) => Belt.Map.String.keysToArray(dict) |> Array.toList
    | _ => list{}
    }
  )
  |> Option.unwrap(~default=list{})
}

let findExpectedType = (
  functions: list<Function.t>,
  tl: toplevel,
  ti: tokenInfo,
): TypeInformation.t => {
  let id = FluidToken.tid(ti.token)
  let default = TypeInformation.default
  TL.getAST(tl)
  |> Option.andThen(~f=AST.getParamIndex(id))
  |> Option.andThen(~f=((name, index)) =>
    functions
    |> List.find(~f=(f: Function.t) => name == FQFnName.toString(f.name))
    |> Option.map(~f=(fn: Function.t) => {
      let param = List.getAt(~index, fn.parameters)
      let returnType =
        Option.map(param, ~f=p => p.typ) |> Option.unwrap(~default=default.returnType)

      let name =
        Option.map(param, ~f=(p: RuntimeTypes.BuiltInFn.Param.t) => p.name) |> Option.unwrap(
          ~default=default.paramName,
        )

      ({fnName: Some(fn.name), returnType: returnType, paramName: name}: TypeInformation.t)
    })
  )
  |> Option.unwrap(~default)
}

// Checks whether an autocomplete item matches the expected types
let typeCheck = (
  pipedType: option<DType.t>,
  expectedReturnType: TypeInformation.t,
  item: item,
): data => {
  let valid: data = {item: item, validity: FACItemValid}
  let invalidFirstArg = typ => {FT.AutoComplete.item: item, validity: FACItemInvalidPipedArg(typ)}
  let invalidReturnType = {
    FT.AutoComplete.item: item,
    validity: FACItemInvalidReturnType(expectedReturnType),
  }

  let expectedReturnType = expectedReturnType.returnType
  switch item {
  | FACFunction(fn) =>
    if !Runtime.isCompatible(fn.returnType, expectedReturnType) {
      invalidReturnType
    } else {
      switch (List.head(fn.parameters), pipedType) {
      | (Some(param), Some(pipedType)) =>
        if Runtime.isCompatible(param.typ, pipedType) {
          valid
        } else {
          invalidFirstArg(pipedType)
        }
      | (None, Some(pipedType)) =>
        // if it takes no arguments, piping into it is invalid
        invalidFirstArg(pipedType)
      | _ => valid
      }
    }
  | FACVariable(_, dval) =>
    switch dval {
    | Some(dv) =>
      if Runtime.isCompatible(RT.Dval.toType(dv), expectedReturnType) {
        valid
      } else {
        invalidReturnType
      }
    | None => valid
    }
  | FACSecret(_, dval) =>
    if Runtime.isCompatible(RT.Dval.toType(dval), expectedReturnType) {
      valid
    } else {
      invalidReturnType
    }
  | FACDatastore(_) =>
    // TODO: do better with the type
    if Runtime.isCompatible(TDB(TVariable("")), expectedReturnType) {
      valid
    } else {
      invalidReturnType
    }

  | FACConstructorName(name, _) =>
    switch expectedReturnType {
    | TOption(_) =>
      if name == "Just" || name == "Nothing" {
        valid
      } else {
        invalidReturnType
      }
    | TResult(_) =>
      if name == "Ok" || name == "Error" {
        valid
      } else {
        invalidReturnType
      }
    | TVariable(_) => valid
    | _ => invalidReturnType
    }
  | FACField(_) | FACLiteral(_) | FACKeyword(_) | FACMatchPattern(_) | FACCreateFunction(_) => valid
  }
}

@ppx.deriving(show) type rec query = (TLID.t, tokenInfo)

type fullQuery = {
  tl: toplevel,
  ti: tokenInfo,
  fieldList: list<string>,
  pipedDval: option<RT.Dval.t>,
  queryString: string,
}

let toQueryString = (ti: tokenInfo): string =>
  if FluidToken.isBlank(ti.token) {
    ""
  } else {
    FluidToken.toText(ti.token)
  }

// ----------------------------
// Autocomplete state
// ----------------------------
let init: t = FluidTypes.AutoComplete.default

// ------------------------------------
// Create the list
// ------------------------------------

let secretToACItem = (s: SecretTypes.t): item => {
  let asDval = RT.Dval.DStr(Util.obscureString(s.secretValue))
  FACSecret(s.secretName, asDval)
}

let lookupIsInQuery = (tl: toplevel, ti: tokenInfo, functions: Functions.t) => {
  let isQueryFn = (name: FQFnName.t) => {
    switch Functions.find(name, functions) {
    | Some(fn) => fn.sqlSpec == QueryFunction
    | None => false
    }
  }

  let ast' = TL.getAST(tl)
  switch ast' {
  | None => false
  | Some(ast) =>
    FluidAST.exprAncestors(FluidToken.tid(ti.token), ast)
    |> List.find(~f=x =>
      switch x {
      | ProgramTypes.Expr.EFnCall(_, name, _, _) => isQueryFn(name)
      | _ => false
      }
    )
    |> Option.is_some
  }
}

let filterToDbSupportedFns = (isInQuery, functions) =>
  if !isInQuery {
    functions
  } else {
    functions |> List.filter(~f=f =>
      switch f {
      | FT.AutoComplete.FACFunction(fn) => RuntimeTypes.BuiltInFn.SqlSpec.isQueryable(fn.sqlSpec)
      | _ => false
      }
    )
  }

let generateExprs = (m: model, props: props, tl: toplevel, ti) => {
  open FT.AutoComplete
  let isInQuery = lookupIsInQuery(tl, ti, props.functions)
  let functions' = Functions.asFunctions(props.functions) |> List.map(~f=x => FACFunction(x))

  let functions = filterToDbSupportedFns(isInQuery, functions')
  let constructors = if !isInQuery {
    list{
      FACConstructorName("Just", 1),
      FACConstructorName("Nothing", 0),
      FACConstructorName("Ok", 1),
      FACConstructorName("Error", 1),
    }
  } else {
    list{}
  }

  let id = FluidToken.tid(ti.token)
  let varnames =
    Analysis.getSelectedTraceID(m, TL.id(tl))
    |> Option.map(~f=Analysis.getAvailableVarnames(m, tl, id))
    |> Option.unwrap(~default=list{})
    |> List.map(~f=((varname, dv)) =>
      if String.isCapitalized(varname) {
        FACDatastore(varname)
      } else {
        FACVariable(varname, dv)
      }
    )

  let keywords = if !isInQuery {
    List.map(~f=x => FACKeyword(x), list{KLet, KIf, KLambda, KMatch, KPipe})
  } else {
    List.map(~f=x => FACKeyword(x), list{KLet, KPipe})
  }

  let literals = List.map(~f=x => FACLiteral(x), list{LBool(true), LBool(false), LNull})

  let secrets = List.map(m.secrets, ~f=secretToACItem)
  Belt.List.concatMany([varnames, secrets, constructors, literals, keywords, functions])
}

let generateMatchPatterns = (allowTuples: bool, ti: tokenInfo, queryString: string): list<item> => {
  let newStandardPatterns = list{
    FT.AutoComplete.FMPABool(true),
    FMPABool(false),
    FMPAConstructor("Just", list{FMPABlank}),
    FMPAConstructor("Nothing", list{}),
    FMPAConstructor("Ok", list{FMPABlank}),
    FMPAConstructor("Error", list{FMPABlank}),
    FMPANull,
  }

  let newTuplePattern = if allowTuples {
    Some(FT.AutoComplete.FMPATuple)
  } else {
    None
  }

  let newVariablePattern = {
    let matchesExpectedPattern = List.member(
      ~value=queryString,
      list{"", "Just", "Nothing", "Ok", "Error", "true", "false", "null"},
    )

    let firstCharacterIsCapitalized = String.left(~count=1, queryString) |> String.isCapitalized

    // if the query is empty, or equals a standard constructor or boolean name,
    // or starts with a capital letter (invalid variable name), don't return
    // a variable pattern suggestion.
    if matchesExpectedPattern || firstCharacterIsCapitalized {
      None
    } else {
      Some(FT.AutoComplete.FMPAVariable(queryString))
    }
  }

  switch ti.token {
  | TMPBlank(mid, _, _) | TMPVariable(mid, _, _, _) =>
    Belt.List.concatMany([
      Option.toList(newVariablePattern),
      newStandardPatterns,
      Option.toList(newTuplePattern),
    ]) |> List.map(~f=p => FT.AutoComplete.FACMatchPattern(mid, p))
  | _ => list{}
  }
}

let generateCommands = (_name, _tlid, _id) =>
  // Disable for now, this is really annoying
  // [FACCreateFunction (name, tlid, id)]
  list{}

let generateFields = fieldList => List.map(~f=x => FT.AutoComplete.FACField(x), fieldList)

let generate = (m: model, props: props, query: fullQuery): list<item> => {
  let tlid = TL.id(query.tl)
  switch query.ti.token {
  | TMPBlank(_) | TMPVariable(_) =>
    let allowTuples = m.settings.contributingSettings.inProgressFeatures.allowTuples
    generateMatchPatterns(allowTuples, query.ti, query.queryString)

  | TFieldName(_) | TFieldPartial(_) => generateFields(query.fieldList)
  | TLeftPartial(_) => // Left partials can ONLY be if/let/match for now
    list{FACKeyword(KLet), FACKeyword(KIf), FACKeyword(KMatch)}
  | TPartial(id, _, name, _) =>
    Belt.List.concat(generateExprs(m, props, query.tl, query.ti), generateCommands(name, tlid, id))
  | _ => generateExprs(m, props, query.tl, query.ti)
  }
}

let filter = (functions: list<Function.t>, candidates0: list<item>, query: fullQuery): list<
  data,
> => {
  let stripColons = Regex.replace(~re=Regex.regex("::"), ~repl="")
  let lcq = query.queryString |> String.toLowercase |> stripColons
  let stringify = i =>
    if 1 >= String.length(lcq) {
      asName(i)
    } else {
      asMatchingString(i)
    }
    |> Regex.replace(~re=Regex.regex(`⟶`), ~repl="->")
    |> stripColons

  // split into different lists
  let (candidates1, notSubstring) = List.partition(
    ~f=\">>"(\">>"(stringify, String.toLowercase), String.includes(~substring=lcq)),
    candidates0,
  )

  let (startsWith, candidates2) = List.partition(
    ~f=\">>"(stringify, String.startsWith(~prefix=query.queryString)),
    candidates1,
  )

  let (startsWithCI, candidates3) = List.partition(
    ~f=\">>"(\">>"(stringify, String.toLowercase), String.startsWith(~prefix=lcq)),
    candidates2,
  )

  let (substring, substringCI) = List.partition(
    ~f=\">>"(stringify, String.includes(~substring=query.queryString)),
    candidates3,
  )

  let (stringMatch, _notMatched) = List.partition(
    ~f=\">>"(\">>"(asName, String.toLowercase), containsOrdered(lcq)),
    notSubstring,
  )

  let allMatches =
    list{startsWith, startsWithCI, substring, substringCI, stringMatch} |> List.flatten

  // Now split list by type validity
  let pipedType = Option.map(~f=RT.Dval.toType, query.pipedDval)
  let expectedTypeInfo = findExpectedType(functions, query.tl, query.ti)
  List.map(allMatches, ~f=typeCheck(pipedType, expectedTypeInfo))
}

let refilter = (props: props, query: fullQuery, old: t, items: list<item>): t => {
  // add or replace the literal the user is typing to the completions
  let newCompletions = filter(Functions.asFunctions(props.functions), items, query)

  let oldHighlight = highlighted(old)
  let newCount = List.length(newCompletions)
  let oldHighlightNewIndex =
    oldHighlight |> Option.andThen(~f=oh =>
      List.elemIndex(~value=oh, List.map(~f=({item, _}) => item, newCompletions))
    )

  let oldQueryString = switch old.query {
  | Some(_, ti) => toQueryString(ti)
  | _ => ""
  }

  let isFieldPartial = switch query.ti.token {
  | TFieldPartial(_) => true
  | _ => false
  }

  let index = if isFieldPartial {
    if query.queryString == "" && query.queryString != oldQueryString {
      /* Show autocomplete - the first item - when there's no text. If we
       * just deleted the text, reset to the top. But only reset on change
       * - we want the arrow keys to work */
      Some(0)
    } else if oldQueryString == "" && old.index == Some(0) {
      // If we didn't actually select the old value, don't cling to it.
      Some(0)
    } else if Option.isSome(oldHighlightNewIndex) {
      // Otherwise we did select something, so let's find it.
      oldHighlightNewIndex
    } else {
      // Always show fields.
      Some(0)
    }
  } else if query.queryString == "" || newCount == 0 {
    // Do nothing if no queryString or autocomplete list
    None
  } else if oldQueryString == query.queryString {
    // If we didn't change anything, don't change anything
    switch oldHighlightNewIndex {
    | Some(newIndex) => Some(newIndex)
    | None => None
    }
  } else {
    // If an entry vanishes, highlight 0
    Some(0)
  }

  {index: index, query: Some(TL.id(query.tl), query.ti), completions: newCompletions}
}

@ocaml.doc("Regenerate calls generate, except that it adapts the result using
  the existing state (mostly putting the index in the right place.")
let regenerate = (m: model, a: t, (tlid, ti): query): t =>
  switch TL.get(m, tlid) {
  | None => init
  | Some(tl) =>
    let props = {functions: m.functions}
    let queryString = toQueryString(ti)
    let fieldList = findFields(m, tl, ti)
    let pipedDval = findPipedDval(m, tl, ti)
    let query = {
      tl: tl,
      ti: ti,
      fieldList: fieldList,
      pipedDval: pipedDval,
      queryString: queryString,
    }
    let items = generate(m, props, query)
    refilter(props, query, a, items)
  }

// ----------------------------
// Autocomplete state
// ----------------------------

let numCompletions = (a: t): int => List.length(a.completions)

let selectDown = (a: t): t =>
  switch a.index {
  | Some(index) =>
    let max_ = numCompletions(a)
    let max = max(max_, 1)
    let new_ = mod(index + 1, max)
    {...a, index: Some(new_)}
  | None => a
  }

let selectUp = (a: t): t =>
  switch a.index {
  | Some(index) =>
    let max = numCompletions(a) - 1
    {
      ...a,
      index: Some(
        if index <= 0 {
          max
        } else {
          index - 1
        },
      ),
    }
  | None => a
  }

let isOpened = (ac: t): bool => Option.isSome(ac.index)

let typeErrorDoc = ({item, validity}: data): Vdom.t<AppTypes.msg> => {
  let _types = asTypeStrings(item)
  let _validity = validity
  switch validity {
  | FACItemValid => Vdom.noNode
  | FACItemInvalidPipedArg(typ) =>
    let acFunction = asName(item)
    let acFirstArgType = asTypeStrings(item) |> Tuple2.first |> List.head
    let typeInfo = switch acFirstArgType {
    | None => list{Html.text(" takes no arguments.")}
    | Some(tipeStr) => list{
        Html.text(" takes a "),
        Html.span(list{Attrs.class'("type")}, list{Html.text(tipeStr)}),
        Html.text(" as its first argument."),
      }
    }

    Html.div(
      list{},
      list{
        Html.span(list{Attrs.class'("err")}, list{Html.text("Type error: ")}),
        Html.text("A value of type "),
        Html.span(list{Attrs.class'("type")}, list{Html.text(DType.tipe2str(typ))}),
        Html.text(" is being piped into this function call, but "),
        Html.span(list{Attrs.class'("fn")}, list{Html.text(acFunction)}),
        ...typeInfo,
      },
    )
  | FACItemInvalidReturnType({fnName, paramName, returnType}) =>
    let acFunction = asName(item)
    let acReturnType = asTypeStrings(item) |> Tuple2.second
    Html.div(
      list{},
      list{
        Html.span(list{Attrs.class'("err")}, list{Html.text("Type error: ")}),
        Html.span(
          list{Attrs.class'("fn")},
          list{Html.text(fnName->Option.map(~f=FQFnName.toString)->Option.unwrap(~default=""))},
        ),
        Html.text(" expects "),
        Html.span(list{Attrs.class'("param")}, list{Html.text(paramName)}),
        Html.text(" to be a "),
        Html.span(list{Attrs.class'("type")}, list{Html.text(DType.tipe2str(returnType))}),
        Html.text(", but "),
        Html.span(list{Attrs.class'("fn")}, list{Html.text(acFunction)}),
        Html.text(" returns a "),
        Html.span(list{Attrs.class'("type")}, list{Html.text(acReturnType)}),
      },
    )
  }
}

let documentationForFunction = (
  f: Function.t,
  sendToRail: option<ProgramTypes.Expr.SendToRail.t>,
): list<Tea.Html.html<'msg>> => {
  let desc = if String.length(f.description) != 0 {
    PrettyDocs.convert(f.description)
  } else {
    list{Html.i(list{}, list{Html.text("no description provided")})}
  }

  let return = Html.div(
    list{Attrs.class("returnType")},
    list{
      Html.text("Returns: "),
      Html.span(list{Attrs.class("type")}, list{Html.text(DType.tipe2str(f.returnType))}),
    },
  )

  let deprecationHeader = if f.deprecation != NotDeprecated {
    list{Html.span(list{Attrs.class'("err")}, list{Html.text("DEPRECATED: ")})}
  } else {
    list{}
  }

  let deprecationFooter = {
    let deprecationFooterContents = switch f.deprecation {
    | NotDeprecated => list{}
    | ReplacedBy(name) => list{Html.text("replaced by " ++ FQFnName.StdlibFnName.toString(name))}
    | RenamedTo(name) => list{Html.text("renamed to " ++ FQFnName.StdlibFnName.toString(name))}
    | DeprecatedBecause(reason) => list{Html.text(reason)}
    }
    if deprecationFooterContents == list{} {
      list{}
    } else {
      list{
        Html.div(
          list{Attrs.class("deprecation-reason")},
          list{
            Html.span(list{Attrs.class'("err")}, list{Html.text("DEPRECATED: ")}),
            ...deprecationFooterContents,
          },
        ),
      }
    }
  }

  Belt.List.concatMany([
    deprecationHeader,
    desc,
    list{ViewErrorRailDoc.hintForFunction(f, sendToRail)},
    deprecationFooter,
    list{return},
  ])
}

let rec documentationForItem = ({item, validity}: data): option<list<Vdom.t<'a>>> => {
  let p = (text: string) => Html.p(list{}, list{Html.text(text)})
  let typeDoc = typeErrorDoc({item: item, validity: validity})
  let simpleDoc = (text: string) => Some(list{p(text), typeDoc})
  switch item {
  | FACFunction(f) =>
    let docs = documentationForFunction(f, None)
    Some(List.append(docs, list{typeDoc}))
  | FACConstructorName("Just", _) => simpleDoc("An Option containing a value")
  | FACConstructorName("Nothing", _) => simpleDoc("An Option representing Nothing")
  | FACConstructorName("Ok", _) => simpleDoc("A successful Result containing a value")
  | FACConstructorName("Error", _) => simpleDoc("A Result representing a failure")
  | FACConstructorName(name, _) =>
    simpleDoc("TODO: this should never occur: the constructor " ++ name)
  | FACField(fieldname) => simpleDoc("The '" ++ fieldname ++ "' field of the object")
  | FACDatastore(var) => simpleDoc("The datastore '" ++ var ++ "'")
  | FACSecret(name, _) => simpleDoc("The secret '" ++ name ++ "'")
  | FACVariable(var, _) => simpleDoc("The variable '" ++ var ++ "'")
  | FACLiteral(_) => simpleDoc("The literal value '" ++ asName(item) ++ "'")
  | FACKeyword(KLet) =>
    simpleDoc("A `let` expression allows you assign a variable to an expression")
  | FACKeyword(KIf) => simpleDoc("An `if` expression allows you to branch on a boolean condition")
  | FACKeyword(KLambda) =>
    simpleDoc(
      "A `lambda` creates an anonymous function. This is most often used for iterating through lists",
    )
  | FACKeyword(KMatch) =>
    simpleDoc(
      "A `match` expression allows you to pattern match on a value, and return different expressions based on many possible conditions",
    )
  | FACKeyword(KPipe) => simpleDoc("Pipe into another expression")
  | FACMatchPattern(_, pat) =>
    switch pat {
    | FMPAConstructor(name, args) =>
      documentationForItem({item: FACConstructorName(name, List.length(args)), validity: validity})
    | FMPAVariable(name) =>
      documentationForItem({item: FACVariable(name, None), validity: validity})
    | FMPABool(b) => documentationForItem({item: FACLiteral(LBool(b)), validity: validity})
    | FMPANull => simpleDoc("A 'null' literal")
    | FMPATuple => simpleDoc("A tuple containing several sub-patterns")
    | FMPABlank => simpleDoc("A blank pattern")
    }
  | FACCreateFunction(_) => None
  }
}

let updateAutocompleteVisibility = (m: model): model => {
  let oldTlid = switch m.fluidState.ac.query {
  | Some(tlid, _) => Some(tlid)
  | None => CursorState.tlidOf(m.cursorState)
  }

  let newTlid = CursorState.tlidOf(m.cursorState)
  if isOpened(m.fluidState.ac) && oldTlid != newTlid {
    let newAc = init
    {...m, fluidState: {...m.fluidState, ac: newAc}}
  } else {
    m
  }
}
