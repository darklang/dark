open Prelude

module RT = RuntimeTypes
module TL = Toplevel

module FT = FluidTypes
module Msg = AppTypes.Msg

type model = AppTypes.model

@ppx.deriving(show) type rec t = FT.AutoComplete.t

@ppx.deriving(show) type rec item = FT.AutoComplete.item

@ppx.deriving(show) type rec data = FT.AutoComplete.data

type props = {functions: Types.functionsType}

@ppx.deriving(show) type rec tokenInfo = FluidTypes.TokenInfo.t

let focusItem = (i: int): AppTypes.cmd =>
  Tea_task.attempt(
    _ => Msg.IgnoreMsg("fluid-autocomplete-focus"),
    Tea_task.nativeBinding(_ => {
      open Webapi.Dom
      open Native.Ext
      let container = Document.getElementById("fluid-dropdown", document)
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
  | FACFunction(fn) => PT.FQFnName.toString(fn.fnName)
  | FACField(name) => name
  | FACVariable(name, _) => name
  | FACLiteral(lit) => lit
  | FACConstructorName(name, _) => name
  | FACKeyword(k) =>
    switch k {
    | KLet => "let"
    | KIf => "if"
    | KLambda => "lambda"
    | KMatch => "match"
    | KPipe => "|>"
    }
  | FACPattern(p) =>
    switch p {
    | FPAVariable(_, _, name) | FPAConstructor(_, _, name, _) => name
    | FPABool(_, _, v) => string_of_bool(v)
    | FPANull(_) => "null"
    }
  | FACCreateFunction(name, _, _) => "Create new function: " ++ name
  }

/* Return the string types of the item's arguments and return types. If the
 * item is not a function, the return type will still be used, and might not be
 * a real type, sometimes it's a hint such as "variable". */
let asTypeStrings = (item: item): (list<string>, string) =>
  switch item {
  | FACFunction(f) =>
    f.fnParameters
    |> List.map(~f=x => x.paramTipe)
    |> List.map(~f=DType.tipe2str)
    |> (s => (s, DType.tipe2str(f.fnReturnTipe)))
  | FACField(_) => (list{}, "field")
  | FACVariable(_, odv) =>
    odv
    |> Option.map(~f=(dv: RT.Dval.t) => dv |> RT.Dval.toType |> DType.tipe2str)
    |> Option.unwrap(~default="variable")
    |> (r => (list{}, r))
  | FACPattern(FPAVariable(_)) => (list{}, "variable")
  | FACConstructorName(name, _) | FACPattern(FPAConstructor(_, _, name, _)) =>
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
    let tipe =
      lit
      |> Runtime.parseDvalLiteral
      |> Option.unwrap(~default=RT.Dval.DIncomplete(SourceNone))
      |> RT.Dval.toType
      |> DType.tipe2str

    (list{}, tipe ++ " literal")
  | FACPattern(FPABool(_)) => (list{}, "boolean literal")
  | FACKeyword(_) => (list{}, "keyword")
  | FACPattern(FPANull(_)) => (list{}, "null")
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
  functions: list<function_>,
  tl: toplevel,
  ti: tokenInfo,
): TypeInformation.t => {
  let id = FluidToken.tid(ti.token)
  let default = TypeInformation.default
  TL.getAST(tl)
  |> Option.andThen(~f=AST.getParamIndex(id))
  |> Option.andThen(~f=((name, index)) =>
    functions
    |> List.find(~f=f => name == PT.FQFnName.toString(f.fnName))
    |> Option.map(~f=fn => {
      let param = List.getAt(~index, fn.fnParameters)
      let returnType =
        Option.map(param, ~f=p => p.paramTipe) |> Option.unwrap(~default=default.returnType)

      let paramName =
        Option.map(param, ~f=p => p.paramName) |> Option.unwrap(~default=default.paramName)

      ({fnName: Some(fn.fnName), returnType: returnType, paramName: paramName}: TypeInformation.t)
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
  let invalidFirstArg = tipe => {FT.AutoComplete.item: item, validity: FACItemInvalidPipedArg(tipe)}
  let invalidReturnType = {
    FT.AutoComplete.item: item,
    validity: FACItemInvalidReturnType(expectedReturnType),
  }

  let expectedReturnType = expectedReturnType.returnType
  switch item {
  | FACFunction(fn) =>
    if !Runtime.isCompatible(fn.fnReturnTipe, expectedReturnType) {
      invalidReturnType
    } else {
      switch (List.head(fn.fnParameters), pipedType) {
      | (Some(param), Some(pipedType)) =>
        if Runtime.isCompatible(param.paramTipe, pipedType) {
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
  | FACConstructorName(name, _) =>
    switch expectedReturnType {
    | TOption =>
      if name == "Just" || name == "Nothing" {
        valid
      } else {
        invalidReturnType
      }
    | TResult =>
      if name == "Ok" || name == "Error" {
        valid
      } else {
        invalidReturnType
      }
    | TAny => valid
    | _ => invalidReturnType
    }
  | _ => valid
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
  FACVariable(s.secretName, Some(asDval))
}

let lookupIsInQuery = (tl: toplevel, ti: tokenInfo) => {
  // CLEANUP: use builtin query attribute in the global function list
  let isQueryFn = (name: PT.FQFnName.t) =>
    switch name {
    | Stdlib({module_: "DB", function, version}) =>
      switch (function, version) {
      | ("query", 4)
      | ("queryWithKey", 3)
      | ("queryOne", 3)
      | ("queryOne", 4)
      | ("queryOneWithKey", 3)
      | ("queryCount", 0) => true
      | _ => false
      }
    | Stdlib(_) => false
    | User(_) => false
    | Package(_) => false
    }

  let ast' = TL.getAST(tl)
  switch ast' {
  | None => false
  | Some(ast) =>
    FluidAST.ancestors(FluidToken.tid(ti.token), ast)
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
      | FT.AutoComplete.FACFunction(fn) => fn.fnIsSupportedInQuery
      | _ => false
      }
    )
  }

let generateExprs = (m: model, props: props, tl: toplevel, ti) => {
  open FT.AutoComplete
  let isInQuery = lookupIsInQuery(tl, ti)
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
    |> List.map(~f=((varname, dv)) => FACVariable(varname, dv))

  let keywords = if !isInQuery {
    List.map(~f=x => FACKeyword(x), list{KLet, KIf, KLambda, KMatch, KPipe})
  } else {
    List.map(~f=x => FACKeyword(x), list{KLet, KPipe})
  }

  let literals = List.map(~f=x => FACLiteral(x), list{"true", "false", "null"})

  let secrets = List.map(m.secrets, ~f=secretToACItem)
  Belt.List.concatMany([varnames, constructors, literals, keywords, functions, secrets])
}

let generatePatterns = (ti: tokenInfo, a: t, queryString: string): list<item> => {
  let alreadyHasPatterns = List.any(~f=v =>
    switch v {
    | {item: FACPattern(_), _} => true
    | _ => false
    }
  , a.completions)

  let newStandardPatterns = mid =>
    // if patterns are in the autocomplete already, don't bother creating
    // new FACPatterns with different mids and pids
    if alreadyHasPatterns {
      a.completions |> List.map(~f=({item, _}: data) => item)
    } else {
      list{
        FT.AutoComplete.FPABool(mid, gid(), true),
        FPABool(mid, gid(), false),
        FPAConstructor(mid, gid(), "Just", list{PBlank(gid())}),
        FPAConstructor(mid, gid(), "Nothing", list{}),
        FPAConstructor(mid, gid(), "Ok", list{PBlank(gid())}),
        FPAConstructor(mid, gid(), "Error", list{PBlank(gid())}),
        FPANull(mid, gid()),
      } |> List.map(~f=p => FT.AutoComplete.FACPattern(p))
    } |> List.filter(~f=c =>
      // filter out old query string variable
      switch c {
      | FT.AutoComplete.FACPattern(FPAVariable(_)) => false
      | _ => true
      }
    )

  let isInvalidPatternVar = str =>
    list{"", "Just", "Nothing", "Ok", "Error", "true", "false", "null"} |> List.member(
      ~value=str,
    ) || str |> String.dropRight(~count=String.length(str) - 1) |> String.isCapitalized

  let newQueryVariable = mid =>
    /* no Query variable if the query is empty or equals to standard
     * constructor or boolean name */
    if isInvalidPatternVar(queryString) {
      list{}
    } else {
      list{FT.AutoComplete.FACPattern(FPAVariable(mid, gid(), queryString))}
    }

  switch ti.token {
  | TPatternBlank(mid, _, _) | TPatternVariable(mid, _, _, _) =>
    Belt.List.concat(newQueryVariable(mid), newStandardPatterns(mid))
  | _ => list{}
  }
}

let generateCommands = (_name, _tlid, _id) =>
  // Disable for now, this is really annoying
  // [FACCreateFunction (name, tlid, id)]
  list{}

let generateFields = fieldList => List.map(~f=x => FT.AutoComplete.FACField(x), fieldList)

let generate = (m: model, props: props, a: t, query: fullQuery): list<item> => {
  let tlid = TL.id(query.tl)
  switch query.ti.token {
  | TPatternBlank(_) | TPatternVariable(_) => generatePatterns(query.ti, a, query.queryString)
  | TFieldName(_) | TFieldPartial(_) => generateFields(query.fieldList)
  | TLeftPartial(_) => // Left partials can ONLY be if/let/match for now
    list{FACKeyword(KLet), FACKeyword(KIf), FACKeyword(KMatch)}
  | TPartial(id, name, _) =>
    Belt.List.concat(generateExprs(m, props, query.tl, query.ti), generateCommands(name, tlid, id))
  | _ => generateExprs(m, props, query.tl, query.ti)
  }
}

let filter = (functions: list<function_>, candidates0: list<item>, query: fullQuery): list<
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

/* Regenerate calls generate, except that it adapts the result using the
 * existing state (mostly putting the index in the right place. */
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
    let items = generate(m, props, a, query)
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
  | FACItemInvalidPipedArg(tipe) =>
    let acFunction = asName(item)
    let acFirstArgType = asTypeStrings(item) |> Tuple2.first |> List.head
    let typeInfo = switch acFirstArgType {
    | None => list{Html.text(" takes no arguments.")}
    | Some(tipeStr) => list{
        Html.text(" takes a "),
        Html.span(list{Html.class'("type")}, list{Html.text(tipeStr)}),
        Html.text(" as its first argument."),
      }
    }

    Html.div(
      list{},
      list{
        Html.span(list{Html.class'("err")}, list{Html.text("Type error: ")}),
        Html.text("A value of type "),
        Html.span(list{Html.class'("type")}, list{Html.text(DType.tipe2str(tipe))}),
        Html.text(" is being piped into this function call, but "),
        Html.span(list{Html.class'("fn")}, list{Html.text(acFunction)}),
        ...typeInfo,
      },
    )
  | FACItemInvalidReturnType({fnName, paramName, returnType}) =>
    let acFunction = asName(item)
    let acReturnType = asTypeStrings(item) |> Tuple2.second
    Html.div(
      list{},
      list{
        Html.span(list{Html.class'("err")}, list{Html.text("Type error: ")}),
        Html.span(
          list{Html.class'("fn")},
          list{Html.text(fnName->Option.map(~f=PT.FQFnName.toString)->Option.unwrap(~default=""))},
        ),
        Html.text(" expects "),
        Html.span(list{Html.class'("param")}, list{Html.text(paramName)}),
        Html.text(" to be a "),
        Html.span(list{Html.class'("type")}, list{Html.text(DType.tipe2str(returnType))}),
        Html.text(", but "),
        Html.span(list{Html.class'("fn")}, list{Html.text(acFunction)}),
        Html.text(" returns a "),
        Html.span(list{Html.class'("type")}, list{Html.text(acReturnType)}),
      },
    )
  }
}

let rec documentationForItem = ({item, validity}: data): option<list<Vdom.t<'a>>> => {
  let p = (text: string) => Html.p(list{}, list{Html.text(text)})
  let typeDoc = typeErrorDoc({item: item, validity: validity})
  let simpleDoc = (text: string) => Some(list{p(text), typeDoc})
  let deprecated = Html.span(list{Html.class'("err")}, list{Html.text("DEPRECATED: ")})
  switch item {
  | FACFunction(f) =>
    let desc = if String.length(f.fnDescription) != 0 {
      f.fnDescription
    } else {
      "Function call with no description"
    }

    let desc = PrettyDocs.convert(desc)
    let desc = if f.fnDeprecated {
      list{deprecated, ...desc}
    } else {
      desc
    }
    Some(Belt.List.concat(desc, list{ViewErrorRailDoc.hintForFunction(f, None), typeDoc}))
  | FACConstructorName("Just", _) => simpleDoc("An Option containing a value")
  | FACConstructorName("Nothing", _) => simpleDoc("An Option representing Nothing")
  | FACConstructorName("Ok", _) => simpleDoc("A successful Result containing a value")
  | FACConstructorName("Error", _) => simpleDoc("A Result representing a failure")
  | FACConstructorName(name, _) =>
    simpleDoc("TODO: this should never occur: the constructor " ++ name)
  | FACField(fieldname) => simpleDoc("The '" ++ (fieldname ++ "' field of the object"))
  | FACVariable(var, _) =>
    if String.isCapitalized(var) {
      simpleDoc("The datastore '" ++ (var ++ "'"))
    } else {
      simpleDoc("The variable '" ++ (var ++ "'"))
    }
  | FACLiteral(lit) => simpleDoc("The literal value '" ++ (lit ++ "'"))
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
  | FACPattern(pat) =>
    switch pat {
    | FPAConstructor(_, _, name, args) =>
      documentationForItem({item: FACConstructorName(name, List.length(args)), validity: validity})
    | FPAVariable(_, _, name) =>
      documentationForItem({item: FACVariable(name, None), validity: validity})
    | FPABool(_, _, var) =>
      documentationForItem({item: FACLiteral(string_of_bool(var)), validity: validity})
    | FPANull(_) => simpleDoc("A 'null' literal")
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
