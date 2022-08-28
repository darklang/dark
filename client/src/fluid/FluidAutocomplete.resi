let focusItem: int => AppTypes.cmd

@ppx.deriving(show) type rec t = FluidTypes.AutoComplete.t

@ppx.deriving(show) type rec item = FluidTypes.AutoComplete.item

@ppx.deriving(show) type rec data = FluidTypes.AutoComplete.data

@ppx.deriving(show) type rec query = (TLID.t, FluidTypes.TokenInfo.t)

type props = {functions: Functions.t}

let asName: item => string

let asTypeStrings: item => (list<string>, string)

let isVariable: item => bool

let isField: item => bool

let isFnCall: item => bool

let isCreateFn: item => bool

let item: data => item

let highlightedWithValidity: t => option<data>

let highlighted: t => option<item>

let init: t

let regenerate: (AppTypes.model, t, query) => t

let numCompletions: t => int

let selectUp: t => t

let selectDown: t => t

let documentationForItem: data => option<list<Vdom.t<AppTypes.msg>>>

let isOpened: t => bool

let updateAutocompleteVisibility: AppTypes.model => AppTypes.model

/* only exposed for tests */
type fullQuery = {
  tl: Types.toplevel,
  ti: FluidTypes.TokenInfo.t,
  fieldList: list<string>,
  pipedDval: option<RuntimeTypes.Dval.t>,
  queryString: string,
}

let refilter: (props, fullQuery, t, list<item>) => t
