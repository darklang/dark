let focusItem: int => Tea.Cmd.t<Types.msg>

@ppx.deriving(show) type rec t = Types.fluidAutocompleteState

@ppx.deriving(show) type rec item = Types.fluidAutocompleteItem

@ppx.deriving(show) type rec data = Types.fluidAutocompleteData

@ppx.deriving(show) type rec query = (TLID.t, Types.fluidTokenInfo)

type props = {functions: Types.functionsType}

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

let regenerate: (Types.model, t, (TLID.t, Types.fluidTokenInfo)) => t

let numCompletions: t => int

let selectUp: t => t

let selectDown: t => t

let documentationForItem: Types.fluidAutocompleteData => option<list<Vdom.t<Types.msg>>>

let isOpened: t => bool

let updateAutocompleteVisibility: Types.model => Types.model

/* only exposed for tests */
type fullQuery = {
  tl: Types.toplevel,
  ti: Types.fluidTokenInfo,
  fieldList: list<string>,
  pipedDval: option<Types.dval>,
  queryString: string,
}

let refilter: (props, fullQuery, t, list<item>) => t
