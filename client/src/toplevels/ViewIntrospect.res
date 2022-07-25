open Prelude
module TL = Toplevel
module B = BlankOr

type msg = AppTypes.msg

let dbColsView = (cols: list<PT.DB.Col.t>): Html.html<msg> => {
  let colView = col =>
    switch col {
    | (B.F(_, nm), B.F(_, ty)) =>
      let html = Html.div(
        list{Html.class'("field")},
        list{
          Html.div(list{Html.class'("name")}, list{Html.text(nm)}),
          Html.div(list{Html.class'("type")}, list{Html.text(ty)}),
        },
      )

      Some(html)
    | _ => None
    }

  Html.div(list{Html.class'("fields")}, List.filterMap(~f=colView, cols))
}

let fnParamsView = (params: list<PT.UserFunction.Parameter.t>): Html.html<msg> => {
  let paramView = (p: PT.UserFunction.Parameter.t) => {
    let name = Html.span(
      list{Html.classList(list{("name", true), ("has-blanks", BlankOr.isBlank(p.name))})},
      list{Html.text(BlankOr.valueWithDefault("no name", p.name))},
    )

    let ptype = Html.span(
      list{Html.classList(list{("type", true), ("has-blanks", BlankOr.isBlank(p.typ))})},
      list{
        Html.text(
          switch p.typ {
          | F(_, v) => Runtime.tipe2str(v)
          | Blank(_) => "no type"
          },
        ),
      },
    )

    Html.div(list{Html.class'("field")}, list{name, ptype})
  }

  Html.div(list{Html.class'("fields")}, List.map(~f=paramView, params))
}

let packageFnParamsView = (params: list<PT.Package.Parameter.t>): Html.html<msg> => {
  let paramView = (p: PT.Package.Parameter.t) => {
    let name = Html.span(list{Html.classList(list{("name", true)})}, list{Html.text(p.name)})
    let ptype = Html.span(
      list{Html.classList(list{("type", true)})},
      list{Html.text(Runtime.tipe2str(p.tipe))},
    )

    Html.div(list{Html.class'("field")}, list{name, ptype})
  }

  Html.div(list{Html.class'("fields")}, List.map(~f=paramView, params))
}

let fnReturnTipeView = (returnTipe: BlankOr.t<DType.t>): Html.html<msg> =>
  switch returnTipe {
  | F(_, v) =>
    let typeStr = Runtime.tipe2str(v)
    Html.div(
      list{},
      list{Html.text("Returns "), Html.span(list{Html.class'("type")}, list{Html.text(typeStr)})},
    )
  | Blank(_) => Vdom.noNode
  }

let hoveringRefProps = (originTLID: TLID.t, originIDs: list<id>, ~key: string) => list{
  ViewUtils.eventNoPropagation(
    ~key=key ++ ("-in_" ++ TLID.toString(originTLID)),
    "mouseenter",
    _ => SetHoveringReferences(originTLID, originIDs),
  ),
  ViewUtils.eventNoPropagation(
    ~key=key ++ ("-out_" ++ TLID.toString(originTLID)),
    "mouseleave",
    _ => SetHoveringReferences(originTLID, list{}),
  ),
}

let dbView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  name: string,
  cols: list<PT.DB.Col.t>,
  direction: string,
): Html.html<msg> =>
  Html.div(
    Belt.List.concat(
      list{
        Html.class'("ref-block db " ++ direction),
        ViewUtils.eventNoPropagation(~key="ref-db-link" ++ TLID.toString(tlid), "click", _ => GoTo(
          FocusedDB(tlid, true),
        )),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-db-hover"),
    ),
    list{
      Html.div(
        list{Html.class'("dbheader")},
        list{
          ViewUtils.fontAwesome("database"),
          Html.span(list{Html.class'("dbname")}, list{Html.text(name)}),
        },
      ),
      dbColsView(cols),
    },
  )

let handlerView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  spec: PT.Handler.Spec.t,
  direction: string,
): Html.html<msg> => {
  module Spec = PT.Handler.Spec
  let modifier_ = switch Spec.modifier(spec) {
  | Some("_") | Some("") | None => Vdom.noNode
  | Some(m) => Html.div(list{Html.class'("spec")}, list{Html.text(m)})
  }
  let space = Spec.space(spec)->Belt.Option.getWithDefault("")

  Html.div(
    list{
      Html.class'("ref-block handler " ++ direction),
      ViewUtils.eventNoPropagation(
        ~key="ref-handler-link" ++ TLID.toString(tlid),
        "click",
        _ => GoTo(FocusedHandler(tlid, None, true)),
      ),
      ...hoveringRefProps(originTLID, originIDs, ~key="ref-handler-hover"),
    },
    list{
      Html.div(list{Html.class'("spec space")}, list{Html.text(space)}),
      Html.div(list{Html.class'("spec")}, list{Html.text(Spec.name(spec))}),
      modifier_,
    },
  )
}

let fnView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  name: string,
  params: list<PT.UserFunction.Parameter.t>,
  returnTipe: BlankOr.t<DType.t>,
  direction: string,
): Html.html<msg> => {
  let header = list{
    ViewUtils.darkIcon("fn"),
    Html.span(list{Html.class'("fnname")}, list{Html.text(name)}),
  }

  Html.div(
    Belt.List.concat(
      list{
        Html.class'("ref-block fn " ++ direction),
        ViewUtils.eventNoPropagation(~key="ref-fn-link" ++ TLID.toString(tlid), "click", _ => GoTo(
          FocusedFn(tlid, None),
        )),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-fn-hover"),
    ),
    list{
      Html.div(list{Html.class'("fnheader fnheader-user")}, header),
      fnParamsView(params),
      fnReturnTipeView(returnTipe),
    },
  )
}

let packageFnView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  name: string,
  params: list<PT.Package.Parameter.t>,
  returnTipe: BlankOr.t<DType.t>,
  direction: string,
): Html.html<msg> => {
  // Spec is here: https://www.notion.so/darklang/PM-Function-References-793d95469dfd40d5b01c2271cb8f4a0f
  let header = list{
    ViewUtils.fontAwesome("box-open"),
    Html.span(list{Html.class'("fnname")}, list{Html.text(name)}),
  }

  Html.div(
    list{
      Html.class'("ref-block pkg-fn " ++ direction),
      ViewUtils.eventNoPropagation(~key="ref-fn-link" ++ TLID.toString(tlid), "click", _ => GoTo(
        FocusedPackageManagerFn(tlid),
      )),
      ...hoveringRefProps(originTLID, originIDs, ~key="ref-fn-hover"),
    },
    list{
      Html.div(list{Html.class'("fnheader fnheader-pkg")}, header),
      packageFnParamsView(params),
      fnReturnTipeView(returnTipe),
    },
  )
}

let tipeView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  name: string,
  _version: int,
  direction: string,
): Html.html<msg> => {
  let header = list{
    ViewUtils.darkIcon("type"),
    Html.span(list{Html.class'("tipename")}, list{Html.text(name)}),
  }

  Html.div(
    Belt.List.concat(
      list{
        Html.class'("ref-block tipe " ++ direction),
        ViewUtils.eventNoPropagation(
          ~key="ref-tipe-link" ++ TLID.toString(tlid),
          "click",
          _ => GoTo(FocusedType(tlid)),
        ),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-tipe-hover"),
    ),
    list{Html.div(list{Html.class'("tipeheader")}, header)},
  )
}

let renderView = (originalTLID, direction, (tl, originalIDs)) =>
  switch tl {
  | TLDB({tlid, name: F(_, name), cols, _}) =>
    dbView(originalTLID, originalIDs, tlid, name, cols, direction)
  | TLHandler({tlid, spec, _}) => handlerView(originalTLID, originalIDs, tlid, spec, direction)
  | TLFunc({tlid, metadata: {name: F(_, name), parameters, returnType, _}, ast: _}) =>
    fnView(originalTLID, originalIDs, tlid, name, parameters, returnType, direction)
  | TLPmFunc(pFn) =>
    let name = PT.FQFnName.PackageFnName.toString(pFn.name)
    packageFnView(
      originalTLID,
      originalIDs,
      pFn.tlid,
      name,
      pFn.parameters,
      BlankOr.newF(pFn.returnType),
      direction,
    )
  | TLTipe({tlid, name: F(_, name), version, definition: _}) =>
    tipeView(originalTLID, originalIDs, tlid, name, version, direction)
  | _ => Vdom.noNode
  }

let allUsagesView = (tlid: TLID.t, uses: list<toplevel>, refs: list<(toplevel, list<id>)>): list<
  Html.html<msg>,
> => {
  let refersTo = List.map(~f=renderView(tlid, "refers-to"), refs)
  let usedIn = List.map(~f=use => renderView(tlid, "used-in")((use, list{})), uses)

  Belt.List.concat(usedIn, refersTo)
}
