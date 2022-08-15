open Prelude
module TL = Toplevel
module B = BlankOr

// hmmm: what is this?
// Oh, I think it's to display usages!

type msg = AppTypes.msg

let dbColsView = (cols: list<PT.DB.Col.t>): Html.html<msg> => {
  let colView = (col: PT.DB.Col.t) =>
    switch col {
    | {name: Some(name), typ: Some(typ), _} =>
      let html = Html.div(
        list{Html.class'("field")},
        list{
          Html.div(list{Html.class'("name")}, list{Html.text(name)}),
          Html.div(list{Html.class'("type")}, list{Html.text(DType.tipe2str(typ))}),
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
      list{Html.classList(list{("name", true), ("has-blanks", p.name == "")})},
      list{
        Html.text(
          if p.name == "" {
            "no name"
          } else {
            p.name
          },
        ),
      },
    )

    let ptype = Html.span(
      list{Html.classList(list{("type", true), ("has-blanks", p.typ == None)})},
      list{
        Html.text(
          switch p.typ {
          | Some(v) => DType.tipe2str(v)
          | None => "no type"
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
      list{Html.text(DType.tipe2str(p.tipe))},
    )

    Html.div(list{Html.class'("field")}, list{name, ptype})
  }

  Html.div(list{Html.class'("fields")}, List.map(~f=paramView, params))
}

let fnReturnTypeView = (returnType: option<DType.t>): Html.html<msg> =>
  switch returnType {
  | Some(v) =>
    let typeStr = DType.tipe2str(v)
    Html.div(
      list{},
      list{Html.text("Returns "), Html.span(list{Html.class'("type")}, list{Html.text(typeStr)})},
    )
  | None => Vdom.noNode
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
  | Some(F(_, "_")) | Some(F(_, "")) | Some(Blank(_)) | None => Vdom.noNode
  | Some(F(_, m)) => Html.div(list{Html.class'("spec")}, list{Html.text(m)})
  }
  let space = Spec.space(spec)->B.toString
  let name = Spec.name(spec)->B.toString

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
      Html.div(list{Html.class'("spec")}, list{Html.text(name)}),
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
  returnType: option<DType.t>,
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
      fnReturnTypeView(returnType),
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
      fnReturnTypeView(B.toOption(returnTipe)),
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
  | TLDB({tlid, name, cols, _}) => dbView(originalTLID, originalIDs, tlid, name, cols, direction)
  | TLHandler({tlid, spec, _}) => handlerView(originalTLID, originalIDs, tlid, spec, direction)
  | TLFunc({tlid, name, parameters, returnType, _}) =>
    fnView(originalTLID, originalIDs, tlid, name, parameters, Some(returnType), direction)
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
  | TLTipe({tlid, name, version, _}) if name != "" =>
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
