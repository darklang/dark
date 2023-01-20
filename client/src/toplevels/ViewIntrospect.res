open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

module TL = Toplevel
module B = BlankOr

module Msg = AppTypes.Msg
type msg = AppTypes.msg

let tw = Attrs.class
let tw2 = (c1, c2) => Attrs.class(`${c1} ${c2}`)

let fieldsStyle = %twc("block w-max text-grey8 ml-[1.375rem]")
let fieldStyle = %twc("block text-xs text-grey7")
let typeStyle = %twc("inline-block ml-2 before:content-[':'] before:mr-2")
let nameStyle = %twc("inline-block")
let refBlockStyle = %twc(
  "block box-content relative min-w-[7.5rem] my-2 mx-0 p-2 bg-black3 text-grey8 text-[13.333px] first:mt-0 before:absolute before:text-2xl before:top-[calc(50%-15px)] hover:cursor-pointer hover:text-[#3a839e] hover:bg-black1 before:font-icons before:font-black before:text-2xl before:text-black3 before:hover:text-black1"
)
let specStyle = %twc("inline-block ml-8 pt-0 pb-0.5 px-2 w-max first:ml-0")
let arrowRefersTo = %twc("before:content-arrowRight before:-left-6")
let arrowUsedIn = %twc("before:content-arrowLeft before:-left-5")

let dbColsView = (cols: list<PT.DB.Col.t>): Html.html<msg> => {
  let colView = (col: PT.DB.Col.t) =>
    switch col {
    | {name: Some(name), typ: Some(typ), _} =>
      let html = Html.div(
        list{tw(fieldStyle)},
        list{
          Html.div(list{tw(nameStyle)}, list{Html.text(name)}),
          Html.div(list{tw(typeStyle)}, list{Html.text(DType.type2str(typ))}),
        },
      )

      Some(html)
    | _ => None
    }

  Html.div(list{tw(fieldsStyle)}, List.filterMap(~f=colView, cols))
}

let fnParamsView = (params: list<PT.UserFunction.Parameter.t>): Html.html<msg> => {
  let paramView = (p: PT.UserFunction.Parameter.t) => {
    let name = Html.span(
      list{Attrs.classList(list{(nameStyle, true), (%twc("text-red italic"), p.name == "")})},
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
      list{Attrs.classList(list{(typeStyle, true), (%twc("text-red italic"), p.typ == None)})},
      list{
        Html.text(
          switch p.typ {
          | Some(v) => DType.type2str(v)
          | None => "no type"
          },
        ),
      },
    )

    Html.div(list{tw(fieldStyle)}, list{name, ptype})
  }

  Html.div(list{tw(fieldsStyle)}, List.map(~f=paramView, params))
}

let packageFnParamsView = (params: list<PT.Package.Parameter.t>): Html.html<msg> => {
  let paramView = (p: PT.Package.Parameter.t) => {
    let name = Html.span(list{Attrs.classList(list{(nameStyle, true)})}, list{Html.text(p.name)})
    let ptype = Html.span(
      list{Attrs.classList(list{(typeStyle, true)})},
      list{Html.text(DType.type2str(p.typ))},
    )

    Html.div(list{tw(fieldStyle)}, list{name, ptype})
  }

  Html.div(list{tw(fieldsStyle)}, List.map(~f=paramView, params))
}

let fnReturnTypeView = (returnType: option<DType.t>): Html.html<msg> =>
  switch returnType {
  | Some(v) =>
    let typeStr = DType.type2str(v)
    Html.div(
      list{},
      list{
        Html.text("Returns"),
        Html.span(list{tw(%twc("inline-block ml-2"))}, list{Html.text(typeStr)}),
      },
    )
  | None => Vdom.noNode
  }

let hoveringRefProps = (originTLID: TLID.t, originIDs: list<id>, ~key: string) => list{
  EventListeners.eventNoPropagation(
    ~key=key ++ ("-in_" ++ TLID.toString(originTLID)),
    "mouseenter",
    _ => Msg.SetHoveringReferences(originTLID, originIDs),
  ),
  EventListeners.eventNoPropagation(
    ~key=key ++ ("-out_" ++ TLID.toString(originTLID)),
    "mouseleave",
    _ => Msg.SetHoveringReferences(originTLID, list{}),
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
        Attrs.id("db"),
        Attrs.classes([
          refBlockStyle,
          {direction == "refers-to" ? arrowRefersTo : arrowUsedIn},
          direction,
        ]),
        EventListeners.eventNoPropagation(
          ~key="ref-db-link" ++ TLID.toString(tlid),
          "click",
          _ => Msg.GoTo(FocusedDB(tlid, true)),
        ),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-db-hover"),
    ),
    list{
      Html.div(
        list{tw(%twc("flex items-center w-full"))},
        list{
          Icons.fontAwesome(~style=%twc("text-blue"), "database"),
          Html.span(list{tw(%twc("pl-2"))}, list{Html.text(name)}),
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
  | Some(F(_, m)) => Html.div(list{tw(specStyle)}, list{Html.text(m)})
  }
  let space = Spec.space(spec)->B.toString
  let name = Spec.name(spec)->B.toString
  Html.div(
    list{
      Attrs.id("handler"),
      Attrs.classes([
        refBlockStyle,
        "ref-block",
        {direction == "refers-to" ? arrowRefersTo : arrowUsedIn},
        %twc("flex flex-row justify-start"),
        direction,
      ]),
      EventListeners.eventNoPropagation(
        ~key="ref-handler-link" ++ TLID.toString(tlid),
        "click",
        _ => Msg.GoTo(FocusedHandler(tlid, None, true)),
      ),
      ...hoveringRefProps(originTLID, originIDs, ~key="ref-handler-hover"),
    },
    list{
      Html.div(list{tw2(specStyle, %twc("text-blue"))}, list{Html.text(space)}),
      Html.div(list{tw(specStyle)}, list{Html.text(name)}),
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
    Icons.darkIcon(~style=%twc("text-blue"), "fn"),
    Html.span(list{tw(%twc("inline-block pl-2"))}, list{Html.text(name)}),
  }
  Html.div(
    Belt.List.concat(
      list{
        Attrs.classes([
          refBlockStyle,
          {direction == "refers-to" ? arrowRefersTo : arrowUsedIn},
          direction,
        ]),
        EventListeners.eventNoPropagation(
          ~key="ref-fn-link" ++ TLID.toString(tlid),
          "click",
          _ => Msg.GoTo(FocusedFn(tlid, None)),
        ),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-fn-hover"),
    ),
    list{
      Html.div(list{tw(%twc("flex w-full items-center"))}, header),
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
  returnType: BlankOr.t<DType.t>,
  direction: string,
): Html.html<msg> => {
  // Spec is here: https://www.notion.so/darklang/PM-Function-References-793d95469dfd40d5b01c2271cb8f4a0f
  let header = list{
    Icons.fontAwesome(~style=%twc("text-grey7"), "box-open"),
    Html.span(list{tw(%twc("inline-block pl-2"))}, list{Html.text(name)}),
  }
  Html.div(
    list{
      Attrs.id("pkg-fn"),
      Attrs.classes([
        refBlockStyle,
        {direction == "refers-to" ? arrowRefersTo : arrowUsedIn},
        "ref-block ",
        direction,
      ]),
      EventListeners.eventNoPropagation(
        ~key="ref-fn-link" ++ TLID.toString(tlid),
        "click",
        _ => Msg.GoTo(FocusedPackageManagerFn(tlid)),
      ),
      ...hoveringRefProps(originTLID, originIDs, ~key="ref-fn-hover"),
    },
    list{
      Html.div(list{Attrs.id("fnheader"), tw(%twc("flex w-full items-center"))}, header),
      packageFnParamsView(params),
      fnReturnTypeView(B.toOption(returnType)),
    },
  )
}

let typeView = (
  originTLID: TLID.t,
  originIDs: list<id>,
  tlid: TLID.t,
  name: string,
  _version: int,
  direction: string,
): Html.html<msg> => {
  let header = list{
    Icons.darkIcon(~style=%twc("text-blue"), "types"),
    Html.span(list{tw(%twc("inline-block pl-2"))}, list{Html.text(name)}),
  }

  Html.div(
    Belt.List.concat(
      list{
        Attrs.classes([
          refBlockStyle,
          {direction == "refers-to" ? arrowRefersTo : arrowUsedIn},
          "typ ",
          direction,
        ]),
        EventListeners.eventNoPropagation(
          ~key="ref-typ-link" ++ TLID.toString(tlid),
          "click",
          _ => Msg.GoTo(FocusedType(tlid)),
        ),
      },
      hoveringRefProps(originTLID, originIDs, ~key="ref-typ-hover"),
    ),
    list{Html.div(list{tw(%twc("flex w-full items-center"))}, header)},
  )
}

let renderView = (originalTLID, direction, (tl, originalIDs)) =>
  switch tl {
  | TLDB({tlid, name, cols, _}) => dbView(originalTLID, originalIDs, tlid, name, cols, direction)
  | TLHandler({tlid, spec, _}) => handlerView(originalTLID, originalIDs, tlid, spec, direction)
  | TLFunc({tlid, name, parameters, returnType, _}) =>
    fnView(originalTLID, originalIDs, tlid, name, parameters, Some(returnType), direction)
  | TLPmFunc(pFn) =>
    let name = FQFnName.PackageFnName.toString(pFn.name)
    packageFnView(
      originalTLID,
      originalIDs,
      pFn.tlid,
      name,
      pFn.parameters,
      BlankOr.newF(pFn.returnType),
      direction,
    )
  | TLType({tlid, name, version, _}) if name != "" =>
    typeView(originalTLID, originalIDs, tlid, name, version, direction)
  | _ => Vdom.noNode
  }

let allUsagesView = (tlid: TLID.t, uses: list<toplevel>, refs: list<(toplevel, list<id>)>): list<
  Html.html<msg>,
> => {
  let refersTo = List.map(~f=renderView(tlid, "refers-to"), refs)
  let usedIn = List.map(~f=use => renderView(tlid, "used-in")((use, list{})), uses)

  Belt.List.concat(usedIn, refersTo)
}
