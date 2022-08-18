open Prelude
open ViewUtils

// Tea
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module RT = Runtime

let onSubmit = (~key, fn) =>
  Html.onWithOptions(
    ~key,
    "submit",
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(fn),
  )

/* This is a paste handler that allows the default behaviour, ignores
 * the incoming Msg, and does _not_ propagate the event upwords.
 *
 * This is to prevent the paste handler on `document` from eating the event */
let defaultPasteHandler = Html.onWithOptions(
  ~key="paste",
  "paste",
  {stopPropagation: true, preventDefault: false},
  Decoders.wrapDecoder(_ => Msg.IgnoreMsg("default-paste-handler")),
)

let normalEntryHtml = (placeholder: string, ac: AppTypes.AutoComplete.t): Html.html<
  AppTypes.msg,
> => {
  let toList = (acis, class', index) => List.mapWithIndex(~f=(i, item) => {
      let highlighted = index == i
      let name = Autocomplete.asName(item)
      let typeStr = Autocomplete.asTypeString(item)
      let specialClass = Autocomplete.asTypeClass(item)
      Html.li(
        ~unique=name,
        list{
          Attributes.classList(list{
            ("autocomplete-item", true),
            ("highlighted", highlighted),
            (class', true),
            (specialClass, true),
          }),
          nothingMouseEvent("mouseup"),
          defaultPasteHandler,
          nothingMouseEvent("mousedown"),
          eventNoPropagation(~key="ac-" ++ name, "click", _ => AutocompleteClick(i)),
        },
        list{
          Html.span(list{Attrs.class'("name")}, list{Html.text(name)}),
          Html.span(list{Attrs.class'("types")}, list{Html.text(typeStr)}),
        },
      )
    }, acis)

  let autocompleteList = toList(ac.completions, "valid", ac.index)
  let autocomplete = if ac.visible {
    Html.ul(list{Attributes.id("autocomplete-holder")}, autocompleteList)
  } else {
    Vdom.noNode
  }

  /* two overlapping input boxes, one to provide suggestions, one * to provide
   * the search. (Note: we used to use this, but took it out. Leaving in the
   * technical means to show "preview" values, even though it's currently
   * unused.) */
  let search = ac.value
  let searchWidth =
    search
    |> String.length
    |> (
      n =>
        if 0 == n {
          String.length(placeholder)
        } else {
          n
        }
    )

  let searchInput = Html.input'(
    list{
      Attributes.id(Defaults.entryID),
      Events.onInput(x => Msg.EntryInputMsg(x)),
      defaultPasteHandler,
      Attributes.value(search),
      Attributes.placeholder(placeholder),
      Vdom.attribute("", "spellcheck", "false"),
      Attributes.autocomplete(false),
    },
    list{},
  )

  // TODO(ian): deliberately using an empty string here
  // and changing absolutely nothing else re: the layout/width
  // here because I have no idea what the effects will be
  let suggestionSpan = Html.span(list{Attributes.id("suggestionBox")}, list{Html.text("")})

  // http://making.fiftythree.com/fluid-text-inputs/
  let fluidWidthSpan = Html.span(
    list{Attributes.id("fluidWidthSpan"), Vdom.prop("contentEditable", "true")},
    list{Html.text(search)},
  )

  let input = Html.fieldset(
    list{Attributes.id("search-container"), widthInCh(searchWidth)},
    list{searchInput, suggestionSpan, fluidWidthSpan},
  )

  Html.div(
    list{Attrs.class'("entry")},
    list{
      Html.form(list{onSubmit(~key="esm2", _ => Msg.EntrySubmitMsg)}, list{input, autocomplete}),
    },
  )
}

let viewEntry = (m: AppTypes.model): Html.html<AppTypes.msg> =>
  switch CursorState.unwrap(m.cursorState) {
  | Omnibox(pos) =>
    let styleProp = switch pos {
    | Some(p) =>
      let offset = m.canvasProps.offset
      let loc = Viewport.subPos(p, offset)
      Html.styles(list{
        ("left", string_of_int(loc.x) ++ "px"),
        ("top", string_of_int(loc.y) ++ "px"),
      })
    | None => Vdom.noProp
    }

    Html.div(list{Attrs.class'("omnibox"), styleProp}, list{normalEntryHtml("", m.complete)})
  | _ => Vdom.noNode
  }
