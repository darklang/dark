open Prelude
open ViewUtils

// Tea
module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

module RT = Runtime

let tw = Attrs.class

let onSubmit = (~key, fn) =>
  Events.onWithOptions(
    ~key,
    "submit",
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(fn),
  )

/* This is a paste handler that allows the default behaviour, ignores
 * the incoming Msg, and does _not_ propagate the event upwords.
 *
 * This is to prevent the paste handler on `document` from eating the event */
let defaultPasteHandler = Events.onWithOptions(
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
          Attrs.classList(list{
            //autocomplete-item
            (
              %twc(
                "py-0 px-1 bg-transparent flex flex-row justify-between items-baseline w-[calc(100%-10px)] hover:bg-highlight-color"
              ),
              true,
            ),
            //highlighted
            (%twc("bg-highlight-color"), highlighted),
            (class', true),
            (specialClass, true),
          }),
          EventListeners.nothingMouseEvent("mouseup"),
          defaultPasteHandler,
          EventListeners.nothingMouseEvent("mousedown"),
          EventListeners.eventNoPropagation(~key="ac-" ++ name, "click", _ => Msg.AutocompleteClick(
            i,
          )),
        },
        list{
          //name
          Html.span(
            list{Attrs.class(%twc("flex-1 text-xxs h-3.5 mx-1 px-2"))},
            list{Html.text(name)},
          ),
          //types
          Html.span(
            list{Attrs.class(%twc("lowercase font-code text-[80%] text-grey8"))},
            list{Html.text(typeStr)},
          ),
        },
      )
    }, acis)

  let autocompleteList = toList(ac.completions, "valid", ac.index)
  let autocomplete = if ac.visible {
    Html.ul(
      list{
        Attrs.id("autocomplete-holder"),
        Attrs.class(
          %twc(
            "text-[11.3px] max-h-[90px] overflow-y-scroll overflow-x-hidden absolute text-start w-[350px] z-[1] bg-black3 rounded-b"
          ),
        ),
      },
      autocompleteList,
    )
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

  let searchInput = Html.input(
    list{
      Attrs.id(Defaults.entryID),
      Events.onInput(x => Msg.EntryInputMsg(x)),
      defaultPasteHandler,
      Attrs.value(search),
      Attrs.placeholder(placeholder),
      Vdom.attribute("", "spellcheck", "false"),
      Attrs.autocomplete(false),
      Attrs.class(
        %twc(
          "bg-transparent text-transparent caret-grey8 relative text-left align-baseline  w-full"
        ),
      ),
    },
    list{},
  )

  // TODO(ian): deliberately using an empty string here
  // and changing absolutely nothing else re: the layout/width
  // here because I have no idea what the effects will be
  let suggestionSpan = Html.span(
    list{Attrs.id("suggestionBox"), tw(%twc("text-[545454] absolute left-0 top-0 w-full"))},
    list{Html.text("")},
  )

  // http://making.fiftythree.com/fluid-text-inputs/
  let fluidWidthSpan = Html.span(
    list{
      Attrs.id("fluidWidthSpan"),
      tw(%twc("text-grey8 absolute left-0 top-0 w-full")),
      Vdom.prop("contentEditable", "true"),
    },
    list{Html.text(search)},
  )

  let inCh = (w: int): string => w |> string_of_int |> (s => s ++ "ch")

  let widthInCh = (w: int): Vdom.property<msg> => w |> inCh |> Attrs.style("width")

  let input = Html.fieldset(
    list{
      Attrs.id("search-container"),
      tw(%twc("bg-black2 border-none m-0 p-0 relative")),
      widthInCh(searchWidth),
    },
    list{searchInput, suggestionSpan, fluidWidthSpan},
  )

  Html.div(
    //entry
    list{Attrs.class(%twc("cursor-default bg-black2 text-white1 mx-1 w-auto"))},
    list{
      Html.form(list{onSubmit(~key="esm2", _ => Msg.EntrySubmitMsg)}, list{input, autocomplete}),
    },
  )
}

let omniboxEntryHtml = (placeholder: string, ac: AppTypes.AutoComplete.t): Html.html<
  AppTypes.msg,
> => {
  let toList = (acis, class', index) => List.mapWithIndex(~f=(i, item) => {
      let highlighted = index == i
      let name = Autocomplete.asName(item)
      let specialClass = Autocomplete.asTypeClass(item)
      Html.li(
        ~unique=name,
        list{
          Attrs.classList(list{
            //autocomplete-item
            (%twc("text-code py-0 px-6 leading-normal"), true),
            //highlighted
            (%twc("bg-highlight-color"), highlighted),
            (class', true),
            (specialClass, true),
          }),
          EventListeners.nothingMouseEvent("mouseup"),
          defaultPasteHandler,
          EventListeners.nothingMouseEvent("mousedown"),
          EventListeners.eventNoPropagation(~key="ac-" ++ name, "click", _ => Msg.AutocompleteClick(
            i,
          )),
        },
        list{
          //name
          Html.span(list{Attrs.class("text-white1")}, list{Html.text(name)}),
        },
      )
    }, acis)

  let autocompleteList = toList(ac.completions, "valid", ac.index)
  let autocomplete = if ac.visible {
    Html.ul(
      list{
        Attrs.id("autocomplete-holder"),
        tw(%twc("w-full bg-black2 overflow-y-visible list-none rounded-b-md ")),
      },
      autocompleteList,
    )
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

  let searchInput = Html.input(
    list{
      //entry-box
      tw(
        %twc(
          "bg-transparent text-transparent caret-grey8 relative text-left align-baseline  w-full"
        ),
      ),
      Attrs.id(Defaults.entryID),
      Events.onInput(x => Msg.EntryInputMsg(x)),
      defaultPasteHandler,
      Attrs.value(search),
      Attrs.placeholder(placeholder),
      Vdom.attribute("", "spellcheck", "false"),
      Attrs.autocomplete(false),
    },
    list{},
  )

  // TODO(ian): deliberately using an empty string here
  // and changing absolutely nothing else re: the layout/width
  // here because I have no idea what the effects will be
  let suggestionSpan = Html.span(list{Attrs.id("suggestionBox")}, list{Html.text("")})

  // http://making.fiftythree.com/fluid-text-inputs/ -This page can't be found should we remove this comment?
  let fluidWidthSpan = Html.span(
    list{
      Attrs.id("fluidWidthSpan"),
      tw(
        %twc(
          "absolute left-0 top-0 w-full box-border ml-6 before:inline-block before:-mt-2 before:-ml-[20.4px] before:mr-1 before:content-search before:font-icons before:font-black before:text-grey2 before:text-base"
        ),
      ),
      Vdom.prop("contentEditable", "true"),
    },
    list{Html.text(search)},
  )

  let inCh = (w: int): string => w |> string_of_int |> (s => s ++ "ch")

  let widthInCh = (w: int): Vdom.property<msg> => w |> inCh |> Attrs.style("width")

  let input = Html.fieldset(
    list{
      Attrs.id("search-container"),
      tw(%twc("relative min-w-[400px] bg-black3 py-0 px-6 rounded-t-md border-none m-0")),
      widthInCh(searchWidth),
    },
    list{searchInput, suggestionSpan, fluidWidthSpan},
  )

  Html.div(
    //entry %twc("cursor-default bg-black2 text-white1 mx-1 w-auto")
    list{Attrs.class("")},
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
      Attrs.styles(list{
        ("left", string_of_int(loc.x) ++ "px"),
        ("top", string_of_int(loc.y) ++ "px"),
      })
    | None => Vdom.noProp
    }

    Html.div(
      list{tw(%twc("fixed top-[200px] left-[40%] -mt-2 text-lg  ")), styleProp},
      list{omniboxEntryHtml("", m.complete)},
    )
  | _ => Vdom.noNode
  }
