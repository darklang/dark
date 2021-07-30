open Prelude

let viewAutocompleteItemTypes ({item; validity} : fluidAutocompleteData) :
    Types.msg Html.html =
  let args, rt = FluidAutocomplete.asTypeStrings item in
  let html =
    let returnTypeHtml =
      let returnTypeClass =
        match validity with
        | FACItemInvalidReturnType _ ->
            "invalid-culprit"
        | _ ->
            ""
      in
      [Html.span [Html.class' returnTypeClass] [Html.text rt]]
    in
    let argsHtml =
      match args with
      | [] ->
          []
      | arg0 :: rest ->
          let arg0Class =
            match validity with
            | FACItemInvalidPipedArg _ ->
                "invalid-culprit"
            | _ ->
                ""
          in
          let args =
            Html.span [Html.class' arg0Class] [Html.text arg0]
            :: List.map ~f:Html.text rest
          in
          args
          |> List.intersperse ~sep:(Html.text ", ")
          |> fun args -> [Html.text "("] @ args @ [Html.text ") -> "]
    in
    argsHtml @ returnTypeHtml
  in
  Html.span [Html.class' "types"] html


let view (ac : Types.fluidAutocompleteState) : Types.msg Html.html =
  let index = ac.index |> Option.unwrap ~default:(-1) in
  let autocompleteList =
    List.mapWithIndex ac.completions ~f:(fun i {item; validity} ->
        let class' = if validity = FACItemValid then "valid" else "invalid" in
        let highlighted = index = i in
        let name = FluidAutocomplete.asName item in
        let fnDisplayName = FluidUtil.fnDisplayName name in
        let versionDisplayName = FluidUtil.versionDisplayName name in
        let versionView =
          if String.length versionDisplayName > 0
          then Html.span [Html.class' "version"] [Html.text versionDisplayName]
          else Vdom.noNode
        in
        let types = viewAutocompleteItemTypes {item; validity} in
        Html.li
          ~unique:name
          [ Html.classList
              [ ("autocomplete-item", true)
              ; ("fluid-selected", highlighted)
              ; (class', true) ]
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewEntry.defaultPasteHandler
          ; ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                FluidMsg (FluidAutocompleteClick item))
          ; ViewUtils.eventBoth
              ~key:("ac-mousemove" ^ name)
              "mousemove"
              (fun _ -> FluidMsg (FluidUpdateDropdownIndex i)) ]
          [Html.text fnDisplayName; versionView; types])
  in
  Html.div [Html.id "fluid-dropdown"] [Html.ul [] autocompleteList]
