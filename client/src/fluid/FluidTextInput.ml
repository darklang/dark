type t = {data : string} [@@deriving show]

type inputEvent =
  { data : string
  ; inputType : string }

let fromCompositionEndEvent (raw : Web.Node.event) : t option =
  let open Json.Decode in
  let j : Js.Json.t = Obj.magic raw in
  Some {data = field "data" string j}


let fromInputEvent (raw : Web.Node.event) : t option =
  let open Json.Decode in
  let j : Js.Json.t = Obj.magic raw in
  try
    let evt =
      {data = field "data" string j; inputType = (field "inputType" string) j}
    in
    match evt with {inputType = "insertText"; data} -> Some {data} | _ -> None
  with _ -> None
