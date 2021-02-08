//
// let matches_event_desc (d : string * string * string) (h : handler) : bool =
//   let space, name, modifier = d in
//   module_for h = Some space
//   && event_name_for h = Some name
//   && modifier_for h = Some modifier
