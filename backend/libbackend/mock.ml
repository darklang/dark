open Core_kernel
module StringMap = String.Map

let strs = ref StringMap.empty

let clear = strs := StringMap.empty

let set_string name value : unit =
  strs := StringMap.set ~key:name ~data:value !strs


let get_string name default : string =
  match StringMap.find !strs name with None -> default | Some v -> v
