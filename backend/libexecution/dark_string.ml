open Core_kernel

type t = string

(* from http://erratique.ch/software/uucp/doc/Uucp.Case.html#caseexamples *)
let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let rec add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self ->
        Uutf.Buffer.add_utf_8 b u
    | `Uchars us ->
        List.iter us (Uutf.Buffer.add_utf_8 b)
  in
  Uutf.String.fold_utf_8 add_map () s ;
  Buffer.contents b


module Character = struct
  type t = string

  let uppercase = cmap_utf_8 Uucp.Case.Map.to_upper

  let lowercase = cmap_utf_8 Uucp.Case.Map.to_lower

  let equal a b = a = b

  let compare a b = Pervasives.compare a b
end

let of_utf8 (s : string) : t option =
  (* the decoder has mutable state *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec validate_string () =
    match Uutf.decode decoder with
    | `Uchar ch when ch = Uchar.min ->
        `Err (* U+0000 is rejected by postgres *)
    | `Uchar _ ->
        validate_string ()
    | `End ->
        `Ok
    | `Await ->
        validate_string ()
    | `Malformed _ ->
        `Err
  in
  match validate_string () with `Ok -> Some s | `Err -> None


let to_utf8 t = t

let graphemes t =
  t
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc seg -> seg :: acc) []
  |> List.rev


let length t =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> 1 + acc) 0 t


let uppercase = cmap_utf_8 Uucp.Case.Map.to_upper

let lowercase = cmap_utf_8 Uucp.Case.Map.to_lower

let equal a b =
  let an = Uunf_string.normalize_utf_8 `NFC a in
  let bn = Uunf_string.normalize_utf_8 `NFC b in
  an = bn


let compare a b =
  let an = Uunf_string.normalize_utf_8 `NFC a in
  let bn = Uunf_string.normalize_utf_8 `NFC b in
  Pervasives.compare an bn
