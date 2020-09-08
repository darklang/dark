open Core_kernel
open Libcommon

module AWS = struct
  (* Slightly modified from
   * https://github.com/mirage/ocaml-cohttp/pull/294/files (to use
   * Buffer.add_string instead of add_bytes); see also
   * https://github.com/mirage/ocaml-uri/issues/65. It's pretty much a straight
   * up port from the Java example at
   * https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html,
   * which calls it UriEncode *)
  let url_encode (s : string) : string =
    (* Percent encode the path as s3 wants it. Uri doesn't
       encode $, or the other sep characters in a path.
       If upstream allows that we can nix this function *)
    let n = String.length s in
    let buf = Buffer.create (n * 3) in
    for i = 0 to n - 1 do
      let c = s.[i] in
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '~' | '.' | '/' ->
          Buffer.add_char buf c
      | '%' ->
          (* Sigh. Annoying we're expecting already escaped strings so ignore the escapes *)
          let is_hex = function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
                true
            | _ ->
                false
          in
          if i + 2 < n
          then
            if is_hex s.[i + 1] && is_hex s.[i + 2]
            then Buffer.add_char buf c
            else Buffer.add_string buf "%25"
      | _ ->
          Buffer.add_string buf (Printf.sprintf "%%%X" (Char.to_int c))
    done ;
    Buffer.contents buf
end

module Digit_string_helpers = struct
  (* Copied from `Core_kernel_private.Digit_string_helpers` *)
  let _unsafe_char_of_digit n = Char.unsafe_of_int (Char.to_int '0' + n)

  let write_1_digit_int bytes ~pos int =
    Bytes.unsafe_set bytes pos (_unsafe_char_of_digit int)


  let _return_tens_and_write_ones bytes ~pos int =
    let tens = int / 10 in
    let ones = int - (tens * 10) in
    write_1_digit_int bytes ~pos ones ;
    tens


  let write_2_digit_int bytes ~pos int =
    let tens = _return_tens_and_write_ones bytes ~pos:(pos + 1) int in
    write_1_digit_int bytes ~pos tens
end

let isostring_of_date (d : Time.t) : string =
  let date, sec = Time.to_date_ofday ~zone:Time.Zone.utc d in
  Date.to_string date ^ "T" ^ Time.Ofday.to_sec_string sec ^ "Z"


let isostring_of_date_basic_date (d : Time.t) : string =
  let date, _ = Time.to_date_ofday ~zone:Time.Zone.utc d in
  Date.to_string_iso8601_basic date


let isostring_of_date_basic_datetime (d : Time.t) : string =
  let date, sec = Time.to_date_ofday ~zone:Time.Zone.utc d in
  let parts = Time.Ofday.to_parts sec in
  let buf = Bytes.create (2 + 2 + 2) in
  Digit_string_helpers.write_2_digit_int buf ~pos:0 parts.hr ;
  Digit_string_helpers.write_2_digit_int buf ~pos:2 parts.min ;
  Digit_string_helpers.write_2_digit_int buf ~pos:4 parts.sec ;
  Date.to_string_iso8601_basic date ^ "T" ^ Bytes.to_string buf ^ "Z"


(* [http_date_string_of_date date] returns [date] as a string in the
 * format described here: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date
 * which is used for things like the Date header and the format for Expires in a cookie
 *)
let http_date_string_of_date (date : Time.t) : string =
  let date, sec = Time.to_date_ofday ~zone:Time.Zone.utc date in
  let parts = Time.Ofday.to_parts sec in
  let day_name =
    match Date.day_of_week date with
    | Sun ->
        "Sun"
    | Mon ->
        "Mon"
    | Tue ->
        "Tue"
    | Wed ->
        "Wed"
    | Thu ->
        "Thu"
    | Fri ->
        "Fri"
    | Sat ->
        "Sat"
  in
  let month_name = date |> Date.month |> Month.to_string in
  Format.sprintf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    day_name
    (Date.day date)
    month_name
    (Date.year date)
    parts.hr
    parts.min
    parts.sec


let date_of_isostring (str : string) : Time.t =
  (* Copied from https://github.com/janestreet/core_kernel/blob/v0.11/src/time.ml#L453 - later versions do not have a default time zone, breaking things like Date::parse *)
  let default_zone () = Time.Zone.utc in
  let find_zone zone_name =
    failwithf
      "unable to lookup Zone %s.  Try using Core.Time.of_string"
      zone_name
      ()
  in
  Time.of_string_gen ~default_zone ~find_zone str


let string_replace (search : string) (replace : string) (str : string) : string
    =
  String.Search_pattern.replace_all
    (String.Search_pattern.create search)
    ~in_:str
    ~with_:replace


let random_string length =
  let gen () =
    match Random.int (26 + 26 + 10) with
    | n when n < 26 ->
        int_of_char 'a' + n
    | n when n < 26 + 26 ->
        int_of_char 'A' + n - 26
    | n ->
        int_of_char '0' + n - 26 - 26
  in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat ~sep:"" (Array.to_list (Array.init length gen))


let list_repeat times item = List.init times ~f:(fun _ -> item)

(* Merge both maps, picking the value from the second argument if the key
 * exists in both *)
let merge_right =
  Map.merge ~f:(fun ~key v ->
      match v with
      | `Left v ->
          Some v
      | `Right v ->
          Some v
      | `Both (v1, v2) ->
          Some v2)


(* ------------------- *)
(* html *)
(* ------------------- *)
let html_escape (html : string) : string =
  String.concat_map html (fun c ->
      match c with
      | '<' ->
          "&lt;"
      | '>' ->
          "&gt;"
      | '&' ->
          "&amp;"
      (* include these for html-attribute-escaping
              even though they're not strictly necessary
              for html-escaping proper. *)
      | '"' ->
          "&quot;"
      (* &apos; doesn't work in IE.... *)
      | '\'' ->
          "&#x27;"
      | _ ->
          String.of_char c)
