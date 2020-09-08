(* For some reason, Bin_io_utils was removed from core_extended in
https://github.com/janestreet/core_extended/commit/6097f23b08217ab6b9696fd846ffd27f27827f35.
I can't find online why that was.

The is deleted from that commit, and adapted for the codebase. This was licensed
with Apache license, v2.0. *)

open Core_kernel

let end_char = '\n'

let escape_char = '\\'

let escaped_end_char = 'n'

(* converts the value to a string with a newline at the end and no other newlines *)
let to_line (bin_t : 'a Bin_prot.Type_class.t) (v : 'a) : Bigstring.t =
  let bstr =
    Bin_prot.Utils.bin_dump bin_t.Bin_prot.Type_class.writer v ~header:false
  in
  let len = Bigstring.length bstr in
  let escaped_bstr = Bigstring.create ((2 * len) + 1) in
  let end_pos = ref 0 in
  let write char =
    escaped_bstr.{!end_pos} <- char ;
    incr end_pos
  in
  for i = 0 to len - 1 do
    if bstr.{i} = end_char
    then (
      write escape_char ;
      write escaped_end_char )
    else if bstr.{i} = escape_char
    then (
      write escape_char ;
      write escape_char )
    else write bstr.{i}
  done ;
  write end_char ;
  Bigstring.sub escaped_bstr ~pos:0 ~len:!end_pos


(* reads a string with no newlines (which must be the output of [to_line] without the
   trailing newline) and converts it to a value *)
let of_line (s : string) (bin_t : 'a Bin_prot.Type_class.t) : 'a =
  let len = String.length s in
  let bstr = Bigstring.create len in
  let bstr_pos = ref 0 in
  let write char =
    bstr.{!bstr_pos} <- char ;
    incr bstr_pos
  in
  let rec loop s_pos =
    if s_pos < len
    then (
      if s.[s_pos] <> escape_char
      then (
        write s.[s_pos] ;
        loop (s_pos + 1) )
      else
        let next = s.[s_pos + 1] in
        if next = escape_char
        then write escape_char
        else if next = escaped_end_char
        then write end_char
        else failwith "bug in write_robust or read_robust" ;
        loop (s_pos + 2) )
  in
  loop 0 ;
  bin_t.Bin_prot.Type_class.reader.Bin_prot.Type_class.read
    (Bigstring.sub_shared bstr ~pos:0 ~len:!bstr_pos)
    ~pos_ref:(ref 0)
