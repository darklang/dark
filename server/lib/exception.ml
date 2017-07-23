open Core

exception UserException of string

let raise (str : string) =
  str |> UserException |> raise
