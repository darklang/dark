open Core

exception UserException of string

let raise (str : string) =
  UserException str |> raise
