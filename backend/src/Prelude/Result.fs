module Result

open NEList

let collect (l : List<Result<'ok, 'err>>) : Result<List<'ok>, 'err> =
  List.fold
    (fun (accum : Result<List<'ok>, 'err>) (arg : Result<'ok, 'err>) ->
      match accum, arg with
      | Ok accum, Ok arg -> Ok(arg :: accum)
      | Error err, _ -> Error err
      | _, Error err -> Error err)
    (Ok [])
    l
  |> Result.map List.rev

let all (f : 'ok -> bool) (l : List<Result<'ok, 'err>>) : bool =
  List.forall
    (fun (arg : Result<'ok, 'err>) ->
      match arg with
      | Ok arg -> f arg
      | Error _ -> false)
    l

let collectNE (l : NEList<Result<'ok, 'err>>) : Result<NEList<'ok>, 'err> =
  collect (NEList.toList l) |> Result.map (NEList.ofListUnsafe "" [])

// TODO: review and remove all usages of this
// just lazy right now...
let unwrap (r : Result<'ok, 'err>) : 'ok =
  match r with
  | Ok r -> r
  | Error err ->
    System.Console.WriteLine err
    Exception.raiseInternal "TODO: failed to unwrap" []
