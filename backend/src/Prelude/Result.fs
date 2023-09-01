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

let collectNE (l : NEList<Result<'ok, 'err>>) : Result<NEList<'ok>, 'err> =
  collect (NEList.toList l) |> Result.map (NEList.ofListUnsafe "" [])
