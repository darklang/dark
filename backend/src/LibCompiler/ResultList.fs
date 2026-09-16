// ResultList.fs - Sequential helpers for list/result transforms
//
// Provides order-preserving sequential mapping helpers for compiler passes.

module ResultList

/// Map over a list sequentially, returning first error
let mapResults (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok result -> loop (result :: acc) rest
    loop [] items
