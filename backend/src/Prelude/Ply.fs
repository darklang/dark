module Ply

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

type Ply<'a> = Ply.Ply<'a>
let uply = FSharp.Control.Tasks.Affine.Unsafe.uply

let map (f : 'a -> 'b) (v : Ply<'a>) : Ply<'b> =
  uply {
    let! v = v
    return f v
  }

let bind (f : 'a -> Ply<'b>) (v : Ply<'a>) : Ply<'b> =
  uply {
    let! v = v
    return! f v
  }

let toTask (v : Ply<'a>) : Task<'a> = Ply.TplPrimitives.runPlyAsTask v


// These functions are sequential versions of List/Map functions like map/iter/etc.
// They await each list item before they process the next.  This ensures each
// request in the list is processed to completion before the next one is done,
// making sure that, for example, a HttpClient call will finish before the next one
// starts. Will allow other requests to run which waiting.
module List =
  let flatten (list : List<Ply<'a>>) : Ply<List<'a>> =
    let rec loop (acc : Ply<List<'a>>) (xs : List<Ply<'a>>) =
      uply {
        let! acc = acc

        match xs with
        | [] -> return List.rev acc
        | x :: xs ->
          let! x = x
          return! loop (uply { return (x :: acc) }) xs
      }

    loop (uply { return [] }) list

  let foldSequentially
    (f : 'state -> 'a -> Ply<'state>)
    (initial : 'state)
    (list : List<'a>)
    : Ply<'state> =
    List.fold
      (fun (accum : Ply<'state>) (arg : 'a) ->
        uply {
          let! accum = accum
          return! f accum arg
        })
      (Ply initial)
      list

  let foldSequentiallyWithIndex
    (f : int -> 'state -> 'a -> Ply<'state>)
    (initial : 'state)
    (list : List<'a>)
    : Ply<'state> =
    List.fold
      (fun (accum : (Ply<int * 'state>)) (arg : 'a) ->
        uply {
          let! (i, state) = accum
          let! result = f i state arg
          return (i + 1, result)
        })
      (Ply((0, initial)))
      list
    |> map Tuple2.second


  let mapSequentially (f : 'a -> Ply<'b>) (list : List<'a>) : Ply<List<'b>> =
    list
    |> foldSequentially
      (fun (accum : List<'b>) (arg : 'a) ->
        uply {
          let! result = f arg
          return result :: accum
        })
      []
    |> map List.reverse

  let mapSequentiallyWithIndex
    (f : int -> 'a -> Ply<'b>)
    (list : List<'a>)
    : Ply<List<'b>> =
    list
    |> foldSequentiallyWithIndex
      (fun (i : int) (accum : List<'b>) (arg : 'a) ->
        uply {
          let! result = f i arg
          return result :: accum
        })
      []
    |> map List.rev

  let filterSequentially (f : 'a -> Ply<bool>) (list : List<'a>) : Ply<List<'a>> =
    uply {
      let! filtered =
        List.fold
          (fun (accum : Ply<List<'a>>) (arg : 'a) ->
            uply {
              let! (accum : List<'a>) = accum
              let! keep = f arg
              return (if keep then (arg :: accum) else accum)
            })
          (Ply [])
          list

      return List.rev filtered
    }

  let iterSequentially (f : 'a -> Ply<unit>) (list : List<'a>) : Ply<unit> =
    List.fold
      (fun (accum : Ply<unit>) (arg : 'a) ->
        uply {
          do! accum // resolve the previous task before doing this one
          return! f arg
        })
      (Ply(()))
      list

  let findSequentially (f : 'a -> Ply<bool>) (list : List<'a>) : Ply<Option<'a>> =
    List.fold
      (fun (accum : Ply<Option<'a>>) (arg : 'a) ->
        uply {
          match! accum with
          | Some v -> return Some v
          | None ->
            let! result = f arg
            return (if result then Some arg else None)
        })
      (Ply None)
      list

  let filterMapSequentially
    (f : 'a -> Ply<Option<'b>>)
    (list : List<'a>)
    : Ply<List<'b>> =
    uply {
      let! filtered =
        List.fold
          (fun (accum : Ply<List<'b>>) (arg : 'a) ->
            uply {
              let! (accum : List<'b>) = accum
              let! keep = f arg

              let result =
                match keep with
                | Some v -> v :: accum
                | None -> accum

              return result
            })
          (Ply [])
          list

      return List.rev filtered
    }

module NEList =
  let mapSequentially
    (f : 'a -> Ply<'b>)
    (list : NEList.NEList<'a>)
    : Ply<NEList.NEList<'b>> =
    uply {
      let! head = f list.head
      let! tail = List.mapSequentially f list.tail
      return NEList.ofList head tail
    }


module Map =
  let foldSequentially
    (f : 'state -> 'key -> 'a -> Ply<'state>)
    (initial : 'state)
    (dict : Map<'key, 'a>)
    : Ply<'state> =
    Map.fold
      (fun (accum : Ply<'state>) (key : 'key) (arg : 'a) ->
        uply {
          let! (accum : 'state) = accum
          return! f accum key arg
        })
      (Ply(initial))
      dict

  let mapSequentially
    (f : 'a -> Ply<'b>)
    (dict : Map<'key, 'a>)
    : Ply<Map<'key, 'b>> =
    foldSequentially
      (fun (accum : Map<'key, 'b>) (key : 'key) (arg : 'a) ->
        uply {
          let! result = f arg
          return Map.add key result accum
        })
      Map.empty
      dict

  let filterSequentially
    (f : 'key -> 'a -> Ply<bool>)
    (dict : Map<'key, 'a>)
    : Ply<Map<'key, 'a>> =
    foldSequentially
      (fun (accum : Map<'key, 'a>) (key : 'key) (arg : 'a) ->
        uply {
          let! keep = f key arg
          return (if keep then (Map.add key arg accum) else accum)
        })
      Map.empty
      dict

  let filterMapSequentially
    (f : 'key -> 'a -> Ply<Option<'b>>)
    (dict : Map<'key, 'a>)
    : Ply<Map<'key, 'b>> =
    foldSequentially
      (fun (accum : Map<'key, 'b>) (key : 'key) (arg : 'a) ->
        uply {
          let! keep = f key arg

          let result =
            match keep with
            | Some v -> Map.add key v accum
            | None -> accum

          return result
        })
      Map.empty
      dict

// TODO : Add more functions
module Result =
  let mapSequentially
    (f : 'a -> Ply<'b>)
    (result : Result<'a, 'err>)
    : Ply<Result<'b, 'err>> =
    match result with
    | Ok v -> map (fun v -> Ok v) (f v)
    | Error err -> Ply (Error err)

// TODO : Add more functions
module Option =
  let mapSequentially
    (f : 'a -> Ply<'b>)
    (option : Option<'a>)
    : Ply<Option<'b>> =
    match option with
    | Some v -> map (fun v -> Some v) (f v)
    | None -> Ply None

