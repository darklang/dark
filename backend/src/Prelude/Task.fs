module Task

open System.Threading.Tasks
open FSharp.Control.Tasks

let map (f : 'a -> 'b) (v : Task<'a>) : Task<'b> =
  task {
    let! v = v
    return f v
  }

let bind (f : 'a -> Task<'b>) (v : Task<'a>) : Task<'b> =
  task {
    let! v = v
    return! f v
  }


let flatten (list : List<Task<'a>>) : Task<List<'a>> =
  Task.WhenAll list |> map Array.toList

let foldSequentially
  (f : 'state -> 'a -> Task<'state>)
  (initial : 'state)
  (list : List<'a>)
  : Task<'state> =
  List.fold
    (fun (accum : Task<'state>) (arg : 'a) ->
      task {
        let! accum = accum
        return! f accum arg
      })
    (Task.FromResult initial)
    list

let mapSequentially (f : 'a -> Task<'b>) (list : List<'a>) : Task<List<'b>> =

  list
  |> foldSequentially
    (fun (accum : List<'b>) (arg : 'a) ->
      task {
        let! result = f arg
        return result :: accum
      })
    []
  |> map List.rev

let mapInParallel (f : 'a -> Task<'b>) (list : List<'a>) : Task<List<'b>> =
  List.map f list |> flatten

/// Call [f v], after claiming the passed semaphore. Releases the semaphore when done
let execWithSemaphore
  (semaphore : System.Threading.SemaphoreSlim)
  (f : 'a -> Task<'b>)
  (v : 'a)
  : Task<'b> =
  task {
    try
      do! semaphore.WaitAsync()
      return! f v
    finally
      semaphore.Release() |> ignore<int>
  }

let mapWithConcurrency
  (concurrencyCount : int)
  (f : 'a -> Task<'b>)
  (list : List<'a>)
  : Task<List<'b>> =
  let semaphore = new System.Threading.SemaphoreSlim(concurrencyCount)
  let f = execWithSemaphore semaphore f
  List.map f list |> flatten

let iterInParallel (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
  task {
    let! (_completedTasks : unit[]) = List.map f list |> Task.WhenAll
    return ()
  }

let iterWithConcurrency
  (concurrencyCount : int)
  (f : 'a -> Task<unit>)
  (list : List<'a>)
  : Task<unit> =
  let semaphore = new System.Threading.SemaphoreSlim(concurrencyCount)
  let f = execWithSemaphore semaphore f
  task {
    let! (_completedTasks : unit[]) = List.map f list |> Task.WhenAll
    return ()
  }

let filterSequentially (f : 'a -> Task<bool>) (list : List<'a>) : Task<List<'a>> =
  task {
    let! filtered =
      List.fold
        (fun (accum : Task<List<'a>>) (arg : 'a) ->
          task {
            let! (accum : List<'a>) = accum
            let! keep = f arg
            return (if keep then (arg :: accum) else accum)
          })
        (Task.FromResult [])
        list

    return List.rev filtered
  }

let iterSequentially (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
  List.fold
    (fun (accum : Task<unit>) (arg : 'a) ->
      task {
        do! accum // resolve the previous task before doing this one
        return! f arg
      })
    (Task.FromResult())
    list

let findSequentially (f : 'a -> Task<bool>) (list : List<'a>) : Task<Option<'a>> =
  List.fold
    (fun (accum : Task<Option<'a>>) (arg : 'a) ->
      task {
        match! accum with
        | Some v -> return Some v
        | None ->
          let! result = f arg
          return (if result then Some arg else None)
      })
    (Task.FromResult None)
    list

let filterMapSequentially
  (f : 'a -> Task<Option<'b>>)
  (list : List<'a>)
  : Task<List<'b>> =
  task {
    let! filtered =
      List.fold
        (fun (accum : Task<List<'b>>) (arg : 'a) ->
          task {
            let! (accum : List<'b>) = accum
            let! keep = f arg

            let result =
              match keep with
              | Some v -> v :: accum
              | None -> accum

            return result
          })
        (Task.FromResult [])
        list

    return List.rev filtered
  }
