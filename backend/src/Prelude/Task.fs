module Task

open System.Threading.Tasks

let map (f : 'a -> 'b) (v : Task<'a>) : Task<'b> =
  task {
    let! v = v
    return f v
  }

/// `f ()`, started with no event loop to come back to: what it awaits inside resumes on the
/// pool, and only what it answers lands on the loop awaiting it. For I/O at the edges.
let offLoop (f : unit -> Task<'a>) : Task<'a> =
  match System.Threading.SynchronizationContext.Current with
  | null -> f ()
  | loop ->
    System.Threading.SynchronizationContext.SetSynchronizationContext null
    try
      f ()
    finally
      System.Threading.SynchronizationContext.SetSynchronizationContext loop


let flatten (list : List<Task<'a>>) : Task<List<'a>> =
  Task.WhenAll list |> map Array.toList

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
