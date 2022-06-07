// Optimized (Value)Task computation expressions for F#
// Author: Nino Floris - mail@ninofloris.com
// Copyright (c) 2019 Crowded B.V.
// Distributed under the MIT License (https://opensource.org/licenses/MIT).

// This code has been temporarily copied into Dark as an experiment:
// https://github.com/darklang/dark/issues/4099#issuecomment-1149026039
//
// Source: https://github.com/crowded/ply at a46c18df19c0d03eeea2763dca24afdd9ce26550
//
// CLEANUP remove this in favor of an updated NuGet package if/when possible,
// or revert back to using the existing NuGet package, if no progress is made.

#if NETSTANDARD2_0
namespace System.Runtime.CompilerServices

[<Sealed; System.AttributeUsage(System.AttributeTargets.All, AllowMultiple = false)>]
type IsReadOnlyAttribute() =
  inherit System.Attribute()
#endif

namespace rec Ply

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Threading.Tasks
open System.Runtime.ExceptionServices

#nowarn "1204"

module internal Internal =
  [<AllowNullLiteral>]
  type IAwaitable<'u> =
    abstract member Await : machine : byref<#IAwaitingMachine> -> unit
    abstract member Continuation : (unit -> Ply<'u>)
  and IAwaitingMachine =
    abstract member AwaitUnsafeOnCompleted<'awt when 'awt :> ICriticalNotifyCompletion> :
      awt : byref<'awt> -> unit

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Ply<'u> =
  val private value : 'u
  val private awaitable : Internal.IAwaitable<'u>
  new(result : 'u) = { value = result; awaitable = Unchecked.defaultof<_> }
  internal new(await) = { value = Unchecked.defaultof<_>; awaitable = await }
  member this.IsCompletedSuccessfully = isNull this.awaitable
  member internal this.Awaitable = this.awaitable
  member internal this.Continuation = this.awaitable.Continuation
  member this.Result =
    if this.IsCompletedSuccessfully then this.value else this.Continuation().Result

[<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
module TplPrimitives =
  open Internal

  let inline createBuilder () = AsyncValueTaskMethodBuilder<_>()

  let inline defaultof<'T> = Unchecked.defaultof<'T>

  let inline unbox<'T> (x : obj) : 'T =
    LanguagePrimitives.IntrinsicFunctions.UnboxFast x

  let ret x = Ply(result = x)
  let zero = ret ()

  type ExceptionDispatchInfo with

    member inline x.Raise() =
      x.Throw()
      defaultof<_>

  // https://github.com/dotnet/coreclr/pull/15781/files
  [<Struct; CompilerGenerated; NoComparison; NoEquality>]
  type internal ContinuationStateMachine<'u> =
    val Builder : AsyncValueTaskMethodBuilder<'u>
    val mutable private continuation : unit -> Ply<'u>

    new(awaitable) =
      { Builder = createBuilder (); continuation = fun () -> Ply(await = awaitable) }
    new(continuation) = { Builder = createBuilder (); continuation = continuation }

    interface IAwaitingMachine with
      member this.AwaitUnsafeOnCompleted(awt : byref<'awt>) =
        this.Builder.AwaitUnsafeOnCompleted(&awt, &this)

    interface IAsyncStateMachine with
      // This method is effectively deprecated on .NET Core so only .NET Fx will still call this.
      member this.SetStateMachine(csm) = this.Builder.SetStateMachine(csm)

      member this.MoveNext() =
        let mutable ex = null
        try
          let next = this.continuation ()
          if next.IsCompletedSuccessfully then
            this.Builder.SetResult(next.Result)
          else
            this.continuation <- next.Continuation
            next.Awaitable.Await(&this)
        with
        | exn -> ex <- exn

        if not (isNull ex) then this.Builder.SetException(ex)

  and [<Sealed>] internal TplAwaitable<'awt, 'u when 'awt :> ICriticalNotifyCompletion>
    (
      awaiter : 'awt,
      cont : unit -> Ply<'u>
    ) =
    let mutable awaiter = awaiter
    interface IAwaitable<'u> with
      member _.Await(csm) = csm.AwaitUnsafeOnCompleted(&awaiter)
      member _.Continuation = cont

  and [<AbstractClass>] NoEdiFSharpFunc<'t, 'u>() =
    inherit FSharpFunc<'t, 'u>()
    // Removes "--- End of stack trace from previous location where exception was thrown ---"
    // from implementers that catch exceptions for the purpose of passing them along as Edi.
    // See https://github.com/dotnet/coreclr/pull/15781/files
    interface IAsyncStateMachine with
      member _.SetStateMachine csm = failwith "not implemented"
      member _.MoveNext() = failwith "not implemented"

  // Unfortunate to have two almost identical awaitables; combining them either makes for poor stack traces or worse perf.
  and [<Sealed; CompilerGenerated>] internal PlyAwaitable<'t, 'u>
    (
      awaitable : IAwaitable<'t>,
      cont : Result<'t, ExceptionDispatchInfo> -> Ply<'u>
    ) =
    inherit NoEdiFSharpFunc<unit, Ply<'u>>()
    let mutable awaitable = awaitable

    override this.Invoke r =
      let mutable (next : Ply<'t>, edi : ExceptionDispatchInfo) = defaultof<_>, null
      // Make sure we run the inner continuation alone in this try block.
      try
        next <- awaitable.Continuation()
      with
      | ex -> edi <- ExceptionDispatchInfo.Capture(ex)

      if isNull edi then
        if next.IsCompletedSuccessfully then
          cont (Ok next.Result)
        else
          awaitable <- next.Awaitable
          Ply<_>(await = this)
      else
        cont (Error edi)

    interface IAwaitable<'u> with
      member _.Await(csm) = awaitable.Await(&csm)
      // `:> obj` here is critical to preventing fsc from generating an FSharpFunc wrapping ours for some reason.
      member this.Continuation = this :> obj |> unbox<_>

  and [<Sealed; CompilerGenerated>] internal AwaitableContinuation<'s, 't, 'u>
    (
      state : 's,
      ply : Ply<'t>,
      cont : AwaitableContinuation<'s, 't, 'u> -> Ply<'u>
    ) =
    inherit NoEdiFSharpFunc<unit, Ply<'u>>()
    let mutable ply, exceptionDispatchInfo = ply, defaultof<_>
    override this.Invoke r =
      // See if we're created with a completed ply result, sometimes happens when we need result suspension (like ediPly)
      if ply.IsCompletedSuccessfully then
        cont this
      else

        let mutable (next : Ply<'t>, edi : ExceptionDispatchInfo) =
          defaultof<_>, null
        // Make sure we run the inner continuation alone in this try block.
        try
          next <- ply.Awaitable.Continuation()
        with
        | ex -> edi <- ExceptionDispatchInfo.Capture(ex)

        if isNull edi then
          if next.IsCompletedSuccessfully then
            ply <- Ply<_>(result = next.Result)
            cont this
          else
            ply <- next
            Ply(await = this)
        else
          exceptionDispatchInfo <- edi
          cont this

    // For iterative continuations
    member this.SetAwaitable(next) = ply <- next

    member this.State = state
    member this.Value = ply
    member this.IsCompletedSuccessfully = this.Value.IsCompletedSuccessfully
    member this.Edi = exceptionDispatchInfo

    interface IAwaitable<'u> with
      member this.Await(csm) =
        // See if we're created with a completed ply result, sometimes happens when we need result suspension (like ediPly)
        // Yielding here is not great but the other option is re-introducing a hasYielded bool return value so MoveNext knows it should not unwind.
        // As this is rarely used - uply users that synchronously complete with an exception - the tradeoff is worth the cost of a dispatch.
        if ply.IsCompletedSuccessfully then
          let mutable awt = Task.Yield().GetAwaiter() in

          csm.AwaitUnsafeOnCompleted(&awt)
        else
          ply.Awaitable.Await(&csm)
      // `:> obj` here is critical to preventing fsc from generating an FSharpFunc wrapping ours for some reason.
      member this.Continuation = this :> obj |> unbox<_>

  // Not inlined to protect implementation details
  let ediPly (edi : ExceptionDispatchInfo) =
    Ply(
      await =
        (AwaitableContinuation(
          edi,
          Ply<_>(result = edi),
          fun this -> this.Value.Result.Raise()
        ))
    )

  // Runs any continuation directly, without any execution context capture, but still suspending any exceptions.
  // Exceptions outside a builder can happen here during Bind when an awaiter is completed but GetResult throws.
  let inline runUnwrappedAsPly (f : unit -> Ply<'u>) : Ply<'u> =
    try
      f ()
    with
    | ex -> ediPly (ExceptionDispatchInfo.Capture ex)

  let run (f : unit -> Ply<'u>) : ValueTask<'u> =
    // ContinuationStateMachine contains a mutable struct so we need to prevent struct copies.
    let mutable x = ContinuationStateMachine<_>(f)
    x.Builder.Start(&x)
    x.Builder.Task

  let runPly (ply : Ply<'u>) : ValueTask<'u> =
    if ply.IsCompletedSuccessfully then
      let mutable b = createBuilder ()
      b.SetResult(ply.Result)
      b.Task
    else
      let mutable x = ContinuationStateMachine<_>(ply.Awaitable)
      x.Builder.Start(&x)
      x.Builder.Task

  // This won't correctly prevent AsyncLocal leakage or SyncContext switches but it does save us the closure alloc
  // Making only this version completely alloc free for the fast path...
  // Read more here https://github.com/dotnet/coreclr/blob/027a9105/src/System.Private.CoreLib/src/System/Runtime/CompilerServices/AsyncMethodBuilder.cs#L954
  let inline runUnwrapped (f : unit -> Ply<'u>) : ValueTask<'u> =
    let next = runUnwrappedAsPly f
    if next.IsCompletedSuccessfully then
      let mutable b = createBuilder ()
      b.SetResult(next.Result)
      b.Task
    else
      runPly next

  let runAsTask (f : unit -> Ply<'u>) : Task<'u> =
    // ContinuationStateMachine contains a mutable struct so we need to prevent struct copies.
    let mutable x = ContinuationStateMachine<_>(f)
    x.Builder.Start(&x)
    x.Builder.Task.AsTask()

  let runPlyAsTask (ply : Ply<'u>) : Task<'u> =
    let task =
      if ply.IsCompletedSuccessfully then
        let mutable b = createBuilder ()
        b.SetResult(ply.Result)
        b.Task
      else
        let mutable x = ContinuationStateMachine<_>(ply.Awaitable)
        x.Builder.Start(&x)
        x.Builder.Task

    task.AsTask()

  // This won't correctly prevent AsyncLocal leakage or SyncContext switches but it does save us the closure alloc
  // Making only this version completely alloc free for the fast path...
  // Read more here https://github.com/dotnet/coreclr/blob/027a9105/src/System.Private.CoreLib/src/System/Runtime/CompilerServices/AsyncMethodBuilder.cs#L954
  let inline runUnwrappedAsTask (f : unit -> Ply<'u>) : Task<'u> =
    let next = runUnwrappedAsPly f
    if next.IsCompletedSuccessfully then
      let mutable b = createBuilder ()
      b.SetResult(next.Result)
      b.Task.AsTask()
    else
      runPlyAsTask next

  let combine (ply : Ply<unit>) (continuation : unit -> Ply<'u>) =
    if ply.IsCompletedSuccessfully then
      continuation ()
    else
      Ply(
        await =
          (AwaitableContinuation(
            continuation,
            ply,
            fun this ->
              if this.IsCompletedSuccessfully then this.State() else this.Edi.Raise()
          ))
      )

  let tryWith (continuation : unit -> Ply<'u>) (catch : exn -> Ply<'u>) =
    try
      let next = continuation ()
      if next.IsCompletedSuccessfully then
        next
      else
        Ply(
          await =
            (AwaitableContinuation(
              catch,
              next,
              fun this ->
                if this.IsCompletedSuccessfully then
                  this.Value
                else
                  this.State this.Edi.SourceException
            ))
        )
    with
    | ex -> catch ex

  let rec tryFinally (continuation : unit -> Ply<'u>) (finallyBody : unit -> unit) =
    try
      let next = continuation ()
      if next.IsCompletedSuccessfully then
        finallyBody ()
        next
      else
        Ply(
          await =
            (AwaitableContinuation(
              finallyBody,
              next,
              fun this ->
                this.State()
                if this.IsCompletedSuccessfully then this.Value else this.Edi.Raise()
            ))
        )
    with
    | ex ->
      finallyBody ()
      reraise ()

  let rec whileLoop (cond : unit -> bool) (body : unit -> Ply<unit>) =
    // As long as we never yield loops are allocation free
    if cond () then
      let next = body ()
      if next.IsCompletedSuccessfully then
        whileLoop cond body
      else
        let cont =
          AwaitableContinuation(
            struct (cond, body),
            next,
            fun this ->
              // Every resumption we go through could end in a stored exception
              if not this.IsCompletedSuccessfully then this.Edi.Raise()
              let struct (cond, body) = this.State
              let mutable awaitable = zero
              while awaitable.IsCompletedSuccessfully && cond () do
                let next = body ()
                if not next.IsCompletedSuccessfully then
                  this.SetAwaitable(next)
                  awaitable <- Ply(await = this)
              awaitable
          )
        Ply(await = cont)
    else
      zero

  let inline using (disposable : #IDisposable) (body : #IDisposable -> Ply<'u>) =
    tryFinally
      (fun () -> body disposable)
      (fun () -> if not (isNull (disposable :> obj)) then disposable.Dispose())

  let inline forLoop (sequence : 'a seq) (body : 'a -> Ply<unit>) =
    using (sequence.GetEnumerator()) (fun e ->
      whileLoop e.MoveNext (fun () -> body e.Current))

  // These types exist for backwards compatibility until 1.0
  // Some types here are supposed to always be instantiated at unit see https://github.com/dotnet/fsharp/issues/9913
  type IAwaiterMethods<'awt, 'res when 'awt :> ICriticalNotifyCompletion> =
    abstract member IsCompleted : byref<'awt> -> bool
    abstract member GetResult : byref<'awt> -> 'res

  [<IsReadOnly; Struct; NoComparison; NoEquality>]
  type TaskAwaiterMethods<'t> =
    interface IAwaiterMethods<TaskAwaiter<'t>, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt = awt.GetResult()
  and [<IsReadOnly; Struct; NoComparison; NoEquality>] UnitTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<TaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt =
        awt.GetResult()
        defaultof<_> // Always unit

  and [<IsReadOnly; Struct; NoComparison; NoEquality>] ConfiguredTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<System.Runtime.CompilerServices.ConfiguredTaskAwaitable<'t>.ConfiguredTaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt = awt.GetResult()
  and [<IsReadOnly; Struct; NoComparison; NoEquality>] ConfiguredUnitTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<ConfiguredTaskAwaitable.ConfiguredTaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt =
        awt.GetResult()
        defaultof<_> // Always unit

  and [<IsReadOnly; Struct; NoComparison; NoEquality>] YieldAwaiterMethods<'t> =
    interface IAwaiterMethods<System.Runtime.CompilerServices.YieldAwaitable.YieldAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt =
        awt.GetResult()
        defaultof<_> // Always unit

  and [<IsReadOnly; Struct; NoComparison; NoEquality>] GenericAwaiterMethods<'awt, 't when 'awt :> ICriticalNotifyCompletion> =
    interface IAwaiterMethods<'awt, 't> with
      member __.IsCompleted awt = false // Always await, this way we don't have to specialize per awaiter
      member __.GetResult awt = defaultof<_> // Always unit because we wrap this continuation to always be unit -> Ply<'u>

  and [<IsReadOnly; Struct; NoComparison; NoEquality>] ValueTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<ValueTaskAwaiter<'t>, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt = awt.GetResult()
  and [<IsReadOnly; Struct; NoComparison; NoEquality>] UnitValueTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<ValueTaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt =
        awt.GetResult()
        defaultof<_> // Always unit

  and [<IsReadOnly; Struct; NoComparison; NoEquality>] ConfiguredValueTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<ConfiguredValueTaskAwaitable<'t>.ConfiguredValueTaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt = awt.GetResult()
  and [<IsReadOnly; Struct; NoComparison; NoEquality>] ConfiguredUnitValueTaskAwaiterMethods<'t> =
    interface IAwaiterMethods<ConfiguredValueTaskAwaitable.ConfiguredValueTaskAwaiter, 't> with
      member __.IsCompleted awt = awt.IsCompleted
      member __.GetResult awt =
        awt.GetResult()
        defaultof<_> // Always unit

  type Binder<'u>() =
    // Exists for binary compatibility reasons until 1.0
    static member Await<'methods, 'awt, 't when 'methods :> IAwaiterMethods<'awt, 't>>
      (
        awt : byref<'awt>,
        cont : 't -> Ply<'u>
      ) =
      let awt = awt in

      Ply(
        await =
          TplAwaitable<_, _>(
            awt,
            fun () ->
              let mutable awt = awt in cont (defaultof<'methods>.GetResult (&awt))
          )
      )

    // We keep Await non inline to protect internals for maximum binary compatibility.
    static member Await<'awt, 't when 'awt :> ICriticalNotifyCompletion>
      (
        awt : byref<'awt>,
        cont : unit -> Ply<'u>
      ) =
      Ply(await = TplAwaitable<_, _>(awt, cont))

    static member inline Tpl< ^awt, 't when ^awt :> ICriticalNotifyCompletion and ^awt : (member get_IsCompleted :
      unit -> bool) and ^awt : (member GetResult : unit -> 't)>
      (
        awt : ^awt,
        cont : 't -> Ply<'u>
      ) =
      if (^awt : (member get_IsCompleted : unit -> bool) (awt)) then
        cont (^awt : (member GetResult : unit -> 't) (awt))
      else
        // Having GetResult here means user stack frames will get captured, as this code will get inlined into cont.
        let mutable mutAwt = awt in

        Binder<'u>
          .Await<_, _>(
            &mutAwt,
            fun () -> cont (^awt : (member GetResult : unit -> 't) (awt))
          )

    // Exists for binary compatibility reasons until 1.0
    static member PlyAwait(ply : Ply<'t>, cont : 't -> Ply<'u>) =
      Ply(
        await =
          (AwaitableContinuation(
            cont,
            ply,
            fun this ->
              if this.IsCompletedSuccessfully then
                this.State this.Value.Result
              else
                this.Edi.Raise()
          ))
      )

    static member PlyAwait
      (
        ply : Ply<'t>,
        resultCont : Result<'t, ExceptionDispatchInfo> -> Ply<'u>
      ) =
      Ply(await = (PlyAwaitable(ply.Awaitable, resultCont)))

    static member inline Ply(ply : Ply<'t>, cont : 't -> Ply<'u>) =
      if ply.IsCompletedSuccessfully then
        cont ply.Result
      else
        Binder.PlyAwait(
          ply,
          fun (r : Result<'t, ExceptionDispatchInfo>) ->
            match r with
            | Ok v -> cont v
            | Error edi -> edi.Raise()
        )

  // Supporting types to have the compiler do what we want with respect to overload resolution.
  type Id<'t> =
    class
    end

  type Default3() =
    class
    end

  type Default2() =
    inherit Default3()

  type Default1() =
    inherit Default2()

  type Bind() =
    inherit Default1()

    static member inline Invoke(m, taskLike, cont : 't -> Ply<'u>) =
      let inline call_2 (task : ^b, cont, a : ^a) =
        ((^a or ^b) : (static member Bind : _ * _ * _ -> Ply<'u>) task, cont, a)
      let inline call (task : 'b, cont, a : 'a) = call_2 (task, cont, a)
      call (taskLike, cont, m)

    static member inline Bind
      (
        configuredTask : ConfiguredTaskAwaitable<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(configuredTask.GetAwaiter(), cont)

    static member inline Bind
      (
        configuredUnitTask : ConfiguredTaskAwaitable,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(configuredUnitTask.GetAwaiter(), cont)

    static member inline Bind
      (
        yieldAwaitable : YieldAwaitable,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(yieldAwaitable.GetAwaiter(), cont)

    static member inline Bind
      (
        _ : Id<'t>,
        _ : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      failwith "Used for forcing delayed resolution."

    static member inline Bind
      (
        configuredValueTask : ConfiguredValueTaskAwaitable<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(configuredValueTask.GetAwaiter(), cont)

    static member inline Bind
      (
        configuredUnitValueTask : ConfiguredValueTaskAwaitable,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(configuredUnitValueTask.GetAwaiter(), cont)

    static member inline Bind
      (
        ply : Ply<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Bind
      ) =
      Binder<'u>.Ply (ply, cont)

  type AffineBind() =
    inherit Bind()

    static member inline Bind
      (
        taskLike : ^taskLike,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default3
      ) =
      Binder<'u>
        .Tpl<_, _>((^taskLike : (member GetAwaiter : unit -> ^awt) (taskLike)), cont)

    static member inline Bind
      (
        unitTask : Task,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default2
      ) =
      Binder<'u>.Tpl<_, _>(unitTask.GetAwaiter(), cont)

    static member inline Bind
      (
        task : Task<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(task.GetAwaiter(), cont)

    static member inline Bind
      (
        async : Async<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>((Async.StartAsTask async).GetAwaiter(), cont)

    static member inline Bind
      (
        valueTask : ValueTask<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(valueTask.GetAwaiter(), cont)

    static member inline Bind
      (
        unitValueTask : ValueTask,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(unitValueTask.GetAwaiter(), cont)

  type NonAffineBind() =
    inherit Bind()

    static member inline Bind
      (
        taskLike : ^taskLike,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default3
      ) =
      Binder<'u>
        .Tpl<_, _>((^taskLike : (member GetAwaiter : unit -> ^awt) (taskLike)), cont)

    static member inline Bind
      (
        taskLike : ^taskLike,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default3
      ) =
      let configured =
        (^taskLike : (member ConfigureAwait : bool -> ^awaitable) (taskLike, false))
      Binder<'u>
        .Tpl<_, _>(
          (^awaitable : (member GetAwaiter : unit -> ^awt) (configured)),
          cont
        )

    static member inline Bind
      (
        unitTask : Task,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default2
      ) =
      Binder<'u>.Tpl<_, _>(unitTask.ConfigureAwait(false).GetAwaiter(), cont)

    static member inline Bind
      (
        task : Task<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(task.ConfigureAwait(false).GetAwaiter(), cont)

    static member inline Bind
      (
        async : Async<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>
        .Tpl<_, _>(
          (Async.StartAsTask async).ConfigureAwait(false).GetAwaiter(),
          cont
        )

    static member inline Bind
      (
        valueTask : ValueTask<'t>,
        cont : 't -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(valueTask.ConfigureAwait(false).GetAwaiter(), cont)

    static member inline Bind
      (
        unitValueTask : ValueTask,
        cont : unit -> Ply<'u>,
        [<Optional>] _impl : Default1
      ) =
      Binder<'u>.Tpl<_, _>(unitValueTask.ConfigureAwait(false).GetAwaiter(), cont)

  type AwaitableBuilder() =
    member inline __.Delay(body : unit -> Ply<'t>) = body
    member inline __.Return(x) = ret x
    member inline __.Zero() = zero
    member inline __.Combine(ply : Ply<unit>, continuation : unit -> Ply<'u>) =
      combine ply continuation
    member inline __.While(condition : unit -> bool, body : unit -> Ply<unit>) =
      whileLoop condition body
    member inline __.TryWith(body : unit -> Ply<'t>, catch : exn -> Ply<'t>) =
      tryWith body catch
    member inline __.TryFinally(body : unit -> Ply<'t>, finallyBody : unit -> unit) =
      tryFinally body finallyBody
    member inline __.Using
      (
        disposable : #IDisposable,
        body : #IDisposable -> Ply<'u>
      ) =
      using disposable body
    member inline __.For(sequence : seq<_>, body : _ -> Ply<unit>) =
      forLoop sequence body

  type AffineBuilder() =
    inherit AwaitableBuilder()
    member inline __.ReturnFrom(task : ^taskLike) =
      Bind.Invoke(defaultof<AffineBind>, task, ret)
    member inline __.Bind(task : ^taskLike, continuation : 't -> Ply<'u>) =
      Bind.Invoke(defaultof<AffineBind>, task, continuation)

  type NonAffineBuilder() =
    inherit AwaitableBuilder()
    member inline __.ReturnFrom(task : ^taskLike) =
      Bind.Invoke(defaultof<NonAffineBind>, task, ret)
    member inline __.Bind(task : ^taskLike, continuation : 't -> Ply<'u>) =
      Bind.Invoke(defaultof<NonAffineBind>, task, continuation)



namespace FSharp.Control.Tasks

open System
open System.ComponentModel
open Ply
open Ply.TplPrimitives
open System.Threading.Tasks

[<EditorBrowsable(EditorBrowsableState.Never)>]
module Builders =
  type TaskBuilder() =
    inherit AffineBuilder()
    member inline __.Run(f : unit -> Ply<'u>) : Task<'u> = runAsTask f

  type UnitTaskBuilder() =
    inherit AffineBuilder()
    member inline __.Run(f : unit -> Ply<'u>) =
      let t = run f
      if t.IsCompletedSuccessfully then Task.CompletedTask else t.AsTask() :> Task

  type ValueTaskBuilder() =
    inherit AffineBuilder()
    member inline __.Run(f : unit -> Ply<'u>) = run f

  type UnitValueTaskBuilder() =
    inherit AffineBuilder()
    member inline __.Run(f : unit -> Ply<'u>) =
      let t = run f
      if t.IsCompletedSuccessfully then
        ValueTask()
      else
        ValueTask(t.AsTask() :> Task)

  // Backwards compat.
  [<Obsolete("Please open FSharp.Control.Tasks instead of FSharp.Control.Tasks.Builders")>]
  let task = TaskBuilder()

  [<Obsolete("Please open FSharp.Control.Tasks instead of FSharp.Control.Tasks.Builders")>]
  let unitTask = UnitTaskBuilder()

  [<Obsolete("Please open FSharp.Control.Tasks instead of FSharp.Control.Tasks.Builders")>]
  let vtask = ValueTaskBuilder()

  [<Obsolete("Please open FSharp.Control.Tasks instead of FSharp.Control.Tasks.Builders")>]
  let unitVtask = UnitValueTaskBuilder()

  module Unsafe =
    type UnsafePlyBuilder() =
      inherit AffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) = runUnwrappedAsPly f

    type UnsafeUnitTaskBuilder() =
      inherit AffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) =
        let t = runUnwrapped f
        if t.IsCompletedSuccessfully then Task.CompletedTask else t.AsTask() :> Task

    type UnsafeValueTaskBuilder() =
      inherit AffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) = runUnwrapped f

    type UnsafeUnitValueTaskBuilder() =
      inherit AffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) =
        let t = runUnwrapped f
        if t.IsCompletedSuccessfully then
          ValueTask()
        else
          ValueTask(t.AsTask() :> Task)

    // Backwards compat.
    [<Obsolete("Please open FSharp.Control.Tasks.Affine.Unsafe instead of FSharp.Control.Tasks.Builders.Unsafe")>]
    let uply = UnsafePlyBuilder()

    [<Obsolete("Please open FSharp.Control.Tasks.Affine.Unsafe instead of FSharp.Control.Tasks.Builders.Unsafe")>]
    let uunitTask = UnsafeUnitTaskBuilder()

    [<Obsolete("Please open FSharp.Control.Tasks.Affine.Unsafe instead of FSharp.Control.Tasks.Builders.Unsafe")>]
    let uvtask = UnsafeValueTaskBuilder()

    [<Obsolete("Please open FSharp.Control.Tasks.Affine.Unsafe instead of FSharp.Control.Tasks.Builders.Unsafe")>]
    let uunitVtask = UnsafeUnitValueTaskBuilder()

  module NonAffine =
    type TaskBuilder() =
      inherit NonAffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) : Task<'u> = runAsTask f

    type UnitTaskBuilder() =
      inherit NonAffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) =
        let t = run f
        if t.IsCompletedSuccessfully then Task.CompletedTask else t.AsTask() :> Task

    type ValueTaskBuilder() =
      inherit NonAffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) = run f

    type UnitValueTaskBuilder() =
      inherit NonAffineBuilder()
      member inline __.Run(f : unit -> Ply<'u>) =
        let t = run f
        if t.IsCompletedSuccessfully then
          ValueTask()
        else
          ValueTask(t.AsTask() :> Task)

    module Unsafe =
      type UnsafePlyBuilder() =
        inherit NonAffineBuilder()
        member inline __.Run(f : unit -> Ply<'u>) = runUnwrappedAsPly f

      type UnsafeUnitTaskBuilder() =
        inherit NonAffineBuilder()
        member inline __.Run(f : unit -> Ply<'u>) =
          let t = runUnwrapped f
          if t.IsCompletedSuccessfully then
            Task.CompletedTask
          else
            t.AsTask() :> Task

      type UnsafeValueTaskBuilder() =
        inherit NonAffineBuilder()
        member inline __.Run(f : unit -> Ply<'u>) = runUnwrapped f

      type UnsafeUnitValueTaskBuilder() =
        inherit NonAffineBuilder()
        member inline __.Run(f : unit -> Ply<'u>) =
          let t = runUnwrapped f
          if t.IsCompletedSuccessfully then
            ValueTask()
          else
            ValueTask(t.AsTask() :> Task)

[<AutoOpen>]
module Affine =
  open Builders

  let task = TaskBuilder()
  let unitTask = UnitTaskBuilder()
  let vtask = ValueTaskBuilder()
  let unitVtask = UnitValueTaskBuilder()

  module Unsafe =
    open Unsafe
    let uply = UnsafePlyBuilder()
    let uunitTask = UnsafeUnitTaskBuilder()
    let uvtask = UnsafeValueTaskBuilder()
    let uunitVtask = UnsafeUnitValueTaskBuilder()

/// Defines builders that are free of scheduler affinity, rejecting the SynchronizationContext or current TaskScheduler.
/// Also known as Task.ConfigureAwait(false), when building a library you want to use these builders.
module NonAffine =
  open Builders.NonAffine

  let task = TaskBuilder()
  let unitTask = UnitTaskBuilder()
  let vtask = ValueTaskBuilder()
  let unitVtask = UnitValueTaskBuilder()

  module Unsafe =
    open Unsafe
    let uply = UnsafePlyBuilder()
    let uunitTask = UnsafeUnitTaskBuilder()
    let uvtask = UnsafeValueTaskBuilder()
    let uunitVtask = UnsafeUnitValueTaskBuilder()
