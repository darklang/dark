/// The smallest useful Dark executable: resolve a stored package function by name, run it, print
/// the answer.
///
/// This exists to keep one claim honest. The platform model says a set composes apart, and every
/// other binary we ship links every platform, so nothing in the tree would notice if the seams grew
/// back. This host links `Core` and nothing else. Its confinement is a LINK-TIME fact rather than a
/// policy decision: there is no `fileRead` in this binary to deny, no socket, no process spawn, no
/// builtin that writes a package. If someone puts a host effect behind a Core builtin, this stops
/// building, which is the point.
///
/// It reads the package store, because `Stdlib.List.map` is a package function and there is no
/// interesting Dark without one. The `RT.PackageManager` it is handed does carry `persistBlob`, so
/// the strict claim is not "this process cannot write" but "no builtin linked here can reach the
/// thing that would". `Builtins.Store` is not in the binary.
module SealedHost.Main

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution
module Plat = LibExecution.Platform.Platform
module PlatSet = LibExecution.Platform.PlatformSet
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Core alone. Composed here rather than through `Platforms.Sets`, deliberately: linking that
/// assembly would pull in every platform we ship and make the whole exercise a lie.
let private platformSet : LibExecution.Platform.PlatformSet =
  PlatSet.make [ Builtins.Pure.Builtin.platform ] []


let private state () : RT.ExecutionState =
  let program : RT.Program = { dbs = Map.empty }
  let notify _ _ _ _ = uply { return () }
  let reportException _ _ (metadata : Metadata) (exn : exn) =
    uply { printException "Internal error" metadata exn }

  // No `setInstancePolicy`. `createState` denies every host effect by default, and here that
  // default is also the ceiling: nothing in this process can perform one.
  { Exe.createState
      platformSet.builtins
      LibDB.PackageManager.rt
      Exe.noTracing
      reportException
      notify
      program with
      platforms = platformSet.platforms }


/// `Darklang.Stdlib.List.length` -> the location to look up. The last segment is the name, the
/// first is the owner, and what is between them is the module path.
let private locationOf (s : string) : Option<PT.PackageLocation> =
  match List.rev (s.Split('.') |> Array.toList) with
  | [] -> None
  | [ _ ] -> None
  | name :: revRest ->
    match List.rev revRest with
    | owner :: modules -> Some { owner = owner; modules = modules; name = name }
    | [] -> None


let private usage () =
  print "usage: SealedHost <Owner.Module.fnName> [args...]   (scripts/run-sealed)"
  print ""
  print "Runs one stored package function with no host access whatsoever, and prints the"
  print "result. Arguments are passed as strings; with none, the function is called with unit."
  print ""
  print "This binary links only the Core platform, so it cannot read a file, open a socket,"
  print "spawn a process, read the environment or write to the package store. Not by policy:"
  print "those builtins are not in it."


/// `print` queues; the queue is drained by a background thread, so a process that returns from
/// `main` without waiting loses whatever had not been written yet.
let private exitWith (code : int) : int =
  NonBlockingConsole.wait ()
  code


let private run (args : List<string>) : int =
  match args with
  | []
  | [ "--help" ]
  | [ "-h" ] ->
    usage ()
    exitWith 0

  | [ "--platforms" ] ->
    // The same question `dark platforms` answers, from the other end of the range: this is what a
    // one-platform executable looks like.
    platformSet.platforms
    |> List.iter (fun p ->
      let effects =
        match Plat.effectSurface p |> Set.toList with
        | [] -> "reaches nothing"
        | es -> es |> List.map LibExecution.Effects.name |> List.sort |> String.concat " "
      print $"{Plat.coordinate p}  {Plat.fnCount p} fns  [{effects}]")
    print $"fingerprint {platformSet.fingerprint}"
    exitWith 0

  | fnName :: rest ->
    match locationOf fnName with
    | None ->
      print $"'{fnName}' is not a package location; it needs at least an owner and a name"
      exitWith 1
    | Some location ->

    match (LibDB.PackageManager.pt.findFn location).Result with
    | None ->
      print $"no package function named '{fnName}'"
      exitWith 1
    | Some pkg ->

    let name = RT.FQFnName.Package(PT2RT.FQFnName.Package.toRT pkg)
    let args =
      match rest with
      | [] -> NEList.singleton RT.DUnit
      | args -> args |> List.map RT.DString |> NEList.ofListUnsafe "args" []

    let state = state ()
    match (Exe.executeFunction state name [] args).Result with
    | Ok result ->
      print (Exe.dvalToRepr state result).Result
      exitWith 0
    | Error(rte, _) ->
      // The error printer is Dark too, and it is Core-only Dark, so it runs here like anything
      // else. If it cannot, the raw error is still better than nothing.
      match (Exe.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        print s
        // The error this host produces most often, and the one worth explaining: the function
        // asked for a builtin that belongs to some other platform. Nothing denied it. It is not
        // in this binary.
        if s.Contains "Builtin." && s.Contains "couldn't be found" then
          print ""
          print "This host links only Core. That builtin belongs to another platform, so it is"
          print "not in this binary at all -- no policy denied it. Run it under `dark` instead."
      | _ -> print (string rte)
      exitWith 1


/// `run` works synchronously off `.Result`, and an unhandled exception out of `main` prints a .NET
/// stack trace, which is the wrong artifact to hand someone running a sealed binary. One catch,
/// one sentence, exit 1.
[<EntryPoint>]
let main (args : string[]) : int =
  try
    run (Array.toList args)
  with e ->
    print $"error: {e.Message}"
    exitWith 1
