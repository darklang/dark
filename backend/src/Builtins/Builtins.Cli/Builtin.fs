module Builtins.Cli.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []


/// This assembly ships FOUR platforms, not one, and that separation is the point of the file.
///
/// One platform over all of it would be files, environment, subprocesses, terminal, stdin and
/// stdout in a single grant reaching eight effects. The argument for that is that splitting would
/// split the shared `Host.perform` door and the posix implementation, but that is a statement about
/// ASSEMBLIES, and a platform is a value. Four platform records over four subsets of these `Libs`
/// share every line of implementation and cost nothing at build time, because no project is added.
///
/// What forced it was `run-local-exec platforms needed`: `Darklang.Cli.Clear.execute` calls exactly
/// one builtin, `stdoutClear`, and under one `Host` platform running it meant granting files,
/// environment, processes and stdin as well. Granting was coarser than needing, by a lot, for the
/// most ordinary thing a CLI does.
module private Sets =
  /// stdout and stdin. What CLI display code actually wants, and now all it has to ask for.
  let terminal () =
    Builtin.combine
      [ Libs.Output.builtins (); Libs.Stdin.builtins (); Libs.Terminal.builtins () ]
      fnRenames

  let files () =
    Builtin.combine [ Libs.Directory.builtins (); Libs.File.builtins () ] fnRenames

  let processes () = Builtin.combine [ Libs.Execution.builtins () ] fnRenames

  let posix () =
    Builtin.combine
      [ Libs.Posix.builtins (); Libs.Environment.builtins () ]
      fnRenames


/// Printing, reading keys, terminal size and color. Reaches `stdout` and `stdin` and nothing else,
/// which is the whole reason this is its own platform.
let terminalPlatform : LibExecution.Platform.Platform =
  { name = "Terminal"
    version = 0
    description = "Printing, reading keys, terminal size and color."
    builtins = Sets.terminal ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }


/// Files and directories: `file-read` and `file-write`, scoped per path at the `Host.perform` door.
let filesPlatform : LibExecution.Platform.Platform =
  { name = "Files"
    version = 0
    description = "Reading and writing files and directories."
    builtins = Sets.files ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }


/// Subprocesses. Small, and the scariest thing here: `Process` plus `Native`, because a spawn names
/// a program and then the program is not ours any more.
let processPlatform : LibExecution.Platform.Platform =
  { name = "Process"
    version = 0
    description = "Spawning and controlling subprocesses."
    builtins = Sets.processes ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }


/// The raw syscall layer, plus the environment. Wide on purpose rather than by neglect: a raw
/// descriptor names a number instead of a resource, so it is granted whole or not at all, and
/// `Libs/Posix.fs` says why in more detail. Splitting it further would be pretending.
///
/// It is still worth having separate from the three above: a program that prints, reads a file and
/// runs a subprocess no longer has to take `chmod`, `setenv` and raw fds along with them.
let posixPlatform : LibExecution.Platform.Platform =
  { name = "Posix"
    version = 0
    description = "Raw descriptors, process facts, and the environment."
    builtins = Sets.posix ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }


/// All four, for a caller that wants this whole assembly's surface.
let platforms : List<LibExecution.Platform.Platform> =
  [ terminalPlatform; filesPlatform; processPlatform; posixPlatform ]


/// Every builtin in this assembly. Still one function, because the cost report and the seed
/// exporter want the whole assembly rather than a platform.
let builtins () =
  Builtin.combine
    [ Sets.terminal (); Sets.files (); Sets.processes (); Sets.posix () ]
    fnRenames
