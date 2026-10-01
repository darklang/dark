/// The seam between this runtime and the native compiler (pbiggar/darklang-compiler,
/// vendored as LibCompiler). Deliberately thin: the compiler parses Dark source and
/// implements the stdlib itself, so everything about WHICH source to hand it (a fn's
/// closure, pretty-printed; the entry expression; synthesized arguments) lives in
/// Dark, in `Darklang.Compiler.*`. This file only turns named source units into a
/// binary and runs it with a deadline.
module Builtins.Compiler.Libs.Compiler

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType

let private target : Lazy<Result<Platform.Target, string>> =
  lazy (Platform.detectHostTarget ())

/// The compiler's stdlib is parsed, checked and lowered once per process; every
/// compile reuses it.
let private stdlib : Lazy<Result<CompilationContexts.StdlibResult, string>> =
  lazy
    (match target.Value with
     | Error e -> Error e
     | Ok t -> StdlibCompilation.buildStdlib t)

let private purposeOf (s : string) : Result<NameSyntax.SourceUnitPurpose, string> =
  match s with
  | "executable" -> Ok NameSyntax.SourceUnitPurpose.Executable
  | "library" -> Ok NameSyntax.SourceUnitPurpose.Library
  | "package" -> Ok NameSyntax.SourceUnitPurpose.Package
  | other -> Error $"unknown unit purpose {other} (executable | library | package)"

/// Compile named source units to a binary. Mode `eval` renders the entry
/// expression's value the way the compiler's eval mode does; `program` requires an
/// Int/Unit entry.
let private compileUnits
  (units : List<string * string * string>)
  (mode : string)
  : Result<byte[], string> =
  match stdlib.Value with
  | Error e -> Error $"stdlib: {e}"
  | Ok stdlib ->
    let purposes = units |> List.map (fun (_, p, _) -> purposeOf p)
    match purposes |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
    | Some e -> Error e
    | None ->
      let sources =
        List.zip units purposes
        |> List.map (fun ((name, _, source), p) ->
          ({ CompilationContexts.SourceUnit.Name = name
             CompilationContexts.SourceUnit.Purpose = (match p with Ok p -> p | Error _ -> NameSyntax.SourceUnitPurpose.Library)
             CompilationContexts.SourceUnit.Source = source }))
      match AST.NonEmptyList.tryFromList sources with
      | None -> Error "no source units"
      | Some sources ->
        let request : CompilationContexts.CompileRequest =
          { Context = CompilationContexts.StdlibOnly stdlib
            Mode =
              (if mode = "program" then CompilerOptions.FullProgram
               else CompilerOptions.TestExpression)
            Sources = sources
            AllowInternal = false
            // DARK_COMPILER_VERBOSITY=1 prints the compiler's pass names, 2 adds
            // timings, 3 dumps its IRs; for finding where a compile spends its time.
            Verbosity =
              (match System.Environment.GetEnvironmentVariable "DARK_COMPILER_VERBOSITY" with
               | null | "" -> 0
               | v -> (try int v with _ -> 0))
            // DARK_COMPILER_NO_INLINE=1 turns the compiler's inlining and the
            // specializations that depend on it off: for telling a hang in those
            // passes from one elsewhere.
            Options =
              (if System.Environment.GetEnvironmentVariable "DARK_COMPILER_NO_INLINE" = "1" then
                 { CompilerOptions.defaultOptions with DisableInlining = true }
               else CompilerOptions.defaultOptions)
            PackageValues = CompilationContexts.emptyPackageValueCatalog
            PassTimingRecorder = None
            Session = None }
        try
          (CompilerLibrary.compile request).Result
        with e ->
          Error $"compiler threw: {e.Message}"

/// Run a compiled binary with a deadline, from a throwaway working directory.
/// (exit code, stdout, stderr); exit code -1 means the deadline passed.
let private runBinary (binary : byte[]) (timeoutMs : int) : int * string * string =
  let dir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dark-native-" + System.Guid.NewGuid().ToString("N"))
  System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
  let path = System.IO.Path.Combine(dir, "a.out")
  try
    System.IO.File.WriteAllBytes(path, binary)
    System.IO.File.SetUnixFileMode(
      path,
      System.IO.UnixFileMode.UserRead ||| System.IO.UnixFileMode.UserWrite ||| System.IO.UnixFileMode.UserExecute
    )
    let psi = System.Diagnostics.ProcessStartInfo(path)
    psi.WorkingDirectory <- dir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.RedirectStandardInput <- true
    psi.UseShellExecute <- false
    use p = System.Diagnostics.Process.Start psi
    p.StandardInput.Close()
    let stdout = p.StandardOutput.ReadToEndAsync()
    let stderr = p.StandardError.ReadToEndAsync()
    if p.WaitForExit timeoutMs then
      (p.ExitCode, stdout.Result, stderr.Result)
    else
      (try p.Kill true with _ -> ())
      (-1, "", "deadline")
  finally
    try System.IO.Directory.Delete(dir, true) with _ -> ()

/// Apply ordered (from, to) text replacements to Dark source, outside string
/// literals. Interpolation holes in a `$"..."` string are code and are rewritten;
/// the text around them is not. A rule that touched a literal once turned
/// "expands to `Darklang.Stdlib.X`" in a docs string into a compiled-vs-
/// interpreted difference.
let private respellCode (rules : List<string * string>) (src : string) : string =
  let out = System.Text.StringBuilder()
  let code = System.Text.StringBuilder()
  // A rule matches at a name boundary only: `Result<` must not fire inside
  // `ParseResult<` or `Stdlib.Result.Result<`.
  let isNameChar (c : char) = System.Char.IsLetterOrDigit c || c = '_' || c = '.'
  let replaceAtBoundaries (text : string) (a : string) (b : string) : string =
    let sb = System.Text.StringBuilder()
    let mutable i = 0
    while i < text.Length do
      if System.String.CompareOrdinal(text, i, a, 0, a.Length) = 0
         && (i = 0 || not (isNameChar text[i - 1])) then
        sb.Append(b) |> ignore<System.Text.StringBuilder>
        i <- i + a.Length
      else
        sb.Append(text[i]) |> ignore<System.Text.StringBuilder>
        i <- i + 1
    sb.ToString()
  let flushCode () =
    let mutable text = code.ToString()
    for (a, b) in rules do
      text <- replaceAtBoundaries text a b
    out.Append(text) |> ignore<System.Text.StringBuilder>
    code.Clear() |> ignore<System.Text.StringBuilder>
  let n = src.Length
  let mutable i = 0
  // holes: depth of `{` inside an interpolated string, per nesting of strings
  let rec scanString (i : int) (interpolated : bool) : int =
    // src[i] is just past the opening quote; returns the index past the closing quote
    let mutable j = i
    let mutable fin = -1
    while fin < 0 && j < n do
      let c = src[j]
      if c = '\\' && j + 1 < n then
        out.Append(c).Append(src[j + 1]) |> ignore<System.Text.StringBuilder>
        j <- j + 2
      elif c = '"' then
        out.Append(c) |> ignore<System.Text.StringBuilder>
        fin <- j + 1
      elif interpolated && c = '{' && j + 1 < n && src[j + 1] = '{' then
        out.Append("{{") |> ignore<System.Text.StringBuilder>
        j <- j + 2
      elif interpolated && c = '{' then
        out.Append(c) |> ignore<System.Text.StringBuilder>
        j <- scanHole (j + 1)
      else
        out.Append(c) |> ignore<System.Text.StringBuilder>
        j <- j + 1
    if fin < 0 then n else fin
  and scanHole (i : int) : int =
    // code inside `{ }` of an interpolated string; returns the index past the `}`
    let mutable j = i
    let mutable depth = 0
    let mutable fin = -1
    while fin < 0 && j < n do
      let c = src[j]
      if c = '"' then
        flushCode ()
        let interp = j > 0 && src[j - 1] = '$'
        out.Append(c) |> ignore<System.Text.StringBuilder>
        j <- scanString (j + 1) interp
      elif c = '{' then
        code.Append(c) |> ignore<System.Text.StringBuilder>
        depth <- depth + 1
        j <- j + 1
      elif c = '}' && depth = 0 then
        flushCode ()
        out.Append(c) |> ignore<System.Text.StringBuilder>
        fin <- j + 1
      elif c = '}' then
        code.Append(c) |> ignore<System.Text.StringBuilder>
        depth <- depth - 1
        j <- j + 1
      else
        code.Append(c) |> ignore<System.Text.StringBuilder>
        j <- j + 1
    if fin < 0 then n else fin
  while i < n do
    let c = src[i]
    if c = '"' then
      flushCode ()
      let interp = i > 0 && src[i - 1] = '$'
      out.Append(c) |> ignore<System.Text.StringBuilder>
      i <- scanString (i + 1) interp
    else
      code.Append(c) |> ignore<System.Text.StringBuilder>
      i <- i + 1
  flushCode ()
  out.ToString()

let private outcome (status : string) (stdout : string) (detail : string) : Dval =
  DTuple(DString status, DString stdout, [ DString detail ])

let fns () : List<BuiltInFn> =
  [ { name = fn "compilerInfo" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Which native compiler is linked, and for which target."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let t =
            match target.Value with
            | Ok t -> string t
            | Error e -> $"no target: {e}"
          Ply(DString $"native compiler linked (pbiggar/darklang-compiler), target {t}")
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "compilerParse" 0
      typeParams = []
      parameters = [ Param.make "source" TString "one source unit" ]
      returnType = TString
      description =
        "Runs only the compiler's front end (layout passes, lexer, parser) over one unit. \"ok\" or the parse error. For telling a parse gap from a later one without a full compile."
      fn =
        (function
        | _, _, _, [| DString source |] ->
          match Parser.parseSourceString true source with
          | Ok _ -> Ply(DString "ok")
          | Error e -> Ply(DString e)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "compilerLayout" 0
      typeParams = []
      parameters = [ Param.make "source" TString "one source unit body" ]
      returnType = TString
      description = "The source after the compiler's layout passes, as its lexer sees it. Statement separators show as U+0002."
      fn =
        (function
        | _, _, _, [| DString source |] -> Ply(DString(Parser.normalizeLayout source))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "compilerRespell" 0
      typeParams = []
      parameters =
        [ Param.make "source" TString "Dark source"
          Param.make "rules" (TList(TTuple(TString, TString, []))) "(from, to), applied in order" ]
      returnType = TString
      description =
        "Text replacements over Dark source, at name boundaries only, that leave string literals alone (interpolation holes are code). For spelling names the way the compiler's front end wants them."
      fn =
        (function
        | _, _, _, [| DString src; DList(_, rules) |] ->
          let rs =
            rules
            |> List.choose (function
              | DTuple(DString a, DString b, []) -> Some(a, b)
              | _ -> None)
          Ply(DString(respellCode rs src))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "compilerCompile" 0
      typeParams = []
      parameters =
        [ Param.make "units" (TList(TTuple(TString, TString, [ TString ]))) "(name, purpose, source); purpose is executable | library | package"
          Param.make "mode" TString "eval (render the entry's value) or program (Int/Unit entry)" ]
      returnType = TTuple(TString, TString, [ TString ])
      description =
        "Compiles the source units with the native compiler. Returns (status, \"\", detail): status is ok with detail = the binary size in bytes, or compile-error with detail = the compiler's message. Does not run anything."
      fn =
        (function
        | _, _, _, [| DList(_, units); DString mode |] ->
          let us =
            units
            |> List.choose (function
              | DTuple(DString n, DString p, [ DString s ]) -> Some(n, p, s)
              | _ -> None)
          match compileUnits us mode with
          | Ok bin -> Ply(outcome "ok" "" (string bin.Length))
          | Error e -> Ply(outcome "compile-error" "" e)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "compilerCompileAndRun" 0
      typeParams = []
      parameters =
        [ Param.make "units" (TList(TTuple(TString, TString, [ TString ]))) "(name, purpose, source)"
          Param.make "mode" TString "eval or program"
          Param.make "timeoutMs" TInt64 "deadline for the compiled binary" ]
      returnType = TTuple(TString, TString, [ TString ])
      description =
        "Compiles the units and runs the binary. Returns (status, stdout, detail): status is ran (detail = stderr), compile-error (detail = message), crash (detail = exit code, stdout kept) or timeout."
      fn =
        (function
        | _, _, _, [| DList(_, units); DString mode; DInt64 timeoutMs |] ->
          let us =
            units
            |> List.choose (function
              | DTuple(DString n, DString p, [ DString s ]) -> Some(n, p, s)
              | _ -> None)
          match compileUnits us mode with
          | Error e -> Ply(outcome "compile-error" "" e)
          | Ok bin ->
            let (code, out, err) = runBinary bin (int timeoutMs)
            if code = 0 then Ply(outcome "ran" out err)
            elif code = -1 then Ply(outcome "timeout" out "")
            else Ply(outcome "crash" out (string code))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
