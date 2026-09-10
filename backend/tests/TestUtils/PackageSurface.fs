/// The `.dark` corpus, and how to ask whether a builtin is named in it.
///
/// One definition, because three readers were growing their own. `Builtin.Tests` had the corpus and
/// the reference regex; `LocalExec.PlatformReport` grew a second copy and got it subtly wrong (it
/// did not strip comments, so a builtin named in a doc comment next to its wrapper counted as a
/// second caller); `Platform.Tests` was about to make a third.
///
/// Lives in `TestUtils` because that is the only assembly both `Tests` and `LocalExec` reference.
/// Neither is shipped, so reading every file in the repo is fine here in a way it would not be in
/// `LibExecution`.
module TestUtils.PackageSurface

open System.IO
open System.Text.RegularExpressions

open Prelude


/// The repo root: the first directory at or above CWD holding `packages/darklang/`.
let findRepoRoot () : string =
  let rec walk (dir : string) : string option =
    if System.String.IsNullOrEmpty dir then
      None
    else
      let candidate = Path.Combine(dir, "packages", "darklang")
      if Directory.Exists candidate then
        Some dir
      else
        walk (Path.GetDirectoryName dir)
  match walk (Directory.GetCurrentDirectory()) with
  | Some root -> root
  | None ->
    Exception.raiseInternal
      "could not find the repo root (no packages/darklang above the working directory)"
      [ "cwd", Directory.GetCurrentDirectory() ]


/// Whole-line comments removed.
///
/// They go because every question asked of this corpus is textual, and naming a builtin in a doc
/// comment (a reasonable thing to do right next to the one fn that wraps it) otherwise reads as a
/// second caller. Only lines that are ENTIRELY a comment are dropped, so a `//` inside a string
/// literal cannot swallow real code after it on the same line.
let private stripWholeLineComments (text : string) : string =
  text
  |> String.splitOnNewline
  |> List.filter (fun line -> not ((line.TrimStart()).StartsWith "//"))
  |> String.concat "\n"


/// Build output holds copies of files we have already read.
let private isBuildOutput (path : string) : bool =
  let sep = Path.DirectorySeparatorChar
  path.Contains $"{sep}Build{sep}"


let private darkFilesUnder (root : string) : List<string * string> =
  if not (Directory.Exists root) then
    []
  else
    Directory.EnumerateFiles(root, "*.dark", SearchOption.AllDirectories)
    |> Seq.filter (isBuildOutput >> not)
    |> Seq.map (fun path ->
      let relative =
        path.Substring(root.Length).TrimStart([| '/'; '\\' |]).Replace("\\", "/")
      (relative, stripWholeLineComments (File.ReadAllText path)))
    |> List.ofSeq


/// Every `.dark` file under `packages/`, as (path relative to `packages/`, contents). Cached.
let packageFiles : Lazy<List<string * string>> =
  lazy (darkFilesUnder (Path.Combine(findRepoRoot (), "packages")))


/// The same corpus as one string.
let packagesText : Lazy<string> =
  lazy (packageFiles.Value |> List.map snd |> String.concat "\n")


/// Every `.dark` file in the repo, as one string. Wider than `packagesText`: it also covers test
/// files, perf workloads and sample scripts, which is the difference between "shipped once" and
/// "dead".
let repoDarkText : Lazy<string> =
  lazy (darkFilesUnder (findRepoRoot ()) |> List.map snd |> String.concat "\n")


/// The module a file belongs to: `darklang/stdlib/list.dark` -> `darklang/stdlib`.
///
/// Owner AND module, not one or the other. One level gives the owner, which is `darklang` for
/// nearly everything and says nothing; three gives a file name, which is noise. The owner still
/// matters at this grain, because a builtin wrapped under a different OWNER is a different kind of
/// finding from one wrapped in the wrong module.
let area (relativePath : string) : string =
  match relativePath.Split('/') |> Array.toList with
  | owner :: modul :: _ :: _ -> $"{owner}/{modul}"
  | [ owner; file ] -> $"{owner}/{Path.GetFileNameWithoutExtension file}"
  | [ single ] -> Path.GetFileNameWithoutExtension single
  | [] -> "?"


/// `Builtin.<name>` or `Builtin.<name>_v<n>`. The `(?![a-zA-Z0-9_])` lookahead stops
/// `Builtin.dictGet` matching the prefix of `Builtin.dictGetItem`.
let private referencePattern (builtinName : string) : string =
  $@"Builtin\.{Regex.Escape builtinName}(?:_v[0-9]+)?(?![a-zA-Z0-9_])"

let countReferencesIn (corpus : string) (builtinName : string) : int =
  Regex.Matches(corpus, referencePattern builtinName).Count

let countReferences (builtinName : string) : int =
  countReferencesIn packagesText.Value builtinName

let referencesBuiltin (contents : string) (builtinName : string) : bool =
  Regex.IsMatch(contents, referencePattern builtinName)


/// Builtins that are language IDIOM rather than library calls, so "one wrapper, everyone through
/// it" does not apply to them. Distinct from any deliberate allowlist, which is for builtins that
/// COULD be wrapped and are not.
let languageIdioms : Set<string> =
  Set.ofList
    [ // `unwrap` reads as syntax and appears in 60-odd places. A generic Dark wrapper typechecks
      // (`let unwrap (value: 'optOrRes) : 'a` works for both Option and Result) and buys nothing:
      // it has no shape to type and nothing to document that the name does not say, and it puts
      // itself at the bottom of every unwrap failure's call stack, one frame below the code that
      // had the None. That frame is the reason, and it is a reason about error messages, not about
      // layering.
      "unwrap" ]
