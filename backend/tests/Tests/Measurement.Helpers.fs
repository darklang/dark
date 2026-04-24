/// Helpers for the Phase 0 measurement harness.
///
/// Walks a Dval tree and counts allocated nodes. Used to expose the
/// boxed-list overhead of today's `List<UInt8>` representation — every
/// byte is one `DUInt8` cell plus one list-node cell, so a 1KB file
/// shows up as ~2000 nodes, not a single leaf.
module Tests.MeasurementHelpers

open Prelude

module RT = LibExecution.RuntimeTypes


/// Counts Dval nodes reachable from a root. A scalar counts as 1.
/// Lists, dicts, records, enums, tuples count themselves + the sum of
/// their children.
let rec countDvalNodes (dv : RT.Dval) : int =
  match dv with
  | RT.DUnit
  | RT.DBool _
  | RT.DInt8 _
  | RT.DUInt8 _
  | RT.DInt16 _
  | RT.DUInt16 _
  | RT.DInt32 _
  | RT.DUInt32 _
  | RT.DInt64 _
  | RT.DUInt64 _
  | RT.DInt128 _
  | RT.DUInt128 _
  | RT.DFloat _
  | RT.DChar _
  | RT.DString _
  | RT.DDateTime _
  | RT.DUuid _
  | RT.DApplicable _
  | RT.DDB _
  | RT.DBlob _
  | RT.DStream _ -> 1
  | RT.DList(_, items) -> 1 + List.sumBy countDvalNodes items
  | RT.DTuple(a, b, rest) ->
    1 + countDvalNodes a + countDvalNodes b + List.sumBy countDvalNodes rest
  | RT.DDict(_, entries) -> 1 + (entries |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DRecord(_, _, _, fields) ->
    1 + (fields |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DEnum(_, _, _, _, fields) -> 1 + List.sumBy countDvalNodes fields


/// Directory measurements write to. Callers: ensure it exists via
/// [ensureOutputDir]. Legacy alias — phase-0 measurements still write
/// here; phase-1 uses [phaseDir "phase-1"].
let outputDir : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "measurements", "phase-0")


/// Per-phase output directory under `rundir/measurements/<phase>/`.
let phaseDir (phase : string) : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "measurements", phase)


let ensureOutputDir () : unit =
  System.IO.Directory.CreateDirectory(outputDir)
  |> ignore<System.IO.DirectoryInfo>


let ensurePhaseDir (phase : string) : unit =
  System.IO.Directory.CreateDirectory(phaseDir phase)
  |> ignore<System.IO.DirectoryInfo>


/// Snapshot of resource use around a measured block.
type Sample =
  { allocatedBytesDelta : int64; peakWorkingSet : int64; elapsedMs : int64 }


/// Runs [f], returning (result, sample). Forces GC before the before-sample
/// so the delta reflects what [f] actually allocated, not prior garbage.
let measure<'a> (f : unit -> 'a) : 'a * Sample =
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()
  System.GC.Collect()

  let proc = System.Diagnostics.Process.GetCurrentProcess()
  let beforeAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  let beforeWs = proc.WorkingSet64
  let sw = System.Diagnostics.Stopwatch.StartNew()

  let result = f ()

  sw.Stop()
  proc.Refresh()
  let afterAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  let afterWs = proc.WorkingSet64

  let sample =
    { allocatedBytesDelta = afterAlloc - beforeAlloc
      peakWorkingSet = max beforeWs afterWs
      elapsedMs = sw.ElapsedMilliseconds }

  result, sample


/// Append a one-line record to the named output file. One file per
/// scenario; scenarios append rows over time. Human-readable format
/// so row-by-row diffs stay legible.
let appendRow (filename : string) (row : string) : unit =
  ensureOutputDir ()
  let path = System.IO.Path.Combine(outputDir, filename)
  System.IO.File.AppendAllText(path, row + "\n")


/// Truncate an output file (if any) so a fresh scenario run starts
/// clean. Call once at the top of each scenario before the first
/// [appendRow].
let resetOutput (filename : string) : unit =
  ensureOutputDir ()
  let path = System.IO.Path.Combine(outputDir, filename)
  if System.IO.File.Exists(path) then System.IO.File.Delete(path)


/// Phase-scoped version of [appendRow] — writes to
/// `rundir/measurements/<phase>/<filename>`.
let appendPhaseRow (phase : string) (filename : string) (row : string) : unit =
  ensurePhaseDir phase
  let path = System.IO.Path.Combine(phaseDir phase, filename)
  System.IO.File.AppendAllText(path, row + "\n")


/// Phase-scoped version of [resetOutput].
let resetPhaseOutput (phase : string) (filename : string) : unit =
  ensurePhaseDir phase
  let path = System.IO.Path.Combine(phaseDir phase, filename)
  if System.IO.File.Exists(path) then System.IO.File.Delete(path)
