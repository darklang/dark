module rec LibTSParser_FS.LibTreeSitter

open System
open System.Runtime.InteropServices
open System.Runtime.Serialization
open System.Diagnostics

module TS = LibTreeSitter_FS.TreeSitter

[<Serializable>]
type TreeSitterException =
  inherit Exception
  new(message : string) = { inherit Exception(message) }

type Point =
  { Row : int
    Column : int }

  // // Override default equality and comparison
  // override this.Equals(otherObj) =
  //   match otherObj with
  //   | :? Point as other -> this.Row = other.Row && this.Column = other.Column
  //   | _ -> false

  // override this.GetHashCode() =
  //   let prime = 397
  //   this.Row * prime + this.Column

  // interface System.IComparable<Point> with
  //   member this.CompareTo(other) =
  //     if Object.ReferenceEquals(this, other) then
  //       0
  //     else
  //       match this.Row.CompareTo(other.Row) with
  //       | 0 -> this.Column.CompareTo(other.Column)
  //       | x -> x

  // For a prettier printout
  override this.ToString() = sprintf "(%d, %d)" this.Row this.Column


type Range = { StartByte : int; EndByte : int; StartPoint : Point; EndPoint : Point }


type SymbolType =
  | Regular = 0
  | Anonymous = 1
  | Auxiliary = 2

type Language(handle : IntPtr) =
  let Handle = handle

  // Constructor logic
  do if handle = IntPtr.Zero then raise (new ArgumentNullException("handle"))
