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

  member this.SymbolCount : int = int (TS.Language.ts_language_symbol_count (Handle))

  member this.SymbolName(symbol : uint16) : string =
    Marshal.PtrToStringAnsi(TS.Language.ts_language_symbol_name (Handle, symbol))

  member this.SymbolForName(name : string, isNamed : bool) : uint16 option =
    let ptr = Marshal.StringToHGlobalAnsi(name)
    let id =
      TS.Language.ts_language_symbol_for_name (
        Handle,
        ptr,
        uint32 name.Length,
        isNamed
      )
    Marshal.FreeHGlobal(ptr)
    if id = 0us then None else Some id

  member this.FieldCount : int = int (TS.Language.ts_language_field_count (Handle))

  member this.FieldNameForId(fieldId : uint16) : string =
    Marshal.PtrToStringAnsi(
      TS.Language.ts_language_field_name_for_id (Handle, fieldId)
    )

  member this.FieldIdForName(fieldName : string) : uint16 option =
    let ptr = Marshal.StringToHGlobalAnsi(fieldName)
    let id =
      TS.Language.ts_language_field_id_for_name (
        Handle,
        ptr,
        uint32 fieldName.Length
      )
    Marshal.FreeHGlobal(ptr)
    if id = 0us then None else Some id

  member this.GetSymbolType(symbol : uint16) : SymbolType =
    // note: originally this had a whole enum<> casting thing - maybe the match here is bad for some reason
    match TS.Language.ts_language_symbol_type (Handle, symbol) with
    | TS.Types.TsSymbolType.Regular -> SymbolType.Regular
    | TS.Types.TsSymbolType.Anonymous -> SymbolType.Anonymous
    | TS.Types.TsSymbolType.Auxiliary -> SymbolType.Auxiliary
    | _ -> failwith "nahjjasdfj"
