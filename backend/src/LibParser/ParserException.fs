module LibParser.ParserException

open Prelude

type Pos = { line : int; column : int }
type Range =
  { start : Pos
    end_ : Pos }

  static member FromFSharpRange(range : FSharp.Compiler.Text.range) =
    { start = { line = range.StartLine - 1; column = range.StartColumn + 1 }
      end_ = { line = range.EndLine - 1; column = range.EndColumn + 1 } }

/// An error raised by the parser
type ParserException(message : string, metadata : Metadata, range : Option<Range>) =
  // to make sure this still gets captured by anything that's capturing InternalExceptions
  inherit Exception.InternalException(message)

  member _.metadata = metadata
  member _.range = range

  new(message : string,
      metadata : Metadata,
      range : Option<FSharp.Compiler.Text.range>) =
    ParserException(message, metadata, Option.map Range.FromFSharpRange range)

let raiseParserError
  (message : string)
  (metadata : Metadata)
  (range : Option<FSharp.Compiler.Text.range>)
  =
  raise (ParserException(message, metadata, range))


let unwrapOptionInternalOrRaiseParserError
  (message : string)
  (metadata : Metadata)
  (range : Option<FSharp.Compiler.Text.range>)
  (option : Option<'a>)
  : 'a =
  match option with
  | Some x -> x
  | None -> raiseParserError message metadata range

let wrapParserCapturingErrors (f : unit -> 'a) : Result<'a, ParserException> =
  try
    f () |> Ok
  with :? ParserException as e ->
    Error e
