/// Standard libraries for SQLite
module BuiltinCli.Libs.Sqlite

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude
open LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module TypeChecker = LibExecution.TypeChecker
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


// Helper function to convert SQLite values to Dvals
let sqliteValueToDval (threadID : ThreadID) (value : obj) : Dval =
  match value with
  | null -> TypeChecker.DvalCreator.option threadID VT.unknownTODO None
  | :? int64 as i -> DInt64 i
  | :? double as d -> DFloat d
  | :? string as s -> DString s
  | :? System.Boolean as b -> DBool b
  | :? (byte array) as bytes ->
    DList(
      ValueType.Known KTUInt8,
      bytes |> Array.map (fun b -> DUInt8 b) |> Array.toList
    )
  | _ -> DString(value.ToString())


// Helper function to convert a row to a Dict
let rowToDict (threadID : ThreadID) (reader : SqliteDataReader) : Dval =
  let mutable fields = Map.empty

  for i in 0 .. reader.FieldCount - 1 do
    let columnName = reader.GetName(i)
    let value = reader.GetValue(i)
    let dval = sqliteValueToDval threadID value
    fields <- Map.add columnName dval fields

  DDict(ValueType.Unknown, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "sqliteQuery" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString ""; Param.make "sql" TString "" ]
      returnType = TypeReference.result (TList(TDict TString)) TString
      description =
        "Executes a SELECT query against the SQLite database at <param path> and returns the results as a list of dictionaries. Each dictionary represents a row with column names as keys."
      fn =
        (function
        | _, vmState, _, [ DString path; DString sql ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let connString = $"Data Source={path};Mode=ReadWrite"
              use connection = new SqliteConnection(connString)
              do! connection.OpenAsync()

              use command = connection.CreateCommand()
              command.CommandText <- sql

              use! reader = command.ExecuteReaderAsync()
              let mutable results = []

              let rec readRows () =
                uply {
                  let! hasRow = reader.ReadAsync()

                  if hasRow then
                    let row = rowToDict vmState.threadID reader
                    results <- row :: results
                    return! readRows ()
                  else
                    return ()
                }

              do! readRows ()

              let listResult =
                DList(ValueType.Known(KTDict(ValueType.Unknown)), List.rev results)

              return
                Dval.resultOk
                  (KTList(ValueType.Known(KTDict(ValueType.Unknown))))
                  KTString
                  listResult
            with e ->
              return
                Dval.resultError
                  (KTList(ValueType.Known(KTDict(ValueType.Unknown))))
                  KTString
                  (DString($"SQLite query error: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqliteQueryOne" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString ""; Param.make "sql" TString "" ]
      returnType = TypeReference.result (TypeReference.option (TDict TString)) TString
      description =
        "Executes a SELECT query against the SQLite database at <param path> and returns the first row as an Option of dictionary. Returns None if no rows match."
      fn =
        (function
        | _, vmState, _, [ DString path; DString sql ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let connString = $"Data Source={path};Mode=ReadWrite"
              use connection = new SqliteConnection(connString)
              do! connection.OpenAsync()

              use command = connection.CreateCommand()
              command.CommandText <- sql

              use! reader = command.ExecuteReaderAsync()

              let! hasRow = reader.ReadAsync()

              let dictType = VT.dict VT.unknownTODO

              let optResult =
                if hasRow then
                  let row = rowToDict vmState.threadID reader
                  TypeChecker.DvalCreator.optionSome vmState.threadID dictType row
                else
                  TypeChecker.DvalCreator.optionNone dictType

              // Manually construct Result.Ok enum containing the option
              return
                DEnum(
                  Dval.resultType,
                  Dval.resultType,
                  [ VT.unknownTODO; VT.string ],
                  "Ok",
                  [ optResult ]
                )
            with e ->
              // Manually construct Result.Error enum
              return
                DEnum(
                  Dval.resultType,
                  Dval.resultType,
                  [ VT.unknownTODO; VT.string ],
                  "Error",
                  [ DString($"SQLite query error: {e.Message}") ]
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqliteExecute" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString ""; Param.make "sql" TString "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Executes a SQL statement (INSERT, UPDATE, DELETE, CREATE, etc.) against the SQLite database at <param path>. Returns the number of rows affected."
      fn =
        (function
        | _, _, _, [ DString path; DString sql ] ->
          uply {
            try
              let path =
                path.Replace(
                  "$HOME",
                  System.Environment.GetEnvironmentVariable "HOME"
                )

              let connString = $"Data Source={path};Mode=ReadWriteCreate"
              use connection = new SqliteConnection(connString)
              do! connection.OpenAsync()

              use command = connection.CreateCommand()
              command.CommandText <- sql

              let! affectedRows = command.ExecuteNonQueryAsync()
              let result = DInt64(int64 affectedRows)
              return Dval.resultOk KTInt64 KTString result
            with e ->
              return
                Dval.resultError
                  KTInt64
                  KTString
                  (DString($"SQLite execute error: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = Builtin.make [] fns
