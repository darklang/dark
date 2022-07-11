module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql
open Npgsql.FSharp
open Npgsql.NodaTime

open Prelude

module RT = LibExecution.RuntimeTypes

module Sql =

  /// Open a database [transaction] and run given fn against it
  let withTransaction (f : unit -> Task<'a>) : Task<'a> =
    task {
      let connection = LibService.DBConnection.connect () |> Sql.createConnection
      connection.Open()

      let! transaction = connection.BeginTransactionAsync()
      let! result = f ()
      do! transaction.CommitAsync()
      return result
    }

  let query (sql : string) : Sql.SqlProps =
    LibService.DBConnection.connect () |> Sql.query sql

  let executeRowOptionAsync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Task<Option<'t>> =
    task {
      match! Sql.executeAsync reader props with
      | [ a ] -> return Some a
      | [] -> return None
      | list ->
        return
          Exception.raiseInternal
            $"Too many results, expected 0 or 1"
            [ "actual", list ]
    }

  let executeRowOptionSync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Option<'t> =
    match Sql.execute reader props with
    | [ a ] -> Some a
    | [] -> None
    | list ->
      Exception.raiseInternal $"Too many results, expected 0 or 1" [ "actual", list ]

  // TODO do a better job of naming these
  let executeExistsAsync (props : Sql.SqlProps) : Task<bool> =
    task {
      match! Sql.executeAsync (fun read -> read.NpgsqlReader.GetBoolean 0) props with
      | [ true ] -> return true
      | [] -> return false
      | result ->
        return
          Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]
    }

  let executeExistsSync (props : Sql.SqlProps) : bool =
    match Sql.execute (fun read -> read.NpgsqlReader.GetBoolean 0) props with
    | [ true ] -> true
    | [] -> false
    | result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]


  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      let! (_count : int) = Sql.executeNonQueryAsync props
      return ()
    }

  let executeStatementSync (props : Sql.SqlProps) : unit =
    let _count = Sql.executeNonQuery props
    ()


  let id (id : uint64) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Bigint
    let idParam = NpgsqlParameter("id", typ)
    idParam.Value <- int64 id
    Sql.parameter idParam

  let tlid (tlid : uint64) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Bigint
    let idParam = NpgsqlParameter("tlid", typ)
    idParam.Value <- int64 tlid
    Sql.parameter idParam

  let idArray (ids : List<uint64>) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let idsParam = NpgsqlParameter("ids", typ)
    idsParam.Value <- ids |> List.map int64 |> List.toArray
    Sql.parameter idsParam

  let queryableDvalMap (dvalmap : RT.DvalMap) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dvalmap", typ)
    param.Value <-
      LibExecution.DvalReprInternalDeprecated.toInternalQueryableV1 dvalmap
    Sql.parameter param

  let roundtrippableDval (dval : RT.Dval) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dval", typ)
    param.Value <-
      LibExecution.DvalReprInternalDeprecated.toInternalRoundtrippableV0 dval
    Sql.parameter param

  let roundtrippableDvalMap (dvalmap : RT.DvalMap) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dvalmap", typ)
    param.Value <-
      LibExecution.DvalReprInternalDeprecated.toInternalRoundtrippableV0 (
        RT.DObj dvalmap
      )
    Sql.parameter param

  // We sometimes erroneously store these as timestamps that are not Utc. But we mean them to be Utc.
  let instantWithoutTimeZone (i : NodaTime.Instant) : SqlValue =
    Sql.timestamp (i.toUtcLocalTimeZone().ToDateTimeUnspecified())

  let instantWithTimeZone (i : NodaTime.Instant) : SqlValue =
    Sql.timestamptz (i.ToDateTimeUtc())

  let instantWithTimeZoneOrNone (i : Option<NodaTime.Instant>) : SqlValue =
    i |> Option.map (fun i -> i.ToDateTimeUtc()) |> Sql.timestamptzOrNone



// Extension methods
type RowReader with

  member this.tlid(name : string) : tlid = this.int64 name |> uint64
  member this.id(name : string) : id = this.int64 name |> uint64

  // CLEANUP migrate these
  // When creating our DB schema, we often incorrectly chose `timestamp` (aka
  // `timestamp without a time zone`) as the type, when we should have chosen
  // `timestampz` (aka `timestamp with a timezone`, though actually just a UTC
  // timestamp). However, internally they're all timestamp in Utc. So this just gets
  // a value from a `timestamp` field and converts it to an instant (adding in the
  // implied UTC timezone)
  member this.instantWithoutTimeZone(name : string) : NodaTime.Instant =
    // fetch as LocalDateTime
    let dateTime : System.DateTime = this.dateTime (name) // with Kind = Unspecific
    // add timezone
    let utcDateTime = System.DateTime(dateTime.Ticks, System.DateTimeKind.Utc)

    NodaTime.Instant.FromDateTimeUtc utcDateTime

  member this.instant(name : string) : NodaTime.Instant =
    let dateTime : System.DateTime = this.dateTime (name)
    NodaTime.Instant.FromDateTimeUtc dateTime

  member this.instantOrNone(name : string) : Option<NodaTime.Instant> =
    this.dateTimeOrNone (name) |> Option.map NodaTime.Instant.FromDateTimeUtc



// member this.queryableDval(name : string) =
//   this.string name |> LibExecution.DvalReprLegacyExternal.ofInternalQueryableV1
// member this.roundtrippableDval(name : string) =
//   this.string name |> LibExecution.DvalReprLegacyExternal.ofInternalRoundtrippableV0


type TableStatsRow =
  { relation : string
    diskBytes : int64
    rows : int64
    diskHuman : string
    rowsHuman : string }

let tableStats () : Task<List<TableStatsRow>> =
  // Sizes from the pg_class table are fast (vs, say, running `SELECT count` on a
  // large table) but also are approximate, not precise. That's fine for purposes
  // of "how big are my tables growing to be?"
  //
  // Three steps in the query in table_stats:
  // 1) subquery "sizes" gets the data we want (size in bytes, number of rows)
  // 2) subquery "with_total_row" appends a row to the resultset that SUM()s the contents of each
  //    field
  // 3) the final query provides both raw- and humanized- formatted columns
  Sql.query
    "WITH sizes AS (
         SELECT
            relname as \"relation\",
            pg_total_relation_size (C .oid) as disk,
            reltuples::bigint AS \"rows\"
         FROM pg_class C
         LEFT JOIN pg_namespace N ON (N.oid = C .relnamespace)
         WHERE nspname NOT IN ('pg_catalog', 'information_schema')
         AND C .relkind <> 'i'
         AND nspname !~ '^pg_toast'
         ORDER BY pg_total_relation_size (C .oid) DESC
     ),

     -- with_total_row is a subquery that appends a SUM() row to the bottom of our result set
     with_total_row AS (
         SELECT relation, disk, \"rows\" FROM sizes
         UNION ALL
         SELECT
            'Total',
            SUM(disk),
            SUM(\"rows\")
         FROM sizes
     )

     -- now we actually do our output, including both raw and humanized-number
     -- columns for \"disk\" and \"rows\"
     SELECT relation,
         disk,
         \"rows\",
         pg_size_pretty(disk) as disk_human,
         -- NOTE: below uses pg_size_pretty to get us something human readable
         -- ('100M' is easier than '100000000', but it's _row count_, not bytes,
         -- hence trimming those parts off.)
         --
         -- Examples for trim(from substring(...)):
         -- 100 MB -> 100M
         -- 100 kb -> 100k
         -- 1 bytes -> 1
         trim(from
             substring(
                 pg_size_pretty ( \"rows\"::bigint)
                 from '[0-9]* [^b]?')
         ) as rows_human
     FROM with_total_row"
  |> Sql.executeAsync (fun read ->
    { relation = read.string "relation"
      diskBytes = read.int64OrNone "disk" |> Tablecloth.Option.unwrap 0
      rows = read.int64OrNone "rows" |> Tablecloth.Option.unwrap 0
      diskHuman = read.string "disk_human"
      rowsHuman = read.string "rows_human" })

let init () : unit =
  NpgsqlConnection.GlobalTypeMapper.UseNodaTime()
  |> ignore<TypeMapping.INpgsqlTypeMapper>
