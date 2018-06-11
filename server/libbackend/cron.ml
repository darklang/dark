open Core

module Dbp = Dbprim

let last_ran_at (canvas_id: Uuid.t) (h: Handler.handler) : Time.t option =
  let open Option in
  let fetched =
    Printf.sprintf
      "SELECT ran_at
       FROM \"cron_records\"
       WHERE tlid = %s
       AND canvas_id = %s
       ORDER BY id DESC
       LIMIT 1"
       (Dbp.tlid h.tlid)
       (Dbp.uuid canvas_id)
    |> Db.fetch_via_sql
    |> List.hd
  in
  fetched >>= fun l ->
  match l with
  | [lrt] ->
    let time =
      Dval.date_of_sqlstring lrt
    in
    Some time
  | _ -> None

let parse_interval (h: Handler.handler) : Time.Span.t option =
  let open Option in
  Handler.modifier_for h >>= fun modif ->
  match String.lowercase modif with
  | "daily" ->
    Time.Span.create ~sign:Sign.Pos ~day:1 ()
    |> Some
  | "weekly" ->
    Time.Span.create ~sign:Sign.Pos ~day:7 ()
    |> Some
  | "fortnightly" ->
    Time.Span.create ~sign:Sign.Pos ~day:14 ()
    |> Some
  | "every 1hr" ->
    Time.Span.create ~sign:Sign.Pos ~hr:1 ()
    |> Some
  | "every 12hrs" ->
    Time.Span.create ~sign:Sign.Pos ~hr:12 ()
    |> Some
  | _ -> None

let should_execute (canvas_id: Uuid.t) (h: Handler.handler) : bool =
  let open Option in
  match last_ran_at canvas_id h with
  | None -> true (* we should always run if we've never run before *)
  | Some lrt ->
    let now = Time.now () in
    (match parse_interval h with
    | None ->
      Exception.internal
        ("Can't parse interval: "
         ^ (Handler.modifier_for h
            |> Option.value ~default:"<empty modifier>")
        )
    | Some interval ->
      (* Example:
       * last_ran_at = 16:00
       * interval: 1 hour
       * now: 16:30
       *
       * therefore:
       *   should_run_after is 17:01
       *
       *   and we should run once now >= should_run_after
       *
       *)
      let should_run_after = Time.add lrt interval in
      now >= should_run_after)

let record_execution (canvas_id: Uuid.t) (h: Handler.handler) : unit =
  Printf.sprintf
    "INSERT INTO \"cron_records\"
    (tlid, canvas_id)
    VALUES (%s, %s)"
    (Dbp.tlid h.tlid)
    (Dbp.uuid canvas_id)
  |> Db.run_sql ~quiet:true

