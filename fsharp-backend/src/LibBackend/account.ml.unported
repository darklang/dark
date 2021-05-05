let insert_account
    ?(validate : bool = true)
    ~(analytics_metadata : Types.RuntimeT.dval_map option)
    (account : account) : (unit, string) Result.t =
  let result = if validate then validate_account account else Ok () in
  let analytics_metadata =
    analytics_metadata
    |> Option.value ~default:([] |> Types.RuntimeT.DvalMap.from_list)
  in
  Result.map result ~f:(fun () ->
      Db.run
        ~name:"insert_account"
        ~subject:account.username
        "INSERT INTO accounts
    (id, username, name, email, admin, password, segment_metadata)
    VALUES
    ($1, $2, $3, $4, false, $5, $6::jsonb)
    ON CONFLICT DO NOTHING"
        ~params:
          [ Uuid (Util.create_uuid ())
          ; String account.username
          ; String account.name
          ; String account.email
          ; String (Password.to_bytes account.password)
          ; QueryableDvalmap analytics_metadata ])
  |> Result.bind ~f:(fun () ->
         if Db.exists
              ~name:"check_inserted_account"
              ~subject:account.username
              "SELECT 1 from ACCOUNTS where
               username = $1 AND name = $2 AND email = $3 AND password = $4"
              ~params:
                [ String account.username
                ; String account.name
                ; String account.email
                ; String (Password.to_bytes account.password) ]
         then Ok ()
         else
           Error
             "Insert failed, probably because the username is already taken.")


(* Passwords set here are only valid locally, production uses auth0 to check
 * access *)
let upsert_account ?(validate : bool = true) (account : account) :
    (unit, string) Result.t =
  let result = if validate then validate_account account else Ok () in
  Result.map result ~f:(fun () ->
      Db.run
        ~name:"upsert_account"
        ~subject:account.username
        "INSERT INTO accounts
    (id, username, name, email, admin, password)
    VALUES
    ($1, $2, $3, $4, false, $5)
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  password = EXCLUDED.password"
        ~params:
          [ Uuid (Util.create_uuid ())
          ; String account.username
          ; String account.name
          ; String account.email
          ; String (Password.to_bytes account.password) ])


let can_access_operations ~(username : username) : bool = is_admin ~username

let auth_domain_for host : string =
  match String.split host '-' with d :: _ -> d | _ -> host


let for_host (host : string) : Uuidm.t option = host |> auth_domain_for |> owner

let for_host_exn (host : string) : Uuidm.t =
  host
  |> for_host
  |> fun o -> Option.value_exn ~message:("No owner found for host " ^ host) o


(************************)
(* Darkinternal functions *)
(************************)

(* Any external calls to this should also call Stroller.heapio_identify_user;
 * we can't do it here because that sets up a module dependency cycle *)
let insert_user
    ~(username : string)
    ~(email : string)
    ~(name : string)
    ?(analytics_metadata : Types.RuntimeT.dval_map option)
    () : (unit, string) Result.t =
  (* As of the move to auth0, we  no longer store passwords in postgres. We do
   * still use postgres locally, which is why we're not removing the field
   * entirely. Local account creation is done in
   * upsert_account_exn/upsert_admin_exn, so using Password.invalid here does
   * not affect that *)
  let password = Password.invalid in
  insert_account {username; email; name; password} ~analytics_metadata
