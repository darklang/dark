let t_route_host () =
  let custom_domain = "route_host.customdomain.com" in
  let canvas = "test-route_host" in
  let open Libbackend.Webserver in
  Db.run
    ~name:"insert test custom_domain"
    "INSERT INTO custom_domains(host,canvas) VALUES ($1, $2)"
    ~params:[Db.String custom_domain; Db.String canvas] ;
  AT.check
    (AT.list AT.string)
    "route_host works"
    (* foo checks
     * *.builtwithdark.com;  custom_domain checks routing via the db's
     * custom_domains table *)
    ["foo"; "test-route_host"]
    ( ["http://foo.builtwithdark.com"; "http://" ^ custom_domain]
    |> List.map ~f:Uri.of_string
    |> List.map ~f:CRequest.make
    |> List.map ~f:route_host
    |> List.map ~f:(function
           | None | Some Static | Some Admin ->
               "failure"
           | Some (Canvas canvas) ->
               canvas) )
