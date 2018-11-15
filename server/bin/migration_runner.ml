let () =
    print_endline "Running migrations";
    Libbackend.Migrations.init ();
    print_endline "Done running migrations"
