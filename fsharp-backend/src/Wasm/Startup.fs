namespace MyApp.Client

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Bolero.Remoting.Client

module Program =

  [<EntryPoint>]
  let Main args =
      printf("tset")
      let builder = WebAssemblyHostBuilder.CreateDefault(args)
      builder.Build().RunAsync() |> ignore
      0
