module LibService.HSTS

open System
open Microsoft.AspNetCore.HttpsPolicy

// If you update these, please ensure the below match each other

let setConfig (options : HstsOptions) =
  options.Preload <- true
  options.IncludeSubDomains <- true
  options.MaxAge <- TimeSpan.FromDays 365

let stringConfig = "max-age=31536000; includeSubDomains; preload"
