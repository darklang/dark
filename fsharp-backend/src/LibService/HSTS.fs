module LibService.HSTS

open System
open Microsoft.AspNetCore.HttpsPolicy

let setConfig (options: HstsOptions) =
  options.Preload <- true
  options.IncludeSubDomains <- true
  options.MaxAge <- TimeSpan.FromDays 365
