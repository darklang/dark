module LibService.HSTS

open Microsoft.AspNetCore.HttpsPolicy

// The traditional methods of using `UseHsts` and `AddHsts` within BwdServer
// were ineffective. Somehow, the Strict-Transport-Security header was not
// included in HTTP Reponses as a result of these efforts. Here, we manually
// work around this by setting it manually.
// CLEANUP: replace this with the more traditional approach, somehow

// If you update these, please ensure the below match each other

let setConfig (options : HstsOptions) =
  options.Preload <- true
  options.IncludeSubDomains <- true
  options.MaxAge <- System.TimeSpan.FromDays 365

let stringConfig = "max-age=31536000; includeSubDomains; preload"
