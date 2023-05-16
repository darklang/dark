using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using SpawnDev.BlazorJS;
using SpawnDev.BlazorJS.JSObjects;
using SpawnDev.BlazorJS.JsonConverters;
using SpawnDev.BlazorJS.Test;
using SpawnDev.BlazorJS.WebWorkers;

#if DEBUG
JSObject.UndisposedHandleVerboseMode = true;
#endif


var builder = WebAssemblyHostBuilder.CreateDefault(args);

builder.RootComponents.Add<App>("#app");

// Modify JSRuntime.JsonSerializerOptions
// (WARNING: Modifying this can cause unexpected results. Test thoroughly.)
builder.Services.AddJSRuntimeJsonOptions(jsRuntimeJsonOptions =>
{
#if DEBUG
  Console.WriteLine($"JSRuntime JsonConverters count: {jsRuntimeJsonOptions.Converters.Count}");
#endif
});

// Add services
builder.Services.AddScoped((sp) =>
  new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) }
);

builder.Services.AddBlazorJSRuntime();

builder.Services.AddWebWorkerService();

builder.Services.AddSingleton<IMathService, MathService>();

// build and Init using BlazorJSRunAsync (instead of RunAsync)
await builder.Build().BlazorJSRunAsync();
