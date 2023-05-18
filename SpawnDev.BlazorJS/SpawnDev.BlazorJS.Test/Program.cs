using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using SpawnDev.BlazorJS;
using SpawnDev.BlazorJS.JSObjects;
using SpawnDev.BlazorJS.JsonConverters;
using SpawnDev.BlazorJS.Test;
using SpawnDev.BlazorJS.WebWorkers;
using Microsoft.JSInterop;




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


var webWorkerService = builder.Services.GetRequiredService<IWebWorkerService>();

public static class ExposedFunctions
{

  static string? canDisposeWorker => webWorker != null ? null : "disabled";
  static string? RunDisabled => Running ? "disabled" : null;
  static string? WorkerDisabled => Running || !supported ? "disabled" : null;
  static bool supported => webWorkerService != null && webWorkerService.SharedWebWorkerSupported;


  static int piDecimalPlaces = 1500;
  static int piProgress = 0;
  static string output = "";

  static WebWorker webWorker;
  static bool Running = false;

  static string rn = Environment.NewLine;


  [JSInvokable]
  static async Task OnWithoutThreadClicked()
  {
    Running = true;
    piProgress = 0;
    var result = await mathService.CalculatePi(piDecimalPlaces);
    piProgress = piDecimalPlaces;
    output += $"{rn}{LogDate()} EstimatePI({piDecimalPlaces}) = {result}";
    Running = false;
  }


  [JSInvokable]
  static async Task OnWithThreadClicked()
  {
    Running = true;
    piProgress = 0;
    StateHasChanged();
    if (webWorker == null) webWorker = await webWorkerService.GetWebWorker();
    var mathServiceWorker = webWorker.GetService<IMathService>();
    var result = await mathServiceWorker.CalculatePiWithActionProgress(piDecimalPlaces, new Action<int>((i) =>
    {
      piProgress = i;
    }));
    piProgress = piDecimalPlaces;
    output += $"{rn}{LogDate()} EstimatePI({piDecimalPlaces}) = {result}";
    Running = false;
  }

  [JSInvokable]
  public static void OnDisposeWorker()
  {
    webWorker?.Dispose();
    webWorker = null;
    Running = false;
  }

  private static string LogDate()
  {
    return DateTime.Now.ToString("HH:mm:ss:fff");
  }
}


