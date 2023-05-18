using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;
using System.Text.Json;

namespace SpawnDev.BlazorJS
{
    public interface IBackgroundService
    {
        Task InitAsync();
    }
    public static class IServiceCollectionExtensions
    {
        /// <summary>
        /// WARNING: Modifying the JSRuntime.JsonSerializerOptions can have unexpected results.
        /// </summary>
        public static IServiceCollection AddJSRuntimeJsonOptions(this IServiceCollection _this, Action<JsonSerializerOptions> configure)
        {
            if (BlazorJSRuntime.RuntimeJsonSerializerOptions != null)
              configure(BlazorJSRuntime.RuntimeJsonSerializerOptions);

            return _this;
        }
        static IServiceCollection? serviceCollection = null;
        /// <summary>
        /// Adds the BlazorJSRuntime singleton service and initializes it.
        /// </summary>
        public static IServiceCollection AddBlazorJSRuntime(this IServiceCollection _this)
        {
            serviceCollection = _this;
            // add IServiceCollection singleton
            _this.AddSingleton<IServiceCollection>(_this);
            // add BlazorJSRuntime and IBlazorJSRuntime singleton
            BlazorJSRuntime.JS = new BlazorJSRuntime();
            return _this.AddSingleton<BlazorJSRuntime>(BlazorJSRuntime.JS).AddSingleton<IBlazorJSRuntime>(BlazorJSRuntime.JS);
        }
        internal static Dictionary<Type, IBackgroundService?> Services { get; private set; } = new Dictionary<Type, IBackgroundService?>();

        /// <summary>
        /// Background services will have their InitAsync methods called in the order the were registered
        /// Background services must be careful to not take too long in their InitAsync methods as other services are waiting to init and the app is waiting to start
        /// </summary>
        static async Task<WebAssemblyHost> StartBackgroundServices(this WebAssemblyHost _this)
        {
            var bgServices = serviceCollection.Where(o => typeof(IBackgroundService).IsAssignableFrom(o.ServiceType) || typeof(IBackgroundService).IsAssignableFrom(o.ImplementationType)).ToList();
            // let all the constructors fire first
            foreach (var kvp in bgServices)
            {
#if DEBUG
                Console.WriteLine($"Getting background service: {kvp.ServiceType.Name}");
#endif
                var service = (IBackgroundService)_this.Services.GetRequiredService(kvp.ServiceType);
                Services[kvp.ServiceType] = service;
            }
            // call InitAsync on each
            foreach (var kvp in Services)
            {
#if DEBUG
                Console.WriteLine($"InitAsync background service: {kvp.Key.Name}");
#endif
                await kvp.Value.InitAsync();
            }
            return _this;
        }

        /// <summary>
        /// Use this method instead of RunAsync to enable BlazorJS support for IBackgroundService services registered with the IServiceCollection.AddBackgroundService method.
        /// And to also enable disable app rendering in workers to prevent unexpected behavior
        /// </summary>
        public static async Task BlazorJSRunAsync(this WebAssemblyHost _this, bool workersSkipPageRendering = true)
        {
            await _this.StartBackgroundServices();
            var tcs = new TaskCompletionSource<object>();
            if (BlazorJSRuntime.JS.IsWorker && workersSkipPageRendering)
            {
                // This is a worker so we are going to use this to allow services in workers without the html renderer trying to load pages
                await tcs.Task;
            }
            else
            {
                // run as normal
                await _this.RunAsync();
            }
        }
    }
}
