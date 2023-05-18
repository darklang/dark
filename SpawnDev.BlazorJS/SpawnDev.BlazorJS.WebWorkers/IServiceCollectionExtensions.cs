using Microsoft.Extensions.DependencyInjection;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SpawnDev.BlazorJS.WebWorkers
{
    public static class IServiceCollectionExtensions
    {
        public static IServiceCollection AddWebWorkerService(this IServiceCollection _this)
        {
            return _this.AddSingleton<WebWorkerService>();
        }
    }
}
