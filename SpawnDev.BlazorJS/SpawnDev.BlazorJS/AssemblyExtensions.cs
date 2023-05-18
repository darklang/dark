using System.Reflection;

namespace SpawnDev.BlazorJS {
    public static class AssemblyExtensions {
        public static string GetAssemblyInformationalVersion(this Assembly _this) {
            var attr = _this.GetCustomAttribute<AssemblyInformationalVersionAttribute>();
            return attr == null ? "" : attr.InformationalVersion;
        }
        public static string GetAssemblyFileVersion(this Assembly _this) {
            var attr = _this.GetCustomAttribute<AssemblyFileVersionAttribute>();
            return attr == null ? "" : attr.Version;
        }
    }
}
