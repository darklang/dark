// What JavaScript can call. F# has no generator for [JSExport], so the exports live here and
// pass straight through to the F# host (Lib/Host.fs).
using System.Runtime.InteropServices.JavaScript;
using Darklang.Wasm;

[assembly: System.Runtime.Versioning.SupportedOSPlatform("browser")]

public static partial class Exports
{
    public static void Main() => Startup.Start(Log);

    [JSImport("globalThis.console.log")]
    static partial void Log(string text);

    [JSExport] public static string DrainOutput() => Browser.DrainOutput();
    [JSExport] public static void PushKey(string key, bool ctrl, bool alt, bool shift) => Browser.PushKey(key, ctrl, alt, shift);
    [JSExport] public static void PushPaste(string text) => Browser.PushPaste(text);
    [JSExport] public static void SetTerminalSize(int cols, int rows) => Browser.SetTerminalSize(cols, rows);
    [JSExport] public static void SetEnv(string name, string value) => Cli.SetEnv(name, value);
    [JSExport] public static string RestoreOutcome() => Cli.RestoreOutcome();
    [JSExport] public static System.Threading.Tasks.Task Boot(string storeUrl) => Cli.Boot(storeUrl);

    [JSExport]
    public static System.Threading.Tasks.Task<int> RunCli([JSMarshalAs<JSType.Array<JSType.String>>] string[] args) => Cli.RunCli(args);

    [JSExport]
    public static System.Threading.Tasks.Task<string> RunCommand([JSMarshalAs<JSType.Array<JSType.String>>] string[] args) => Cli.RunCommand(args);
}
