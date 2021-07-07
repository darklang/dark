
namespace WasmDelegate
{
  // This entire project exists to provide just this delegate. This is needed
  // to call CreateDelegate in Wasm.fs. However, I don't seem to be able to
  // figure out how to specify this exact type in F#, so we need a C#
  // declaration, which needs its own project.
  public delegate object InvokeDelegate(string method, params object[]
      parameters);
}
