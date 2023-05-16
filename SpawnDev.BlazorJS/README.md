
# NuGet

| Package | Description |
|---------|-------------|
|**[SpawnDev.BlazorJS](#spawndevblazorjs)** <br /> [![NuGet version](https://badge.fury.io/nu/SpawnDev.BlazorJS.svg)](https://www.nuget.org/packages/SpawnDev.BlazorJS)| Enhanced Blazor WebAssembly JavaScript interop |
|**[SpawnDev.BlazorJS.WebWorkers](#spawndevblazorjswebworkers)** <br /> [![NuGet version](https://badge.fury.io/nu/SpawnDev.BlazorJS.WebWorkers.svg)](https://www.nuget.org/packages/SpawnDev.BlazorJS.WebWorkers)| Blazor WebAssembly WebWorkers and SharedWebWorkers |


# SpawnDev.BlazorJS
[![NuGet](https://img.shields.io/nuget/dt/SpawnDev.BlazorJS.svg?label=SpawnDev.BlazorJS)](https://www.nuget.org/packages/SpawnDev.BlazorJS)

An easy-to-use library to interop with JavaScript from WebAssembly applications, designed for client-side "Blazor" development.

"Blazor" is the branding Microsoft has assigned to the ability of and tooling around compiling .NET (C#, F#, etc.) code to WebAssembly.

(TODO: reference official Blazor WebAssembly docs)
Without this library, .NET avails a few tools to interop between WASM-compiled .NET code and a JS "host" runtime:
- .NET code may expose functions to be used in JS (TODO: reference ways)
  (TODO: tiny demo)
- a `JS` obect with members like
  TODO: tiny demo
  TODO: reference relevant namespaces, etc.

If you're not familiar with those features, it may be worth:
- https://learn.microsoft.com/en-us/aspnet/core/blazor/javascript-interoperability/?view=aspnetcore-7.0
  - [Call JS from .NET](https://learn.microsoft.com/en-us/aspnet/core/blazor/javascript-interoperability/call-javascript-from-dotnet?view=aspnetcore-7.0)
  - [Call .NET from JS](https://learn.microsoft.com/en-us/aspnet/core/blazor/javascript-interoperability/call-dotnet-from-javascript?view=aspnetcore-7.0)
  - [JSImport](https://learn.microsoft.com/en-us/aspnet/core/blazor/javascript-interoperability/import-export-interop?view=aspnetcore-7.0)
    TODO: actually learn more about this
- reading the easy-to-read docs (TODO: link)
- stachu's blog post TODO
- reading the boring docs

## Features

Supports Blazor WebAssembly .NET 6, 7, and 8.

- Use JavaScript libraries in Blazor without writing any JavaScript code
- An alternative `JSRuntime` that wraps the default one adding additional functionality
- Create new JavaScript objects directly from Blazor
- Get and set JavaScript object properties as well as access methods.
- Easily pass .NET methods to JavaScript using the Callback.Create or Callback.CreateOne methods
- Wrap JavaScript objects for direct manipulation from Blazor
  - Easily access JavaScript objects by wrapping them in a simple interface that implements `IJSObject`
  - Alternatively use the `JSObject` base class to wrap your objects for more control
- Over 100 strongly typed `JSObject` wrappers included in BlazorJS including Promises, WebGL, WebRTC, DOM, etc...
- Use `SpawnDev.BlazorJS.WebWorkers` to enable calling Blazor services in web worker threads
- Supports Promises, passing undefined to JavaScript, and more
- Originally written for C#, but has F#-specific bindings as well

# BlazorJSRuntime

Getting started.

Using `BlazorJS` requires 2 changes to your `Program.cs` or `Program.fs`
- Add the `BlazorJSRuntime` service with `builder.Services.AddBlazorJSRuntime()`
- Initialize `BlazorJSRuntime` by calling `builder.Build().BlazorJSRunAsync()` instead of `builder.Build().RunAsync()`

```cs
// ... other usings
using SpawnDev.BlazorJS;

// ... normal builder code
var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");
// Services section
// Add SpawnDev.BlazorJS.BlazorJSRuntime
builder.Services.AddBlazorJSRuntime();
// ... more app services (such as WebWorkerService if needed)
// build and Init using BlazorJSRunAsync (instead of RunAsync)
await builder.Build().BlazorJSRunAsync();
```

And use.
```cs
[Inject]
BlazorJSRuntime JS { get; set; }

// Get Set
var innerHeight = JS.Get<int>("window.innerHeight");
JS.Set("document.title", "Hello World!");

// Call
var item = JS.Call<string?>("localStorage.getItem", "itemName");
JS.CallVoid("addEventListener", "resize", Callback.Create(() => Console.WriteLine("WindowResized"), _callBacks));
```



## IMPORTANT NOTE - Async vs Sync JavaScript calls

The `BlazorJSRuntime` behaves differently than the `JSRuntime` shipped normally in .NET. `BlazorJSRuntime` is more of a 1-to-1 mapping to JavaScript.
TODO: what does this mean^? How is the official one not a 1:1 mapping?

The official `JSRuntime` shipped in .NET allows you to call up on sy
Unlike with the official `JSRuntime` shipped in .NET, when using this library, you need to be consistent in defining functions as sync/async, and _using_ them as such.


### Async function calls

If a JS function returns a Promise, use `CallAsync`, `CallVoidAsync`, or `GetAsync` in .NET to call upon it.

```js
// JavaScript
async function AddNum(num1, num2){
    return num1 + num2;
}
```

```cs
// C#
var total = await JS.CallAsync<int>("AddNum", 20, 22);
// total == 42 here
```

### Synchronous function calls calls

If a JS function doesn't return a promise, use `Call`, `CallVoid`, or `Get` in .NET to call upon it.

```js
// JavaScript
function AddNum(num1, num2){
    return num1 + num2;
}
```

```cs
// C#
var total = JS.Call<int>("AddNum", 20, 22);
// total == 42 here
```


# `IJSInProcessObjectReference` extended

`IJSInProcessObjectReference` is an type exposed by .NET which allows you to ... TODO

```cs
// Get Set
var window = JS.Get<IJSInProcessObjectReference>("window");
window.Set("myVar", 5);
var myVar = window.Get<int>("myVar");

// Call
window.CallVoid("addEventListener", "resize", Callback.Create(() => Console.WriteLine("WindowResized")));
```

Create a new JavaScript object
```cs
IJSInProcessObjectReference worker = JS.New("Worker", myWorkerScript);
```

# Action and Func serialization
BlazorJS supports serialization of both Func and Action types. Internally the BlazorJS.Callback object is used. Serialized and deserialized Action and Func objects must call their DisposeJS() extension method to dispose the auto created and associated Callback and/or Function objects.

```cs
var tcs = new TaskCompletionSource<bool>();
var callback = () =>
{
    tcs.TrySetResult(true);
};
JS.CallVoid("setTimeout", callback, 100);
await tcs.Task;
callback.DisposeJS();
```

```cs
int testValue = 42;
var origFunc = new Func<int, int>((val) =>
{
    return val;
});
// set a global JavaScript var to our Func<int>
// if this is the first time this Func is passed to JavaScript a Callback will be created and associated to this Func for use in future serialization
// the auto created Callback must be disposed by calling the extension method Func.DisposeJS()
JS.Set("_funcCallback", origFunc);
// read back in our Func as an Func
// internally a JavaScript Function reference is created and associated with this Func.
// the auto created Function must be disposed by calling the extension method Func.DisposeJS()
var readFunc = JS.Get<Func<int, int>>("_funcCallback");
var readVal = readFunc(testValue);
if (readVal != testValue) throw new Exception("Unexpected result");
// dispose the Function created and associated with the read Func
readFunc.DisposeJS();
// dispose the Callback created and associated with the original Func
origFunc.DisposeJS();
```

# Callback

The Callback object is used internally to support Action and Func serialization. It can be used for a bit more control over the lifetime of you callbacks. Pass methods to JavaScript using the Callback.Create and Callback.CreateOne methods. These methods use type arguments to set the types expected for incoming arguments (if any) and the expected return type (if any.) async methods are passed as Promises.

Pass lambda callbacks to JavaScript
```cs
JS.Set("testCallback", Callback.Create<string>((strArg) => {
    Console.WriteLine($"JavaScript sent: {strArg}");
    // this prints "Hello callback!"
}));
```
```js
// in JavaScript
testCallback('Hello callback!');
```

Pass method callbacks to JavaScript
```cs
string SomeNetFn(string input){
    return $"Recvd: {input}";
}

JS.Set("someNetFn", Callback.CreateOne<string, string>(SomeNetFn));
```
```js
// in JavaScript
someNetFn('Hello callback!');

// prints
Recvd: Hello callback!
```

Pass async method callbacks to JavaScript
Under the hood, BlazorJS is returning a Promise to JavaScript when the method is called

```cs
async Task<string> SomeNetFnAsync(string input){
    return $"Recvd: {input}";
}

JS.Set("someNetFnAsync", Callback.CreateOne<string, string>(SomeNetFnAsync));
```
```js
// in JavaScript
await someNetFnAsync('Hello callback!');

// prints
Recvd: Hello callback!
```

# IJSObject Interface
SpawnDev.BlazorJS can now wrap JavaScript objects using interfaces. Just like objects derived from the JSObject class, IJSObject interfaces internally use IJSInProcessObjectReference to wrap a JavaScript object for direct manipulation and can be passed to and from JavaScript. The main difference is IJSObjects use DispatchProxy to implement the desired interface at runtime instead of requiring a type that inherits JSObject. Currently SpawnDev.BlazorJS does not provide any interfaces for JavaScript objects or apis but interfaces are simple to set up.

IJSObject Example
```cs
// create an interface for your JavaScript object that implements IJSObject
public interface IWindow : IJSObject
{
    string Name { get; set; }
    void Alert(string msg = "");
    // ...
}

// use your IJSObject interface to interact with the JavaScript object
public void IJSObjectInterfaceTest() {
    var w = JS.Get<IWindow>("window");
    var randName = Guid.NewGuid().ToString();
    // directly set the window.name property
    w.Name = randName;
    // verify the read back
    if (w.Name != randName) throw new Exception("Interface property set/get failed");
}
```

# JSObject Base Class

JSObjects are wrappers around IJSInProcessReference objects that can be passed to and from JavaScript and allow strongly typed access to the underlying object. JSObjects take a bit more work to set up but offer more versatility.

JSObject type wrapper example (same as the IJSObject interface example above but with JSObject)
```cs
// create a class for your JavaScript object that inherits from JSObject
public class Window : JSObject
{
    // required constructor
    public Window(IJSInProcessObjectReference _ref) : base(_ref) { }
    public string Name { get => JSRef.Get<string>("name"); set => JSRef.Set("name", value); }
    public void Alert(string msg = "") => JSRef.CallVoid(msg);
    // ...
}

// use the JSObject class to interact with the JavaScript object
public void JSObjectClassTest() {
    var w = JS.Get<Window>("window");
    var randName = Guid.NewGuid().ToString();
    // directly set the window.name property
    w.Name = randName;
    // verify the read back
    if (w.Name != randName) throw new Exception("Interface property set/get failed");
}
```

Use the extended functions of IJSInProcessObjectReference to work with JavaScript objects or use the growing library of over 100 of the most common JavaScript objects, including ones for Window, HTMLDocument, and more in SpawnDev.BlazorJS.JSObjects. JSObjects are wrappers around IJSInProcessObjectReference that allow strongly typed use.

Below shows a section of the SpawnDev.BlazorJS.JSObjects.Window class. Window's base type, EventTarget, inherits from JSObject.
```cs
public class Window : EventTarget {
    // all JSObject types must have this constructor
    public Window(IJSInProcessObjectReference _ref) : base(_ref) { }
    // here is a property with both getter and setter
    public string? Name { get => JSRef.Get<string>("name"); set => JSRef.Set("name", value); }
    // here are methods
    public long SetTimeout(Callback callback, double delay) => JSRef.Call<long>("setTimeout", callback, delay);
    public void ClearTimeout(long requestId) => JSRef.CallVoid("clearTimeout", requestId);
    // ...
}
```

Below the JSObject derived Window class is used
```cs
// below the JSObject derived Window class is used
using var window = JS.Get<Window>("window");
var randName = Guid.NewGuid().ToString();
// set and get properties
window.Name = randName;
var name = window.Name;
// call methods
window.Alert("Hello!");
```

## Promise
SpawnDev.BlazorJS.JSObjects.Promise - is a JSObject wrapper for the JavaScript Promise class.
Promises can be created in .Net to wrap async methods or Tasks. They are essentially JavaScript's version of Task.

Ways to create a Promise in .Net
```cs
var promise = new Promise();
// pass to JavaScript api
...
// then later resolve
promise.Resolve();
```

Create Promise from lambda
```cs
var promise = new Promise(async () => {
    await Task.Delay(5000);
});
// pass to JavaScript api

```
Create Promise from lambda with return value
```cs
var promise = new Promise<string>(async () => {
    await Task.Delay(5000);
    return "Hello world!";
});
// pass to JavaScript api
```
Create Promise from Task
```cs
var taskSource = new TaskCompletionSource<string>();
var promise = new Promise<string>(taskSource.Task);
// pass to JavaScript api
...
// then later resolve
taskSource.TrySetResult("Hello world!");
```

Below is a an example that uses Promises to utilize the [Web Locks API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Locks_API)

```cs
using var navigator = JS.Get<Navigator>("navigator");
using var locks = navigator.Locks;

Console.WriteLine($"lock: 1");

using var waitLock = locks.Request("my_lock", Callback.CreateOne((Lock lockObj) => new Promise(async () => {
    Console.WriteLine($"lock acquired 3");
    await Task.Delay(5000);
    Console.WriteLine($"lock released 4");
})));

using var waitLock2 = locks.Request("my_lock", Callback.CreateOne((Lock lockObj) => new Promise(async () => {
    Console.WriteLine($"lock acquired 5");
    await Task.Delay(5000);
    Console.WriteLine($"lock released 6");
})));

Console.WriteLine($"lock: 2");
```

## Custom JSObjects
Implement your own JSObject classes for JavaScript objects not already available in the BlazorJS.JSObjects library.

Instead of this (simple but not as reusable)
```cs
var audio = JS.New("Audio", "https://some_audio_online");
audio.CallVoid("play");
```
You can do this...
Create a custom JSObject wrapper
```cs
public class Audio : JSObject
{
    public Audio(IJSInProcessObjectReference _ref) : base(_ref) { }
    public Audio(string url) : base(JS.New("Audio", url)) { }
    public void Play() => JSRef.CallVoid("play");
}
```

Then use your new object
```cs
var audio = new Audio("https://some_audio_online");
audio.Play();
```


# SpawnDev.BlazorJS.WebWorkers
[![NuGet](https://img.shields.io/nuget/dt/SpawnDev.BlazorJS.WebWorkers.svg?label=SpawnDev.BlazorJS.WebWorkers)](https://www.nuget.org/packages/SpawnDev.BlazorJS.WebWorkers)

- Easily call Blazor Services in separate threads with WebWorkers and SharedWebWorkers

- Works in Blazor WASM .Net 6, 7, and 8.

- Does not require SharedArrayBuffer and therefore does not require the special HTTP headers associated with using it.

- Supports and uses transferable objects whenever possible

Tested working in the following browsers (tested with .Net 8.) Chrome Android does not currently support SharedWorkers.

| Browser         | WebWorker Status | SharedWebWorker Status |
|-----------------|------------------|------------------------|
| Chrome          | ✔ | ✔ |
| MS Edge         | ✔ | ✔ |
| Firefox         | ✔ | ✔ |
| Chrome Android  | ✔ | ❌ (SharedWorker not supported by browser) |
| MS Edge Android | ✔ | ❌ (SharedWorker not supported by browser) |
| Firefox Android | ✔ | ✔ |

Firefox WebWorkers note:
Firefox does not support dynamic modules in workers, which originally made BlazorJS.WebWorkers fail in that browser.
The web worker script now tries to detect this and changes the blazor wasm scripts before they are loaded to workaround this limitation. It is possible some other browsers may have this issue but may not be detected properly.

Issues can be reported [here](https://github.com/LostBeard/SpawnDev.BlazorJS/issues) on GitHub.

Example WebWorkerService setup and usage.

```cs
// Program.cs
...
using SpawnDev.BlazorJS;
using SpawnDev.BlazorJS.WebWorkers;

var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");


builder.Services.AddBlazorJSRuntime();


builder.Services.AddWebWorkerService();

builder.Services.AddSingleton<IMathsService, MathsService>();

builder.Services.AddScoped((sp) => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });


await builder.Build().BlazorJSRunAsync();
```

## WebWorker
```cs

// Create a WebWorker

[Inject]
WebWorkerService workerService { get; set; }

 // ...

var webWorker = await workerService.GetWebWorker();

// Call GetService<ServiceInterface> on a web worker to get a proxy for the service on the web worker.
// GetService can only be called with Interface types
var workerMathService = webWorker.GetService<IMathsService>();

// Call async methods on your worker service
var result = await workerMathService.CalculatePi(piDecimalPlaces);

// Action types can be passed for progress reporting
var result = await workerMathService.CalculatePiWithActionProgress(piDecimalPlaces, new Action<int>((i) =>
{
    // the worker thread can call this method to report progress if desired
    piProgress = i;
    StateHasChanged();
}));
```

## SharedWebWorker
Calling GetSharedWebWorker in another window with the same sharedWorkerName will return the same SharedWebWorker
```cs
// Create or get SHaredWebWorker with the provided sharedWorkerName
var sharedWebWorker = await workerService.GetSharedWebWorker("workername");

// Just like WebWorker but shared
var workerMathService = sharedWebWorker.GetService<IMathsService>();

// Call async methods on your shared worker service
var result = await workerMathService.CalculatePi(piDecimalPlaces);

```

## Send events
```cs
// Optionally listen for event messages
worker.OnMessage += (sender, msg) =>
{
    if (msg.TargetName == "progress")
    {
        PiProgress msgData = msg.GetData<PiProgress>();
        piProgress = msgData.Progress;
        StateHasChanged();
    }
};

// From SharedWebWorker or WebWorker threads send an event to connected parents
workerService.SendEventToParents("progress", new PiProgress { Progress = piProgress });

// Or on send an event to a connected worker
webWorker.SendEvent("progress", new PiProgress { Progress = piProgress });
```

## Worker Transferable JSObjects

[Faster is better.](https://developer.chrome.com/blog/transferable-objects-lightning-fast/) SpawnDev WebWorkers use [transferable objects](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Transferable_objects) by default for better performance, but it can be disabled with WorkerTransferAttribute. Setting WorkerTransfer to false will cause the property, return value, or parameter to be copied to the receiving thread instead of transferred.

Example
```cs
public class ProcessFrameResult : IDisposable
{
    [WorkerTransfer(false)]
    public ArrayBuffer? ArrayBuffer { get; set; }
    public byte[]? HomographyBytes { get; set; }
    public void Dispose(){
        ArrayBuffer?.Dispose();
    }
}

[return: WorkerTransfer(false)]
public async Task<ProcessFrameResult?> ProcessFrame([WorkerTransfer(false)] ArrayBuffer? frameBuffer, int width, int height, int _canny0, int _canny1, double _needlePatternSize)
{
    var ret = new ProcessFrameResult();
    // ...
    return ret;
}
```

In the above example; the WorkerTransferAttribute on the return type set to false will prevent all properties of the return type from being transferred.

### Transferable JSObject types
ArrayBuffer
MessagePort
TransformStream
AudioData
ImageBitmap
VideoFrame
OffscreenCanvas
RTCDataChannel

# IDisposable
NOTE: The above code shows quick examples. Some objects implement IDisposable, such as JSObject, Callback, and IJSInProcessObjectReference types.

JSObject types will dispose of their IJSInProcessObjectReference object when their finalizer is called if not previously disposed.

Callback types must be disposed unless created with the Callback.CreateOne method, in which case they will dispose themselves after the first callback. Disposing a Callback prevents it from being called.

IJSInProcessObjectReference does not dispose of interop resources with a finalizer and MUST be disposed when no longer needed. Failing to dispose these will cause memory leaks.

IDisposable objects returned from a WebWorker or SharedWorker service are automatically disposed after the data has been sent to the calling thread.

# Support

Issues can be reported [here](https://github.com/LostBeard/SpawnDev.BlazorJS/issues) on GitHub.

Inspired by Tewr's BlazorWorker implementation. Thank you! I wrote my implementation from scratch as I needed workers in .Net 7.
https://github.com/Tewr/BlazorWorker

BlazorJS and WebWorkers Demo
https://blazorjs.spawndev.com/

Buy me a coffee

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=7QTATH4UGGY9U)
