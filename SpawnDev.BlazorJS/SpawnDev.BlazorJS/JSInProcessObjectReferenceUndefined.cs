using Microsoft.JSInterop;
using System.Diagnostics.CodeAnalysis;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS
{
    public class JSInProcessObjectReferenceUndefined : IJSInProcessObjectReference
    {
        [JsonPropertyName("__undefinedref__")]
        public bool UndefinedTag { get; } = true;
        public void Dispose() { }
        public ValueTask DisposeAsync() => ValueTask.CompletedTask;
        public TValue Invoke<[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors | DynamicallyAccessedMemberTypes.PublicFields | DynamicallyAccessedMemberTypes.PublicProperties)] TValue>(string identifier, params object?[]? args) => throw new NotImplementedException();
        public ValueTask<TValue> InvokeAsync<[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors | DynamicallyAccessedMemberTypes.PublicFields | DynamicallyAccessedMemberTypes.PublicProperties)] TValue>(string identifier, object?[]? args) => throw new NotImplementedException();
        public ValueTask<TValue> InvokeAsync<[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors | DynamicallyAccessedMemberTypes.PublicFields | DynamicallyAccessedMemberTypes.PublicProperties)] TValue>(string identifier, CancellationToken cancellationToken, object?[]? args) => throw new NotImplementedException();
    }
}