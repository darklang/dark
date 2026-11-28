#!/usr/bin/env bash
# What the browser build needs on top of the dev image, which stays as it is for everyone who
# never builds this. Once per container.
#
# - the wasm-tools workload: emscripten, the browser runtime packs and the AOT compiler (~1 GB).
# - the C# code-style targets file. The SDK imports it for every C# project, and the dev image
#   deletes it along with the SDK's `cs` (Czech) messages. Wasm.csproj runs no style analyzers,
#   so an empty project does.
set -euo pipefail
if ! dotnet workload list 2>/dev/null | grep -q "^wasm-tools "; then
  echo "installing the wasm-tools workload (once per container, a few minutes)"
  sudo dotnet workload install wasm-tools
fi

for sdk in /usr/share/dotnet/sdk/*/Sdks/Microsoft.NET.Sdk; do
  targets="$sdk/codestyle/cs/build/Microsoft.CodeAnalysis.CSharp.CodeStyle.targets"
  if [[ ! -f "$targets" ]]; then
    sudo mkdir -p "$(dirname "$targets")"
    echo "<Project />" | sudo tee "$targets" > /dev/null
  fi
done
