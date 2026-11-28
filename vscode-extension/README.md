# Darklang for VS Code

Darklang in VS Code, from `dark lsp`: diagnostics, hover, navigation, completion, live values,
and the package store as documents you can edit.

- Install `dark` and make sure it's on your `PATH`, or set `darklang.path`.
- Open a `.dark` file, or the Darklang view in the activity bar. The extension starts `dark lsp`
  and stops it when VS Code closes.
- Package items open from the store as `darkfs:/Owner/Module/name.fn.dark` documents. Saving one
  applies it, as `dark edit` does. They reload when the store changes.
- Above a store function: run it, its recorded runs and their timing, its effects, its tests.
  Beside each call: the value it gave in the newest run.
- The status bar shows your branch and draft; click it to switch branch or commit.

`docs/editors.md` in the repository describes the server and every feature.

## Building

```
cd vscode-extension
npm ci
npm run build      # type-checks and bundles to out/extension.js
npm run package    # darklang-<version>.vsix
```

In VS Code at the repo root, "Run the VS Code extension" (F5) opens a development window with it. Run `npm run build` first.
