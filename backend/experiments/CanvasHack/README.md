# CanvasHack

## Usage

- `./scripts/run-canvas-hack load-from-disk dark-editor`
  This will clear the `dark-editor` canvas prior to import, via `./scripts/clear-canvas.sh`,
  and then load the `dark-editor` canvas from disk (its handlers, fns, etc).

  after loading from disk, you may access the new endpoints at
  http://dark-editor.dlio.localhost:11001/

- `./scripts/run-canvas-hack save-from-disk`
