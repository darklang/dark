# Experiment 1

## Demo:

Build the vue app

- `cd canvases/dark-editor-vue`
- `npm install`
- `npm run build`

Editor

- In canvases/dark-editor/main.dark `/api/gpt4` handler add an openai api key
- `./scripts/prep-experiment1`
- Go to http://dark-editor.dlio.localhost:11003/
- Write a prompt
- Edit result to be a valid code example:

```
(
  let a = 1 + 2
  List.repeat 10 a
)
```

- Run the code
- Check console for result
