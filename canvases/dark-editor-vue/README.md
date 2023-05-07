# Dark-Editor: Experiment 1

Build the vue app

- `cd canvases/dark-editor-vue`
- `npm install`
- `npm run build`

Editor

- TODO: set up your .secrets
- `./scripts/prep-experiment1`
- Go to http://dark-editor.dlio.localhost:11003
- Write a prompt
- Edit result to be a valid code example:

```
type Person = { name: Text; age: Int }

let getAge (person: Person): Int =
  person.age

[Person { name = "Alice"; age = 42 }]
 |> List.map(fun p -> getAge p)
```

- Run the code
- Check console for result
