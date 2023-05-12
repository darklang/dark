# Dark-Editor: Experiment 1

## Build the vue app

- `cd canvases/dark-editor-vue`
- `npm install`
- `npm run build`

## Load the canvas from disk

- follow instructions in `template.secrets` to set up secrets
- run `./scripts/prep-experiment1` to load the canvas from disk

## Try it out

- go view the app at http://dark-editor.dlio.localhost:11003
- write a prompt in the editor
  (i.e. `model a "Person". write a fn to extract their age. use the function to get the age of each person in a hardcoded list.`)
- click "Run"
- edit the result to be parse-able Darklang code
  e.g.

  ```
  type Person = { name: Text; age: Int }

  let getAge (person: Person): Int =
    person.age

  [Person { name = "Alice"; age = 42 }]
   |> List.map(fun p -> getAge p)
  ```

- eval the code
- check console logs for result
