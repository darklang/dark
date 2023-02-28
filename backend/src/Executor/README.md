# Executor

The executor is roughly Wasm.fsproj from before. It takes dark textual or json
code and parses it, returning either analysis results (many results, one for each
expression) or just one result

The executor will be called darklang-executor:

```bash
$ darklang-executor \*.dark # executes dark files, spits out JSON
$ darklang-executor # reads and executes stdin, spits outf JSON
$ darklang-executor serve -p 3004 # creates HTTP server running at localhost:3004. The API spec:
```

### /execute-text POST

input type: `{ code: string, symtable: Map<string,dval> }`
output type: `dval`

### /execute-json POST

input type: `{ code: ProgramTypes.Expr, symtable: Map<string,dval> }`
output type: `dval`
