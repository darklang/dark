# Wasm

Some of Dark's "backend" F# source code is compiled to WebAssemby to be usable by JS.

In Dark-Classic, it was used to support "analysis," in-editor immediate debugging
that looped in 'traces' -- recordings of the inputs/outputs of fn calls in the
eval of a Handler (HTTP, Cron, Queue, Script).

We'll likely use this for something similar in the new iteration of Dark, but
haven't used it quite yet, so this project is sitting around until we need it.