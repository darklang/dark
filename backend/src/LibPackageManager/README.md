This exists to fetch package items from a Dark-hosted canvas, over HTTP.

There are a handful of endpoints that return package types, constants, and functions.

This project contains...

- the types equivalent to the json data returned by the endpoints
- the functions to fetch the data from the endpoints
- the functions to convert the json data into the types
  (there's some one-off Json parsing and decoding in here, that should probably be extracted out,
  mostly because our application is AOT-compiled and we can't use traditional reflection-based
  approaches like those in STJ or Newtonsoft.Json here. I tried using Thoth but didn't love the API.)

CLEANUP this README
CLEANUP the JSON magic
