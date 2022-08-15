
// Supports an API used to save a canvas as JSON, to use in integration tests

type rec t = string

let decode = (j): t => {
  open Json_decode_extended
  string(j)
}
