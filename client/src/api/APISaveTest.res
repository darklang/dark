type rec t = string

let decode = (j): t => {
  open Json_decode_extended
  string(j)
}
