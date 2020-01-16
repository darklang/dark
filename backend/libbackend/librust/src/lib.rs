#[macro_use]
extern crate derive_ocaml;
#[macro_use]
extern crate ocaml;

#[ocaml_ffi]
pub fn random_bytes(n: usize) -> ocaml::List {
    let mut values = ocaml::List::new();
    for _ in 0..n {
        let v = ocaml::Value::i64(rand::random::<u8>() as i64);
        values.push_hd(v);
    }
    ocaml::List::from(values)
}
