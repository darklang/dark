extern crate futures;
extern crate hmac;
extern crate hyper;
extern crate hyper_tls;
extern crate md5;
#[macro_use]
extern crate serde_json;
extern crate sha2;

mod config;
mod push;
mod service;

use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::Server;

fn main() {
    let addr = ([0, 0, 0, 0], config::port()).into();

    let make_service = || service_fn(service::handle::<push::Client>);

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
