extern crate futures;
extern crate hmac;
extern crate hyper;
extern crate hyper_tls;
extern crate md5;
extern crate native_tls;
extern crate r2d2;
#[macro_use]
extern crate serde_derive;
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

    let manager = push::PusherClientManager::new(
        &config::pusher_cluster(),
        &config::pusher_app_id(),
        &config::pusher_key(),
        &config::pusher_secret(),
    );
    let pool = r2d2::Pool::builder()
        .max_size(1)
        .min_idle(Some(0))
        .build(manager)
        .expect("TODO");

    let make_service = move || {
        let pool = pool.clone();
        service_fn(move |req| {
            let client = pool.get().expect("TODO");

            service::handle(client, req)
        })
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
