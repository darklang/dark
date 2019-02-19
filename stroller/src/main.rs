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

use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::Server;

fn main() {
    let addr = ([0, 0, 0, 0], config::port()).into();

    let shutting_down = Arc::new(AtomicBool::new(false));

    let manager = push::PusherClientManager::new(
        &config::pusher_cluster(),
        &config::pusher_app_id(),
        &config::pusher_key(),
        &config::pusher_secret(),
    );
    /*
     * This is a "connection pool" with only one connection, mostly as an easy
     * way to get a lazily-initialized connection that multiple threads can
     * safely share. As a bonus though, if we need to handle high volume, we
     * can have multiple Pusher connections per stroller instance by just
     * bumping up max_size below.
     */
    let pool = r2d2::Pool::builder()
        .max_size(1)
        .min_idle(Some(0))
        .build(manager)
        .unwrap();

    let make_service = move || {
        let pool = pool.clone();
        let shutting_down = shutting_down.clone();

        service_fn(move |req| service::handle(&shutting_down, &pool, req))
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
