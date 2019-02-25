mod config;
mod push;
mod service;
mod worker;

use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use std::sync::mpsc;
use std::thread;

use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::Server;

fn main() {
    let addr = ([0, 0, 0, 0], config::port()).into();

    let shutting_down = Arc::new(AtomicBool::new(false));

    // create 'infinite', non-blocking, multi-producer, single-consumer channel
    let (sender, receiver) = mpsc::channel::<service::CanvasEvent>();
    // lifetime needed to appease the borrow checker
    {
        let shutting_down = shutting_down.clone();
        thread::spawn(move || {
            worker::run(receiver, &shutting_down);
        });
    }

    let make_service = move || {
        let shutting_down = shutting_down.clone();
        let sender = sender.clone();

        service_fn(move |req| service::handle(&shutting_down, sender.clone(), req))
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
