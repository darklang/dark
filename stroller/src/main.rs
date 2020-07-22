mod config;
mod heapio;
mod push;
mod service;
mod util;
mod worker;

use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use std::sync::mpsc;
use std::thread;

use hyper::rt::Future;
use hyper::service::service_fn;
use hyper::Server;

use crate::worker::{PusherMessage, WorkerTerminationReason};

use slog::o;
use slog_scope::{error, info};

use slog::{Drain, FnValue, PushFnValue, Record};
use std::sync::Mutex;

fn main() {
    let log = slog::Logger::root(
        Mutex::new(
            slog_json::Json::new(std::io::stdout())
                .set_pretty(false) // for honeytail compatibility
                .add_key_value(o!(
                           "timestamp" => PushFnValue(move |_ : &Record, ser| {
                    ser.emit(chrono::Local::now().to_rfc3339())
                }),
                "level" => FnValue(move |rinfo : &Record| {
                    rinfo.level().as_short_str()
                }),
                "msg" => PushFnValue(move |record : &Record, ser| {
                    ser.emit(record.msg())
                })))
                .build(),
        )
        .map(slog::Fuse),
        o!("file" => FnValue(move |info| {
            format!("{}:{} {}",
                    info.file(),
                    info.line(),
                    info.module()) })
        ),
    );
    // setting the global logger here so we can use slog_scope::logger() everywhere else
    let _guard = slog_scope::set_global_logger(log);

    let addr = ([0, 0, 0, 0], config::port()).into();

    let shutting_down = Arc::new(AtomicBool::new(false));

    // create 'infinite', non-blocking, multi-producer, single-consumer channel
    let (pusher_sender, pusher_receiver) = mpsc::channel::<PusherMessage>();
    thread::spawn(move || match worker::run(pusher_receiver) {
        WorkerTerminationReason::ViaDie => std::process::exit(0),
        WorkerTerminationReason::SendersDropped => std::process::exit(1),
    });

    // create 'infinite', non-blocking, multi-producer, single-consumer channel
    let (heapio_sender, heapio_receiver) = mpsc::channel::<heapio::HeapioMessage>();
    thread::spawn(move || match heapio::run(heapio_receiver) {
        heapio::WorkerTerminationReason::ViaDie => std::process::exit(0),
        heapio::WorkerTerminationReason::SendersDropped => std::process::exit(1),
    });

    let make_service = move || {
        let shutting_down = shutting_down.clone();
        let pusher_sender = pusher_sender.clone();
        let heapio_sender = heapio_sender.clone();

        service_fn(move |req| {
            service::handle(
                &shutting_down,
                pusher_sender.clone(),
                heapio_sender.clone(),
                req,
            )
        })
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| error!("server error: {}", e));

    info!("Listening on {}", addr);
    hyper::rt::run(server);
}
