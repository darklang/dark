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

use crate::worker::{Message, WorkerTerminationReason};

#[macro_use(o, slog_info, slog_error)]
extern crate slog;

extern crate slog_json;
#[macro_use(info, error)]
extern crate slog_scope;

extern crate chrono;
extern crate uuid;

use slog::{Drain, FnValue, PushFnValue, Record};
use std::sync::Mutex;

fn main() {
    //let plain = slog_term::PlainSyncDecorator::new(std::io::stdout());
    let log = slog::Logger::root(
        Mutex::new(
            slog_json::Json::new(std::io::stdout())
                .set_pretty(true)
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
    let _guard = slog_scope::set_global_logger(log);

    let addr = ([0, 0, 0, 0], config::port()).into();

    let shutting_down = Arc::new(AtomicBool::new(false));

    // create 'infinite', non-blocking, multi-producer, single-consumer channel
    let (sender, receiver) = mpsc::channel::<Message>();
    thread::spawn(move || match worker::run(receiver) {
        WorkerTerminationReason::ViaDie => std::process::exit(0),
        WorkerTerminationReason::SendersDropped => std::process::exit(1),
    });

    let make_service = move || {
        let shutting_down = shutting_down.clone();
        let sender = sender.clone();

        service_fn(move |req| service::handle(&shutting_down, sender.clone(), req))
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| error!("server error: {}", e));

    info!("Listening on {}", addr);
    hyper::rt::run(server);
}
