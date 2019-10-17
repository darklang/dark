use std::sync::Mutex;
use std::thread;
use std::time::{Duration, Instant};

use slog::Drain; // allow treating Mutex as a Drain
use slog::{info, o}; // macros

fn main() {
    let t_start = Instant::now();

    let log = slog::Logger::root(
        Mutex::new(
            slog_json::Json::new(std::io::stdout())
                .set_pretty(false) // for honeytail compatibility
                .add_key_value(o!(
                "meta.name" => "queue-scheduler",
                "meta.process_age_s" => slog::PushFnValue(move |_, s| {
                    s.emit(t_start.elapsed().as_secs())
                }),
                "timestamp" => slog::PushFnValue(move |_, s| {
                    s.emit(chrono::Local::now().to_rfc3339())
                }),
                "msg" => slog::PushFnValue(move |rec, s| {
                    s.emit(rec.msg())
                })))
                .build(),
        )
        .map(slog::Fuse),
        o!(),
    );

    loop {
        thread::sleep(Duration::from_secs(1));
        info!(log, "tick")
    }
}
