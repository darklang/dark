use std::sync::Arc;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use slog::o;
use slog::Drain; // allow treating Mutex as a Drain

use scheduler::config;
use scheduler::errors;
use scheduler::pg;
use scheduler::Looper;

const TICK_INTERVAL: Duration = Duration::from_secs(1);

fn main() {
    let t_start = Instant::now();

    // it would be nice to rollbar here if the config is invalid, but we've got a bit of a chicken
    // & egg problem, given we need a bunch of rollbar config to initialize the rollbar client.
    let cfg = config::load().unwrap();

    // Our logger has to be behind an Arc because we pass it all over.
    let log = Arc::new(slog::Logger::root(
        Mutex::new(
            slog_json::Json::new(std::io::stdout())
                .set_pretty(false) // for honeytail compatibility
                .add_key_value(o!(
                "meta.process_id" => rand::random::<u64>(),
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
    ));

    // Set up error reporter and global panic hook.
    let reporter = Arc::new(errors::ErrorReporter::new(log.clone(), cfg.rollbar));
    {
        let panic_reporter = reporter.clone();
        std::panic::set_hook(Box::new(move |info| panic_reporter.panic_hook(info)));
    }

    // Make a database connection and then kick off the looper
    match pg::connect(log.clone(), &cfg.database.url)
        .and_then(|conn| Looper::new(conn, log.clone()).every(TICK_INTERVAL))
    {
        Ok(_) => {}
        Err(e) => {
            reporter.report_error(e).join().unwrap();
        }
    }
}
