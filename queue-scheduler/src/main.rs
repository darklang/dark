use std::sync::Mutex;
use std::thread;
use std::time;

use slog::Drain; // allow treating Mutex as a Drain
use slog::{info, o}; // macros

mod config;
mod scheduler;
mod stats;

const TICK_INTERVAL: time::Duration = time::Duration::from_secs(1);

#[derive(Debug)]
enum FatalError {
    PostgresError(postgres::Error),
}

impl From<postgres::Error> for FatalError {
    fn from(e: postgres::Error) -> Self {
        FatalError::PostgresError(e)
    }
}

struct Looper {
    conn: postgres::Connection,
    log: slog::Logger,
    stats: stats::EventStats,
}

impl Looper {
    fn new(conn: postgres::Connection, log: slog::Logger) -> Looper {
        Looper {
            conn,
            log,
            stats: stats::EventStats::new(),
        }
    }

    fn every(&mut self, d: time::Duration) -> Result<(), FatalError> {
        loop {
            thread::sleep(d);
            self.tick()?;
        }
    }

    fn tick(&mut self) -> Result<(), FatalError> {
        let t_start = time::Instant::now();

        let newly_scheduled = scheduler::schedule_events(&self.conn)?;
        self.stats.fetch(&self.conn)?;

        info!(self.log, "tick" ;
        "duration" => t_start.elapsed().as_secs_f64() * 1000.0,
        "events.newly_scheduled" => newly_scheduled,
        "events.new_count" => self.stats.new,
        "events.scheduled_count" => self.stats.scheduled,
        );

        Ok(())
    }
}

fn main() {
    let t_start = time::Instant::now();

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

    let cfg = config::load().unwrap();
    let conn = postgres::Connection::connect(cfg.database.url, postgres::TlsMode::None).unwrap();

    let mut looper = Looper::new(conn, log);

    // FIXME: rollbar before crash
    looper.every(TICK_INTERVAL).unwrap();
}
