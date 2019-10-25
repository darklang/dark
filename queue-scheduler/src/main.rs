use std::fmt;
use std::thread;
use std::time;

use std::error::Error;
use std::sync::Arc;
use std::sync::Mutex;

use slog::Drain; // allow treating Mutex as a Drain
use slog::{error, info, o}; // macros

mod config;
mod scheduler;
mod stats;

const TICK_INTERVAL: time::Duration = time::Duration::from_secs(1);

#[derive(Debug)]
enum FatalError {
    PostgresError(postgres::Error),
}

impl Error for FatalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            FatalError::PostgresError(e) => Some(e),
        }
    }
}

impl fmt::Display for FatalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FATAL: {:?}", self)
    }
}

impl From<postgres::Error> for FatalError {
    fn from(e: postgres::Error) -> Self {
        FatalError::PostgresError(e)
    }
}

struct Looper {
    conn: postgres::Connection,
    log: Arc<slog::Logger>,
}

impl Looper {
    fn new(conn: postgres::Connection, log: Arc<slog::Logger>) -> Looper {
        Looper { conn, log }
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
        let stats = stats::fetch(&self.conn)?;

        info!(*self.log, "tick" ;
        "duration" => t_start.elapsed().as_secs_f64() * 1000.0,
        "events.newly_scheduled" => newly_scheduled,
        "events.new_count" => stats.new,
        "events.scheduled_count" => stats.scheduled,
        );

        Ok(())
    }
}

fn main() {
    let t_start = time::Instant::now();

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
    let reporter = Arc::new(ErrorReporter::new(log.clone(), cfg.rollbar));
    {
        let panic_reporter = reporter.clone();
        std::panic::set_hook(Box::new(move |info| panic_reporter.panic_hook(info)));
    }

    // Make a database connection and then kick off the looper
    match postgres::Connection::connect(cfg.database.url, postgres::TlsMode::None)
        .or_else(|e| Err(FatalError::PostgresError(e)))
        .and_then(|conn| Looper::new(conn, log.clone()).every(TICK_INTERVAL))
    {
        Ok(_) => {}
        Err(e) => {
            reporter.report_error(e).join().unwrap();
        }
    }
}

struct ErrorReporter {
    log: Arc<slog::Logger>,
    rollc: rollbar::Client,
}

impl ErrorReporter {
    fn new(log: Arc<slog::Logger>, cfg: config::Rollbar) -> ErrorReporter {
        let rollc = if cfg.enabled {
            rollbar::Client::new(cfg.token, cfg.environment)
        } else {
            rollbar::Client::new("no-token", "no-env")
        };
        ErrorReporter { rollc, log }
    }

    fn panic_hook(&self, panic_info: &std::panic::PanicInfo<'_>) {
        let error_msg = if let Some(msg) = panic_info.payload().downcast_ref::<&str>() {
            msg
        } else if let Some(msg) = panic_info.payload().downcast_ref::<String>() {
            msg
        } else {
            "<unknown>"
        };
        error!(*self.log, "panic"; "error.msg" => error_msg,);

        let backtrace = backtrace::Backtrace::new();
        self.rollc
            .build_report()
            .from_panic(panic_info)
            .with_backtrace(&backtrace)
            .send()
            .join()
            .unwrap();
    }

    fn report_error<E: Error>(
        &self,
        err: E,
    ) -> thread::JoinHandle<Option<rollbar::ResponseStatus>> {
        error!(*self.log, "fatal"; "error.msg" => format!("{}", err));
        let c = &self.rollc;
        rollbar::report_error!(c, err)
    }
}
