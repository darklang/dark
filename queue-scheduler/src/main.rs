use std::sync::Mutex;
use std::thread;
use std::time;

use slog::Drain; // allow treating Mutex as a Drain
use slog::{info, o}; // macros

fn main() {
    let t_start = time::Instant::now();

    let mut cfg = config::Config::default();
    cfg.merge(config::Environment::with_prefix("DARK_CONFIG"))
        .unwrap();

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

    // FIXME(dean) - parse into a config struct or something better
    let database_url = format!(
        "postgres://{}:{}@{}/{}",
        cfg.get_str("DB_USER").unwrap(),
        cfg.get_str("DB_PASSWORD").unwrap(),
        cfg.get_str("DB_HOST").unwrap(),
        cfg.get_str("DB_DBNAME").unwrap(),
    );
    let conn = postgres::Connection::connect(database_url, postgres::TlsMode::None).unwrap();

    loop {
        thread::sleep(time::Duration::from_secs(1));
        let rows = conn
            .query("SELECT COUNT(*) FROM events WHERE status = 'new'", &[])
            .unwrap();
        let count: i64 = rows.get(0).get(0);
        info!(log, "tick" ; "new_events.count" => count);
    }
}
