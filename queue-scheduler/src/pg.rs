use std::sync::Arc;
use std::time::Duration;

use backoff::Error::{Permanent, Transient};
use backoff::Operation; // trait for retry_notify
use failure::Error;
use slog::debug;

pub fn connect(log: Arc<slog::Logger>, dburl: &str) -> Result<postgres::Connection, Error> {
    let notifier = |e: postgres::Error, d: Duration| {
        debug!(log, "postgres.connect.error";
        "error.msg" => e.to_string(),
        "retry_in" => d.as_millis()
        );
    };
    let mut boff = backoff::ExponentialBackoff {
        initial_interval: Duration::from_millis(500),
        max_interval: Duration::from_secs(1),
        max_elapsed_time: Some(Duration::from_secs(10)),
        ..Default::default()
    };

    let mut connect = || {
        // TODO(ds): do we want to label some as Permanent?
        postgres::Connection::connect(dburl, postgres::TlsMode::None).map_err(Transient)
    };

    connect
        .retry_notify(&mut boff, notifier)
        .map_err(|e| match e {
            Permanent(e) | Transient(e) => Error::from_boxed_compat(Box::new(e)),
        })
}
