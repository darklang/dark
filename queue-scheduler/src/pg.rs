// use std::io;
use std::sync::Arc;
use std::time;

use backoff::Operation;
use slog::debug; // macros

use crate::errors;

pub fn connect(
    log: Arc<slog::Logger>,
    dburl: &str,
) -> Result<postgres::Connection, errors::FatalError> {
    let notifier = |e: postgres::Error, d: time::Duration| {
        debug!(log, "postgres.connect.error"; "error.msg" => format!("{}", e), "retry_in" => d.as_millis());
    };
    let mut boff = backoff::ExponentialBackoff {
        initial_interval: time::Duration::from_millis(500),
        max_interval: time::Duration::from_secs(1),
        max_elapsed_time: Some(time::Duration::from_secs(10)),
        ..Default::default()
    };

    let mut connect = || {
        postgres::Connection::connect(dburl, postgres::TlsMode::None)
            .map_err(backoff::Error::Transient) // TODO(ds): do we want to label some as Permanent?
    };

    connect
        .retry_notify(&mut boff, notifier)
        .map_err(|e| match e {
            backoff::Error::Permanent(e) => errors::FatalError::PostgresConnectError(e),
            backoff::Error::Transient(e) => errors::FatalError::PostgresConnectError(e),
        })
}
