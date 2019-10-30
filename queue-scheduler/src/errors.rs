use std::error::Error;
use std::fmt;
use std::sync::Arc;
use std::thread;

use slog::error; // macro

use crate::config;

#[derive(Debug)]
pub enum FatalError {
    GenericPostgresError(postgres::Error),
    PostgresConnectError(postgres::Error),
}

impl Error for FatalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            FatalError::GenericPostgresError(e) => Some(e),
            FatalError::PostgresConnectError(e) => Some(e),
        }
    }
}

impl fmt::Display for FatalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FatalError::GenericPostgresError(e) => {
                write!(f, "FatalError::GenericPostgresError: {}", e)
            }
            FatalError::PostgresConnectError(e) => {
                write!(f, "FatalError::PostgresConnectError: {}", e)
            }
        }
    }
}

impl From<postgres::Error> for FatalError {
    fn from(e: postgres::Error) -> Self {
        FatalError::GenericPostgresError(e)
    }
}

pub struct ErrorReporter {
    log: Arc<slog::Logger>,
    rollc: rollbar::Client,
}

impl ErrorReporter {
    pub fn new(log: Arc<slog::Logger>, cfg: config::Rollbar) -> ErrorReporter {
        let rollc = if cfg.enabled {
            rollbar::Client::new(cfg.token, cfg.environment)
        } else {
            rollbar::Client::new("no-token", "no-env")
        };
        ErrorReporter { rollc, log }
    }

    pub fn panic_hook(&self, panic_info: &std::panic::PanicInfo<'_>) {
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

    pub fn report_error<E: Error>(
        &self,
        err: E,
    ) -> thread::JoinHandle<Option<rollbar::ResponseStatus>> {
        error!(*self.log, "fatal"; "error.msg" => format!("{}", err));
        let c = &self.rollc;
        rollbar::report_error!(c, err)
    }
}
