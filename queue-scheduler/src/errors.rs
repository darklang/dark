use std::sync::Arc;
use std::thread;

use failure::Error;
use slog::error;

use crate::config;

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

    pub fn report_error(&self, err: Error) -> thread::JoinHandle<Option<rollbar::ResponseStatus>> {
        error!(*self.log, "fatal"; "error.msg" => err.to_string());

        // only print when !release
        if cfg!(debug_assertions) {
            println!("{}", err.backtrace());
        }
        let c = &self.rollc;
        let rerr = err.compat();
        rollbar::report_error!(c, rerr)
    }
}
