use std::time::{Duration, SystemTime};

pub fn ms_duration(start: SystemTime) -> u64 {
    (start
        .elapsed()
        .unwrap_or_else(|_| Duration::new(0, 0))
        .as_millis()) as u64
}
