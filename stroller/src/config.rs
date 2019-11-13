use std::env;
use std::fmt::Display;
use std::str::FromStr;

fn require_str(name: &str) -> String {
    env::var(name).unwrap_or_else(|_| panic!("{} must be set", name))
}

fn maybe_str(name: &str) -> Option<String> {
    env::var(name).ok()
}

fn require_parse<T>(name: &str) -> T
where
    T: FromStr,
    <T as FromStr>::Err: Display,
{
    let raw = require_str(name);
    raw.parse()
        .unwrap_or_else(|e| panic!("invalid {}: {:?}: {}", name, raw, e))
}

pub fn port() -> u16 {
    require_parse("DARK_CONFIG_STROLLER_PORT")
}

pub fn pusher_app_id() -> String {
    require_str("DARK_CONFIG_PUSHER_APP_ID")
}

pub fn pusher_key() -> String {
    require_str("DARK_CONFIG_PUSHER_KEY")
}

pub fn pusher_secret() -> String {
    require_str("DARK_CONFIG_PUSHER_SECRET")
}

pub fn pusher_cluster() -> String {
    require_str("DARK_CONFIG_PUSHER_CLUSTER")
}

pub fn segment_write_key() -> Option<String> {
    maybe_str("DARK_CONFIG_SEGMENT_WRITE_KEY")
}
