use std::env;
use std::fmt::Display;
use std::str::FromStr;

fn require_str(name: &str) -> String {
    env::var(name).unwrap_or_else(|_| panic!("{} must be set", name))
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

// TODO move me to push.rs
pub fn pusher_host() -> String {
    let cluster = require_str("DARK_CONFIG_PUSHER_CLUSTER");
    format!("api-{}.pusher.com", cluster)
}
