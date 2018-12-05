use std::env;

fn require_str(name: &str) -> String {
    env::var(name).expect(&format!("{} must be set", name))
}

pub fn db_host() -> String {
    require_str("DARK_CONFIG_DB_HOST")
}

pub fn db_dbname() -> String {
    require_str("DARK_CONFIG_DB_DBNAME")
}

pub fn db_user() -> String {
    require_str("DARK_CONFIG_DB_USER")
}

pub fn db_password() -> String {
    require_str("DARK_CONFIG_DB_PASSWORD")
}
