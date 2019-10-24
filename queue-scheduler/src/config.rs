pub use config::ConfigError;
use config::{Config, Environment};

#[derive(Debug)]
pub struct Database {
    pub user: String,
    pub password: String,
    pub host: String,
    pub dbname: String,
    pub url: String,
}

#[derive(Debug)]
pub struct Rollbar {
    pub enabled: bool,
    pub environment: String,
    pub token: String,
}

#[derive(Debug)]
pub struct AppConfig {
    pub database: Database,
    pub rollbar: Rollbar,
}

pub fn load() -> Result<AppConfig, ConfigError> {
    let mut cfg = Config::new();
    cfg.merge(Environment::with_prefix("DARK_CONFIG"))?;

    Ok(AppConfig {
        rollbar: Rollbar {
            enabled: cfg.get_str("ROLLBAR_ENABLED").map(|s| s == "y")?,
            environment: cfg.get_str("ROLLBAR_ENVIRONMENT")?,
            token: cfg.get_str("ROLLBAR_POST_SERVER_ITEM")?,
        },
        database: Database {
            user: cfg.get_str("DB_USER")?,
            password: cfg.get_str("DB_PASSWORD")?,
            host: cfg.get_str("DB_HOST")?,
            dbname: cfg.get_str("DB_DBNAME")?,
            url: format!(
                "postgres://{}:{}@{}/{}",
                cfg.get_str("DB_USER")?,
                cfg.get_str("DB_PASSWORD")?,
                cfg.get_str("DB_HOST")?,
                cfg.get_str("DB_DBNAME")?,
            ),
        },
    })
}
