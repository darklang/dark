use config::{Config, Environment};
use failure::Error;

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

pub fn load_database(cfg: &Config) -> Result<Database, Error> {
    Ok(Database {
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
    })
}

pub fn load_rollbar(cfg: &Config) -> Result<Rollbar, Error> {
    Ok(Rollbar {
        enabled: cfg.get_str("ROLLBAR_ENABLED").map(|s| s == "y")?,
        environment: cfg.get_str("ROLLBAR_ENVIRONMENT")?,
        token: cfg.get_str("ROLLBAR_POST_SERVER_ITEM")?,
    })
}

pub fn load() -> Result<AppConfig, Error> {
    let mut cfg = Config::new();
    cfg.merge(Environment::with_prefix("DARK_CONFIG"))?;

    Ok(AppConfig {
        rollbar: load_rollbar(&cfg)?,
        database: load_database(&cfg)?,
    })
}
