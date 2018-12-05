use diesel::pg::PgConnection;
use diesel::r2d2::{ConnectionManager, Pool};

use config::*;

pub mod models;
pub mod schema;

fn database_url() -> String {
    format!(
        "postgres://{}:{}@{}/{}",
        db_user(),
        db_password(),
        db_host(),
        db_dbname()
    )
}

#[cfg(test)]
pub fn connect() -> PgConnection {
    use diesel::Connection;

    PgConnection::establish(&database_url()).expect("Couldn't connect to Postgres")
}

pub fn new_pool() -> Pool<ConnectionManager<PgConnection>> {
    Pool::builder()
        /*
         * Set the number of connections the pool should maintain.
         * This is the max, but also the min, since we don't specify min_idle.
         * 10 is the default so this is a no-op, here for the sake of
         * explicitness.
         */
        .max_size(10)
        .build(ConnectionManager::new(database_url()))
        .expect("couldn't establish connection pool")
}
