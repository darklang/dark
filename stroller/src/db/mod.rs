use diesel::pg::PgConnection;
use diesel::Connection;

use config::*;

pub mod models;
pub mod schema;

pub fn connect() -> PgConnection {
    let db_url = format!(
        "postgres://{}:{}@{}/{}",
        db_user(),
        db_password(),
        db_host(),
        db_dbname()
    );

    PgConnection::establish(&db_url).expect(&format!("Couldn't connect to Postgres"))
}
