use std::sync::Arc;

use failure::Error;
use uuid::Uuid;

use scheduler::config as sconfig;
use scheduler::pg;

pub fn setup_database(log: Arc<slog::Logger>) -> Result<postgres::Connection, Error> {
    let mut cfg = config::Config::new();
    cfg.merge(config::Environment::with_prefix("DARK_CONFIG"))
        .unwrap();

    let dbcfg = sconfig::load_database(&cfg)?;

    pg::connect(log, &dbcfg.url)
}

pub fn setup_canvas(conn: &postgres::Connection) -> Result<(Uuid, Uuid), Error> {
    conn.execute(
        "INSERT INTO accounts (id, username, name, email, admin, password)
        VALUES (
            $1,
            'scheduler',
            'queue-scheduler tests',
            'test+scheduler@darklang.com',
            false,
            ''
        )
        ON CONFLICT DO NOTHING",
        &[&Uuid::new_v4()],
    )?;

    let account_id: Uuid = conn
        .query("SELECT id FROM accounts WHERE username = 'scheduler'", &[])?
        .get(0)
        .get(0);

    let canvas_id: Uuid = conn
        .query(
            "SELECT canvas_id($1, $2, 'scheduler-test')",
            &[&Uuid::new_v4(), &account_id],
        )?
        .get(0)
        .get(0);

    Ok((canvas_id, account_id))
}

pub fn get_event_status(conn: &postgres::Connection, eid: i32) -> Result<String, Error> {
    let status: String = conn
        .query("SELECT status::text FROM events WHERE id = $1", &[&eid])?
        .get(0)
        .get(0);
    Ok(status)
}

pub fn insert_event(
    conn: &postgres::Connection,
    cid: &Uuid,
    aid: &Uuid,
    delay: &str,
) -> Result<i32, Error> {
    let eid: i32 = conn
        .query(
            &format!(
                "INSERT INTO events (
                    status, dequeued_by, canvas_id, account_id, space,
                    name, modifier, value, delay_until, enqueued_at
                 ) VALUES (
                    'new', NULL, $1, $2, 'WORKER',
                    'test-1', '_', '{{}}', {}, CURRENT_TIMESTAMP
                ) RETURNING id",
                delay
            ),
            &[cid, aid],
        )?
        // .map_err(|e| Error::from_boxed_compat(Box::new(e)))?
        .get(0)
        .get(0);

    Ok(eid)
}
