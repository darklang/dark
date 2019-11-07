use std::sync::Arc;

use failure::Error;
use slog::o;
use uuid::Uuid;

use scheduler::config as sconfig;
use scheduler::pg;
use scheduler::Looper;

pub fn setup() -> Result<(postgres::Connection, Looper), Error> {
    let log = Arc::new(slog::Logger::root(slog::Discard {}, o!()));
    let lconn = setup_database(log.clone())?;
    let looper = Looper::new(lconn, log.clone());
    let conn = setup_database(log.clone())?;
    Ok((conn, looper))
}

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
    name: &str,
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
                    $3, '_', '{{}}', {}, CURRENT_TIMESTAMP
                ) RETURNING id",
                delay
            ),
            &[cid, aid, &name.to_owned()],
        )?
        .get(0)
        .get(0);

    Ok(eid)
}

pub fn insert_pause_rule(
    conn: &postgres::Connection,
    cid: &Uuid,
    name: &str,
) -> Result<i32, Error> {
    let eid: i32 = conn
        .query(
            "INSERT INTO scheduling_rules (rule_type, canvas_id, handler_name, event_space)
             VALUES ('pause', $1, $2, 'WORKER')
             RETURNING id",
            &[cid, &name.to_owned()],
        )?
        .get(0)
        .get(0);

    Ok(eid)
}
