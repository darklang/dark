mod helpers;

use slog::o;
use std::sync::Arc;

use helpers::*;
use scheduler::Looper;

#[test]
fn schedules_new_events() {
    let log = Arc::new(slog::Logger::root(slog::Discard {}, o!()));

    let lconn = helpers::setup_database(log.clone()).unwrap();
    let mut looper = Looper::new(lconn, log.clone());

    let conn = helpers::setup_database(log.clone()).unwrap();
    let (canvas_id, account_id) = helpers::setup_canvas(&conn).unwrap();

    let current_event_id =
        insert_event(&conn, &canvas_id, &account_id, "CURRENT_TIMESTAMP").unwrap();

    let delayed_event_id = insert_event(
        &conn,
        &canvas_id,
        &account_id,
        "CURRENT_TIMESTAMP + INTERVAL '1 MINUTE'",
    )
    .unwrap();

    // run one iteration of scheduling
    looper.tick().unwrap();

    assert_eq!(
        "scheduled",
        get_event_status(&conn, current_event_id).unwrap(),
        "schedules the new, undelayed event (event = {})",
        current_event_id
    );

    assert_eq!(
        "new",
        get_event_status(&conn, delayed_event_id).unwrap(),
        "does not schedule the delayed event (event = {})",
        delayed_event_id
    );
}
