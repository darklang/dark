mod helpers;

use failure::{ensure, Error};
use helpers::*;

#[test]
fn schedules_new_events() -> Result<(), Error> {
    let (conn, mut looper) = setup()?;
    let (canvas_id, account_id) = setup_canvas(&conn)?;

    let current_event_id = insert_event(
        &conn,
        &canvas_id,
        &account_id,
        "worker-1",
        "CURRENT_TIMESTAMP",
    )?;

    let delayed_event_id = insert_event(
        &conn,
        &canvas_id,
        &account_id,
        "worker-name",
        "CURRENT_TIMESTAMP + INTERVAL '1 MINUTE'",
    )?;

    looper.tick()?;

    ensure!(
        "scheduled" == get_event_status(&conn, current_event_id)?,
        "schedules the new, undelayed event (event = {})",
        current_event_id
    );

    ensure!(
        "new" == get_event_status(&conn, delayed_event_id)?,
        "does not schedule the delayed event (event = {})",
        delayed_event_id
    );

    Ok(())
}

#[test]
fn does_not_schedule_paused_events() -> Result<(), Error> {
    let (conn, mut looper) = setup()?;
    let (canvas_id, account_id) = setup_canvas(&conn)?;

    insert_pause_rule(&conn, &canvas_id, "worker-2")?;

    let evt1 = insert_event(
        &conn,
        &canvas_id,
        &account_id,
        "worker-1",
        "CURRENT_TIMESTAMP",
    )?;
    let evt2 = insert_event(
        &conn,
        &canvas_id,
        &account_id,
        "worker-2",
        "CURRENT_TIMESTAMP",
    )?;

    looper.tick()?;

    ensure!(
        "scheduled" == get_event_status(&conn, evt1)?,
        "schedules the un-paused event (event = {})",
        evt1
    );

    ensure!(
        "new" == get_event_status(&conn, evt2)?,
        "does not schedule the paused event (event = {})",
        evt1
    );

    Ok(())
}
