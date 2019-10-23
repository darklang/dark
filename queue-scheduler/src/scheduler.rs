pub fn schedule_events(conn: &postgres::Connection) -> Result<u64, postgres::Error> {
    conn.execute(
        "UPDATE events SET status = 'scheduled' \
         WHERE status = 'new' AND delay_until < CURRENT_TIMESTAMP",
        &[],
    )
}
