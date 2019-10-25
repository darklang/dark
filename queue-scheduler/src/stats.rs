#[derive(Default)]
pub struct EventStats {
    pub new: i64,
    pub scheduled: i64,
}

// fetch queries the database for event stats, populating &self with the current values.
// Returns a error if the query cannot be made for some reason.
pub fn fetch(conn: &postgres::Connection) -> Result<EventStats, postgres::Error> {
    // ensure struct is cleared out before making query, in case we don't have any events in
    // some of the statuses and postgres consequently doesn't return rows for those statuses.
    let mut stats = EventStats::default();

    // === WARNING ===
    // There are millions of rows in the events table.
    // Please run an EXPLAIN ANALYZE before changing this query.
    let rows = conn.query(
        "SELECT status::text, COUNT(id) FROM events \
         WHERE status IN ('new', 'scheduled') \
         GROUP BY status",
        &[],
    )?;

    for row in rows.iter() {
        let status: String = row.get(0);
        match status.as_str() {
            "new" => {
                stats.new = row.get(1);
            }
            "scheduled" => {
                stats.scheduled = row.get(1);
            }
            _ => {}
        }
    }

    Ok(stats)
}
