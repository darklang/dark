pub struct EventStats {
    pub new: i64,
    pub scheduled: i64,
}

impl EventStats {
    pub fn new() -> EventStats {
        EventStats {
            new: 0,
            scheduled: 0,
        }
    }

    // fetch queries the database for event stats, populating &self with the current values.
    // Returns a error if the query cannot be made for some reason.
    pub fn fetch(&mut self, conn: &postgres::Connection) -> Result<(), postgres::Error> {
        // ensure struct is cleared out before making query, in case we don't have any events in
        // some of the statuses and postgres consequently doesn't return rows for those statuses.
        self.new = 0;
        self.scheduled = 0;

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
                    self.new = row.get(1);
                }
                "scheduled" => {
                    self.scheduled = row.get(1);
                }
                _ => {}
            }
        }

        Ok(())
    }
}
