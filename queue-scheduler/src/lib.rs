pub mod config;
pub mod errors;
pub mod pg;
mod stats;

use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use failure::Error;
use slog::info; // macro

pub struct Looper {
    conn: postgres::Connection,
    log: Arc<slog::Logger>,
}

impl Looper {
    pub fn new(conn: postgres::Connection, log: Arc<slog::Logger>) -> Looper {
        Looper { conn, log }
    }

    pub fn every(&mut self, d: Duration) -> Result<(), Error> {
        loop {
            thread::sleep(d);
            self.tick()?;
        }
    }

    pub fn tick(&mut self) -> Result<(), Error> {
        let t_start = Instant::now();

        let newly_scheduled = schedule_events(&self.conn)?;
        let stats = stats::fetch(&self.conn)?;

        info!(*self.log, "tick" ;
        "duration" => t_start.elapsed().as_secs_f64() * 1000.0,
        "events.newly_scheduled" => newly_scheduled,
        "events.new_count" => stats.new,
        "events.scheduled_count" => stats.scheduled,
        );

        Ok(())
    }
}

fn schedule_events(conn: &postgres::Connection) -> Result<u64, Error> {
    conn.execute(
        "UPDATE events
         SET status = 'scheduled'
         WHERE NOT EXISTS (
             SELECT * FROM scheduling_rules
             WHERE rule_type IN ('block', 'pause')
             AND canvas_id = events.canvas_id
             AND handler_name = events.name
             AND event_space = events.space
         )
         AND status = 'new'
         AND delay_until < CURRENT_TIMESTAMP",
        &[],
    )
    .map_err(|e| Error::from_boxed_compat(Box::new(e)))
}
