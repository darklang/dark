#![allow(proc_macro_derive_resolution_fallback)]

use chrono::{DateTime, Utc};
use uuid::Uuid;

#[derive(Queryable, Debug)]
pub struct StoredEvent {
    pub canvas_id: Uuid,
    pub trace_id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub module: String,
    pub path: String,
    pub value: String,
}
