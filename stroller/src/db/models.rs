#![allow(proc_macro_derive_resolution_fallback)]

use chrono::{DateTime, Utc};
use rustc_serialize::json;
use uuid::Uuid;

#[derive(Queryable, Debug)]
pub struct StoredEvent {
    pub canvas_id: Uuid,
    pub trace_id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub module: String,
    pub path: String,
    pub value_json: String,
}

impl StoredEvent {
    pub fn value(&self) -> Result<json::Json, String> {
        json::Json::from_str(&self.value_json)
            .map_err(|e| format!("bad JSON in value field: {:?}", e).to_string())
    }
}
