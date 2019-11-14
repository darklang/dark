use crate::config;
use analytics::client::Client;
use analytics::http::HttpClient;
use analytics::message::{Message, Track, User};
use chrono::{DateTime, Utc};
use slog::o;
use slog_scope::{debug, error, info};

use std::sync::mpsc::Receiver;

fn msg_to_json_string(m: &Message) -> String {
    serde_json::to_value(m)
        .unwrap_or_else(|_| {
            serde_json::from_str(
                "{\"message\": \"could not deserialize Segment message for logging\"}",
            )
            .unwrap()
        })
        .to_string()
}

pub fn send(m: &Message, client: &HttpClient) {
    match &config::segment_write_key() {
        Some(segment_write_key) => {
            debug!("about to send to segment" ; "msg" => msg_to_json_string(m) );
            match client.send(segment_write_key, m) {
                Ok(_) => {
                    info!("Successfully sent to segment.");
                }
                Err(e) => error!("Could not send to segment: {}", e),
            }
        }
        None => {
            // It'd be nice if we could do info!("segment msg sent", kv), but I'm not sure how to
            // get slog::SerdeValue to recognize a serde Value ...
            info!("segment message sent"; "msg" => msg_to_json_string(m))
        }
    }
}

// TODO batcher! https://github.com/segmentio/analytics-rust/blob/master/src/batcher.rs

pub enum SegmentMessage {
    Message(Box<analytics::message::Message>, String, String, String),
    Die,
}

pub enum WorkerTerminationReason {
    ViaDie,
    SendersDropped,
}

// currently just Track, could expand to other types
pub fn new_message(
    msg_type: String,
    user_id: String,
    event: String,
    body: Vec<u8>,
    request_id: String,
) -> Option<SegmentMessage> {
    let event = event.to_owned();
    let user = User::UserId {
        user_id: format!("user-{}", user_id),
    };

    String::from_utf8(body)
        .map_err(|_| error!("Segment body was not a valid utf8 string"))
        .and_then(|s: String| {
            serde_json::from_str(&s)
                .map_err(|_| {
                    error!("Segment body was not valid json");
                })
                .and_then(|value: serde_json::Value| {
                    if value.is_object() {
                        Ok(value)
                    } else {
                        // segment's api fails silently if the properties value is not an object
                        error!("Segment body was valid json, but not an object.");
                        Err(())
                    }
                })
        })
        .ok()
        .and_then(|mut properties| match msg_type.as_str() {
            "track" => {
                // note: this leaves {"timestamp": null} in the Value; I think that's ok
                let timestamp: Option<DateTime<Utc>> = match properties["timestamp"].take() {
                    serde_json::Value::String(s) => DateTime::parse_from_rfc3339(&s)
                        .map(|dt| dt.with_timezone(&Utc))
                        .map_err(|e| {
                            error!("Expected rfc3339 datetime"; o!("invalid_datetime" => s));
                            e
                        })
                        .ok(),
                    _ => {
                        info!(
                            "no timestamp key in body of /segment/ event";
                            o!("request_id" => request_id.clone(),
                               "event" => event.clone(),
                               "msg_type" => msg_type.clone())
                        );
                        None
                    }
                };

                Some(analytics::message::Message::Track(Track {
                    user,
                    event: event.clone(),
                    timestamp,
                    properties,
                    ..Default::default()
                }))
            }
            _ => {
                error!("Segment message type '{}' is not supported.", msg_type);
                None
            }
        })
        .map(|msg| SegmentMessage::Message(Box::new(msg), msg_type, event, request_id))
}

pub fn run(channel: Receiver<SegmentMessage>) -> WorkerTerminationReason {
    let client = HttpClient::default();

    info!("Segment worker initialized");
    loop {
        match channel.recv() {
            Ok(SegmentMessage::Message(m, msg_type, event, request_id)) => {
                info!("analytics msg recv: ok"; o!(
                "msg_type" => msg_type,
                "event" => event,
                "x-request-id" => &request_id
                ));
                send(&m, &client);
            }
            Ok(SegmentMessage::Die) => {
                info!("Received `Die` in segment worker thread");
                break WorkerTerminationReason::ViaDie;
            }
            Err(_) => {
                error!("All analytics senders dropped and queue didn't receive `Die`!");
                break WorkerTerminationReason::SendersDropped;
            }
        }
    }
}
