use chrono::{DateTime, Utc};
use slog::o;
use slog_scope::{debug, error, info};

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use std::sync::mpsc::Receiver;
use std::time::Duration;

use failure::Error;

use crate::config::heapio_id;

fn msg_to_json_string(m: &Message) -> String {
    serde_json::to_value(m)
        .unwrap_or_else(|_| {
            serde_json::from_str(
                "{\"message\": \"could not deserialize Heapio message for logging\"}",
            )
            .unwrap()
        })
        .to_string()
}

fn default_http_client() -> reqwest::Client {
    reqwest::Client::builder()
        .connect_timeout(Some(Duration::new(10, 0)))
        .build()
        .unwrap()
}

pub fn send(msg: &Message, client: &reqwest::Client) -> Result<(), Error> {
    debug!("about to send to heapio " ; "msg" => msg_to_json_string(msg) );
    let path = match msg {
        Message::Identify(_) => "/v1/identify",
        Message::Track(_) => "/v1/track",
    };

    client
        .post(&format!("{}{}", "https://api.heap.io.todo", path))
        .basic_auth("", Some(heapio_id()))
        .json(msg)
        .send()?
        .error_for_status()?;

    info!("Successfully sent to heapio.");
    Ok(())
}

// TODO batcher! https://github.com/segmentio/analytics-rust/blob/master/src/batcher.rs

// Copied from https://github.com/segmentio/analytics-rust/blob/master/src/message.rs
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Identify(Identify),
    Track(Track),
    // Batch(Batch),
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum User {
    /// The user is identified only by a user ID.
    UserId {
        #[serde(rename = "userId")]
        user_id: String,
    },

    /// The user is identified only by an anonymous ID.
    AnonymousId {
        #[serde(rename = "anonymousId")]
        anonymous_id: String,
    },

    /// The user is identified by both a user ID and an anonymous ID.
    Both {
        #[serde(rename = "userId")]
        user_id: String,

        #[serde(rename = "anonymousId")]
        anonymous_id: String,
    },
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize, Default)]
pub struct Identify {
    /// The user associated with this message.
    #[serde(flatten)]
    pub user: User,

    /// The traits to assign to the user.
    pub traits: Value,

    /// The timestamp associated with this message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<DateTime<Utc>>,

    /// Context associated with this message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<Value>,

    /// Integrations to route this message to.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrations: Option<Value>,

    /// Extra fields to put at the top level of this message.
    #[serde(flatten)]
    pub extra: Map<String, Value>,
}

impl Default for User {
    fn default() -> Self {
        User::AnonymousId {
            anonymous_id: "".to_owned(),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize, Default)]
pub struct Track {
    /// The user associated with this message.
    #[serde(flatten)]
    pub user: User,

    /// The name of the event being tracked.
    pub event: String,

    /// The properties associated with the event.
    pub properties: Value,

    /// The timestamp associated with this message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<DateTime<Utc>>,

    /// Context associated with this message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<Value>,

    /// Integrations to route this message to.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrations: Option<Value>,

    /// Extra fields to put at the top level of this message.
    #[serde(flatten)]
    pub extra: Map<String, Value>,
}

pub enum HeapioMessage {
    Message(Box<Message>, String, String, String),
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
) -> Option<HeapioMessage> {
    let user = User::UserId {
        user_id: format!("user-{}", user_id),
    };

    String::from_utf8(body)
        .map_err(|_| error!("Heapio body was not a valid utf8 string"))
        .and_then(|s: String| {
            serde_json::from_str(&s)
                .map_err(|_| {
                    error!("Heapio body was not valid json");
                })
                .and_then(|value: serde_json::Value| {
                    if value.is_object() {
                        Ok(value)
                    } else {
                        error!("Heapio body was valid json, but not an object.");
                        Err(())
                    }
                })
        })
        .ok()
        .and_then(|mut body| {
            // note: this leaves {"timestamp": null} in the Value; I think that's ok
            let timestamp: Option<DateTime<Utc>> = match body["timestamp"].take() {
                serde_json::Value::String(s) => DateTime::parse_from_rfc3339(&s)
                    .map(|dt| dt.with_timezone(&Utc))
                    .map_err(|e| {
                        error!("Expected rfc3339 datetime"; o!("invalid_datetime" => s));
                        e
                    })
                    .ok(),
                _ => {
                    error!(
                        "no timestamp key in body of /heapio/ event";
                        o!("request_id" => request_id.clone(),
                           "event" => event.clone(),
                           "msg_type" => msg_type.clone())
                    );
                    None
                }
            };
            match msg_type.as_str() {
                "identify" => Some(Message::Identify(Identify {
                    user,
                    timestamp,
                    traits: body,
                    ..Default::default()
                })),
                "track" => Some(Message::Track(Track {
                    user,
                    event: event.clone(),
                    timestamp,
                    properties: body,
                    ..Default::default()
                })),
                _ => {
                    error!("Heapio message type '{}' is not supported.", msg_type);
                    None
                }
            }
        })
        .map(|msg| HeapioMessage::Message(Box::new(msg), msg_type, event, request_id))
}

pub fn run(channel: Receiver<HeapioMessage>) -> WorkerTerminationReason {
    let client = default_http_client();

    info!("Heapio worker initialized");
    loop {
        match channel.recv() {
            Ok(HeapioMessage::Message(m, msg_type, event, request_id)) => {
                info!("analytics msg recv: ok"; o!(
                "msg_type" => msg_type,
                "event" => event,
                "x-request-id" => &request_id
                ));
                match send(&m, &client) {
                    Ok(_) => info!("Successfully sent to heapio."),
                    Err(e) => error!("Could not send to segment: {}", e),
                }
            }
            Ok(HeapioMessage::Die) => {
                info!("Received `Die` in heapio worker thread");
                break WorkerTerminationReason::ViaDie;
            }
            Err(_) => {
                error!("All analytics senders dropped and queue didn't receive `Die`!");
                break WorkerTerminationReason::SendersDropped;
            }
        }
    }
}
