use crate::config;
use analytics::client::Client;
use analytics::http::HttpClient;
use analytics::message::{Message, Track, User};
use slog_scope::{error, info};

use std::sync::mpsc::Receiver;

// TODO: in dev mode, can we write to stdout or sth?
pub fn send(m: &Message) {
    let client = HttpClient::default();
    client
        .send(&config::segment_write_key(), m)
        .expect("Could not send to segment.")
}

// TODO batcher! https://github.com/segmentio/analytics-rust/blob/master/src/batcher.rs

pub enum SegmentMessage {
    Message(Box<analytics::message::Message>),
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
) -> Option<SegmentMessage> {
    let event = event.to_owned();
    let user = User::UserId {
        user_id: format!("user-{}", user_id),
    };
    let msg = match msg_type.as_str() {
        "track" => Some(analytics::message::Message::Track(Track {
            user,
            event,
            properties: serde_json::Value::from(body),
            ..Default::default()
        })),
        _ => {
            error!("Segment message type '{}' is not supported.", msg_type);
            None
        }
    };

    msg.map(|msg| SegmentMessage::Message(Box::new(msg)))
}

pub fn run(channel: Receiver<SegmentMessage>) -> WorkerTerminationReason {
    info!("Segment worker initialized");
    loop {
        match channel.recv() {
            Ok(SegmentMessage::Message(m)) => {
                // TODO: log x-request-id, maybe other metadata?
                info!("analytics msg recv: ok");
                // TODO actually handle message here
                send(&m);
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
