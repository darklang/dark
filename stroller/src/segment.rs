use crate::config;
use analytics::client::Client;
use analytics::http::HttpClient;
use analytics::message::{Message, Track, User};
use serde_json::json;
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
    Message(analytics::message::Message),
    Die,
}

pub enum WorkerTerminationReason {
    ViaDie,
    SendersDropped,
}

// currently just Track, could expand to other types
pub fn new_message(user_id: String, _body: Vec<u8>) -> SegmentMessage {
    // TODO use body
    SegmentMessage::Message(analytics::message::Message::Track(Track {
        user: User::UserId {
            user_id: format!("user-{}", user_id),
        },
        event: "Example event".to_owned(),
        properties: json!({}),
        ..Default::default()
    }))
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
