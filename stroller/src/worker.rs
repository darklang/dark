use std::sync::mpsc::Receiver;

use crate::config;
use crate::push::PusherClient;

use slog::o;
use slog_scope::{error, info};

pub enum Message {
    CanvasEvent(String, String, Vec<u8>, String),
    Die,
}

pub enum WorkerTerminationReason {
    ViaDie,
    SendersDropped,
}

pub fn run(channel: Receiver<Message>) -> WorkerTerminationReason {
    let mut client = PusherClient::new(
        &config::pusher_cluster(),
        &config::pusher_app_id(),
        &config::pusher_key(),
        &config::pusher_secret(),
    );
    info!("Worker initialized");
    loop {
        match channel.recv() {
            Ok(Message::CanvasEvent(canvas_uuid, event_name, body, request_id)) => {
                info!("msg recv: ok"; o!("canvas" => &canvas_uuid,
                "event" => &event_name,
                "body" => String::from_utf8_lossy(&body).to_string(),
                "x-request-id" => &request_id
                ));
                let result =
                    client.push_canvas_event(&canvas_uuid, &event_name, &body, &request_id);
                if let Err(e) = result {
                    error!("Error pushing to pusher: {}", e; o!("canvas" => &canvas_uuid,
                    "event" => &event_name,
                    "x-request-id" => &request_id
                    ));
                }
            }
            Ok(Message::Die) => {
                info!("Received `Die` in worker thread");
                break WorkerTerminationReason::ViaDie;
            }
            Err(_) => {
                error!("All senders dropped and queue didn't receive `Die`!");
                break WorkerTerminationReason::SendersDropped;
            }
        }
    }
}
