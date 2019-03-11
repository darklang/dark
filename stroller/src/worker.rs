use std::sync::mpsc::Receiver;

use crate::config;
use crate::push::PusherClient;

use slog::{o,slog_info,slog_error};
use slog_scope::{info,error};

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
                /* TODO ismith
                 * removed body from log b/c "body" => body.to_string() got:
                   = note: the method `to_string` exists but the following trait bounds were not satisfied:
                        `std::vec::Vec<u8> : std::string::ToString`
                        `[u8] : std::string::ToString`
                */

                info!("msg recv: ok"; o!("canvas" => canvas_uuid.to_string(),
                "event" => event_name.to_string(),
                "x-request-id" => request_id.to_string()
                ));
                let result =
                    client.push_canvas_event(&canvas_uuid, &event_name, &body, &request_id);
                if let Err(e) = result {
                    error!("Error pushing to pusher: {}", e; o!("canvas" => canvas_uuid.to_string(),
                    "event" => event_name.to_string(),
                    "x-request-id" => request_id
                    ));
                }
            }
            Ok(Message::Die) => {
                info!("Received `Die` in worker thread");
                break WorkerTerminationReason::ViaDie;
            }
            Err(_) => {
                slog_error!(
                    slog_scope::logger(),
                    "All senders dropped and queue didn't receive `Die`!"
                );
                break WorkerTerminationReason::SendersDropped;
            }
        }
    }
}
