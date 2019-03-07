use std::sync::mpsc::Receiver;

use crate::config;
use crate::push::PusherClient;

pub enum Message {
    CanvasEvent(String, String, Vec<u8>),
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
    slog_info!(slog_scope::logger(), "Worker initialized");
    loop {
        match channel.recv() {
            Ok(Message::CanvasEvent(canvas_uuid, event_name, body)) => {
                /* TODO ismith
                 * removed body from log b/c "body" => body.to_string() got:
                   = note: the method `to_string` exists but the following trait bounds were not satisfied:
                        `std::vec::Vec<u8> : std::string::ToString`
                        `[u8] : std::string::ToString`
                */
                let log = &slog_scope::logger().new(o!("canvas" => canvas_uuid.to_string(),
                                                       "event" => event_name.to_string()));

                slog_info!(log, "msg recv: ok");
                let result = client.push_canvas_event(&canvas_uuid, &event_name, &body);
                if let Err(e) = result {
                    slog_error!(log, "Error pushing to pusher: {}", e);
                }
            }
            Ok(Message::Die) => {
                slog_info!(slog_scope::logger(), "Received `Die` in worker thread");
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
