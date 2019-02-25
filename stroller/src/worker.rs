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
    println!("Worker initialized");
    loop {
        match channel.recv() {
            Ok(Message::CanvasEvent(canvas_uuid, event_name, body)) => {
                println!("{} {} {:?}", canvas_uuid, event_name, body);
                let result = client.push_canvas_event(&canvas_uuid, &event_name, &body);
                if let Err(e) = result {
                    eprintln!("Error pushing to Pusher: {}", e);
                }
            }
            Ok(Message::Die) => {
                println!("Received `Die` in worker thread");
                break WorkerTerminationReason::ViaDie;
            }
            Err(_) => {
                eprintln!("All senders dropped and queue did receive `Die`!");
                break WorkerTerminationReason::SendersDropped;
            }
        }
    }
}
