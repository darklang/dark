use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use std::sync::mpsc::Receiver;

use config;
use push::PusherClient;
use service;

pub fn run(channel: Receiver<service::CanvasEvent>, shutting_down: &Arc<AtomicBool>) -> () {
    let mut client = PusherClient::new(
        &config::pusher_cluster(),
        &config::pusher_app_id(),
        &config::pusher_key(),
        &config::pusher_secret(),
    );
    println!("Worker intiialized");
    loop {
        match channel.recv() {
            Ok((canvas_uuid, event_name, body)) => {
                println!("{} {} {:?}", canvas_uuid, event_name, body);
                client.push_canvas_event(&canvas_uuid, &event_name, &body);
            }
            Err(_) => {
                println!("All senders dropped!");
                break;
            }
        }
    }
}
