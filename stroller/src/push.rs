// Thin wrapper around pusher-http-rust to discourage lock-in

use std::collections::HashMap;
use std::thread;

use pusher::Pusher;

use config::*;
use db::models::*;

pub type Error = String;

/*
 * A client for pushing events. Currently clients are one-use-only, due to use of a background
 * thread to avoid blocking, and the non-thread-safety of the underlying Pusher client.
 */
pub struct Client(Pusher);

impl Client {
    pub fn connect() -> Self {
        let pusher = Pusher::new(&pusher_app_id(), &pusher_key(), &pusher_secret())
            .host(&pusher_host())
            /*
             * :( we definitely should be using HTTPS but this panics with
             * "Invalid scheme for Http".
             * On inspection it appears .secure() just doesn't work.
             * pusher-http-rust depends on an old version of hyper; however, TLS
             * support was extracted out of hyper (in an even older version) into
             * hyper-tls, the latest version of which is incompatible with the
             * version of hyper that pusher-http-rust requires (and in particular
             * with pusher-http-rust's .client() builder method).
             */
            //.secure()
            .finalize();
        Client(pusher)
    }

    // Push an event. Consumes this Client (due to use of a background thread)
    pub fn trigger(mut self, event: &StoredEvent) -> Result<(), Error> {
        let mut m = HashMap::new();
        m.insert("message", event.value()?);

        // make actual Pusher call in the background without blocking the caller
        // (since that's the whole point of having this in a separate process)
        thread::spawn(move || {
            // TODO use event.canvas_id (and event.trace_id?) to route event to the right user
            match self.0.trigger("my-channel", "my-event", m) {
                Ok(_) => println!("Pushed event"),
                Err(e) => eprintln!("Error pushing event: {:?}", e),
            }
        });

        Ok(())
    }
}
