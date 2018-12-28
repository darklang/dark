// Thin wrapper around pusher-http-rust to discourage lock-in

use std::thread;

use pusher::Pusher;
use rustc_serialize::json;

use config::*;

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
    pub fn trigger(
        mut self,
        canvas: String,
        event_name: String,
        json_bytes: &[u8],
    ) -> Result<(), Error> {
        /*
         * This actually parses the JSON and then immediately re-serializes it
         * again...  which is ridiculous, but required because pusher-http-rust
         * requires its payload to be rustc_serialize::Encodable.
         */

        let parsed = json::Json::from_reader(&mut std::io::Cursor::new(json_bytes))
            .map_err(|e| format!("couldn't send invalid JSON ({}): {:?}", e, json_bytes))?;

        // make actual Pusher call in the background without blocking the caller
        // (since that's the whole point of having this in a separate process)
        thread::spawn(move || {
            let channel = format!("canvas_{}", canvas);
            match self.0.trigger(&channel, &event_name, parsed) {
                Ok(_) => println!("Pushed event"),
                Err(e) => eprintln!("Error pushing event: {:?}", e),
            }
        });

        Ok(())
    }
}
