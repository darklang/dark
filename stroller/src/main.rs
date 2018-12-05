#[macro_use]
extern crate diesel;
extern crate hyper;
extern crate pusher;

use std::collections::HashMap;

use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};
use pusher::Pusher;

mod config;
mod db;

const GREETING: &str = "Hello Dark!";

const PORT: u16 = 3000;

const PUSHER_APP_ID: &str = "661887";
const PUSHER_KEY: &str = "ee6267fa618c71d4d341";
const PUSHER_SECRET: &str = "ce29d2033402c9f037e1";

fn hello_world(_req: Request<Body>) -> Response<Body> {
    Response::new(Body::from(GREETING))
}

fn main() {
    let addr = ([0, 0, 0, 0], PORT).into();

    let _dbconn = db::connect();

    let mut pusher = Pusher::new(PUSHER_APP_ID, PUSHER_KEY, PUSHER_SECRET)
        .host("api-us2.pusher.com")
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

    let mut m = HashMap::new();
    m.insert("message", "hey");

    match pusher.trigger("my-channel", "my-event", m) {
        Ok(events) => println!("Pushed events: {:?}", events),
        Err(err) => eprintln!("Error pushing events: {}", err),
    }
    // TODO make actual Pusher call in the background without blocking the
    // caller (since that's the whole point of having this in a separate
    // process)

    let hello_svc = || service_fn_ok(hello_world);

    let server = Server::bind(&addr)
        .serve(hello_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responds_ok() {
        let req = Request::get("/ignored").body(Body::empty()).unwrap();
        let resp = hello_world(req);

        assert_eq!(resp.status(), 200);
    }
}
