extern crate chrono;
#[macro_use]
extern crate diesel;
extern crate hyper;
extern crate pusher;
extern crate uuid;

mod config;
mod db;

use std::collections::HashMap;

use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};
use pusher::Pusher;


const GREETING: &str = "Hello Dark!";

const PORT: u16 = 3000;

const PUSHER_APP_ID: &str = "661887";
const PUSHER_KEY: &str = "ee6267fa618c71d4d341";
const PUSHER_SECRET: &str = "ce29d2033402c9f037e1";

fn hello_world(_req: Request<Body>) -> Response<Body> {
    Response::new(Body::from(GREETING))
}

fn main() {
    use diesel::debug_query;
    use diesel::pg::Pg;
    use diesel::prelude::*;

    use db::models::*;
    use db::schema::stored_events_v2::dsl::*;

    let addr = ([0, 0, 0, 0], PORT).into();

    let dbconn = db::connect();

    let test_query = stored_events_v2.order(path);

    println!("{}", debug_query::<Pg, _>(&test_query));

    let latest_event = test_query
        .order(timestamp.desc())
        .limit(1)
        .first::<StoredEvent>(&dbconn)
        .expect("Error loading event");
    println!("{:?}", latest_event);

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
    m.insert("message", latest_event.value);

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
