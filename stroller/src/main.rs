extern crate chrono;
#[macro_use]
extern crate diesel;
extern crate hyper;
extern crate pusher;
extern crate rustc_serialize;
extern crate uuid;

mod config;
mod db;

use std::collections::HashMap;

use diesel::pg::PgConnection;
use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use pusher::Pusher;
use uuid::Uuid;

use db::models::*;

const PORT: u16 = 3000;

const PUSHER_APP_ID: &str = "661887";
const PUSHER_KEY: &str = "ee6267fa618c71d4d341";
const PUSHER_SECRET: &str = "ce29d2033402c9f037e1";

fn service(dbconn: &PgConnection, req: Request<Body>) -> Response<Body> {
    let mut response = Response::new(Body::empty());

    let path_segments: Vec<&str> = req.uri().path().split("/").collect();

    match (req.method(), path_segments.as_slice()) {
        (&Method::GET, ["", ""]) => {
            *response.body_mut() = Body::from("Try POSTing to /canvas/:uuid/trace/:uuid/events");
        }
        (&Method::POST, ["", "canvas", canvas_uuid, "trace", trace_uuid, "events"]) => {
            println!(
                "Got an event for canvas {} and trace {}",
                canvas_uuid, trace_uuid
            );
            match handle_push_stored_event(dbconn, canvas_uuid, trace_uuid) {
                Ok(_) => {
                    *response.status_mut() = StatusCode::ACCEPTED;
                }
                Err(e) => {
                    *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                    eprintln!("error trying to push stored event: {}", e);
                }
            }
        }
        _ => {
            *response.status_mut() = StatusCode::NOT_FOUND;
        }
    }

    response
}

fn handle_push_stored_event(
    dbconn: &PgConnection,
    canvas_uuid: &str,
    trace_uuid: &str,
) -> Result<(), String> {
    let canvas_id = Uuid::parse_str(canvas_uuid)
        .map_err(|e| format!("invalid canvas UUID: {}", e).to_string())?;
    let trace_id = Uuid::parse_str(trace_uuid)
        .map_err(|e| format!("invalid trace UUID: {}", e).to_string())?;
    push_stored_event(dbconn, canvas_id, trace_id)
}

fn push_stored_event(dbconn: &PgConnection, canvas_id: Uuid, trace_id: Uuid) -> Result<(), String> {
    let latest_event = get_latest_event(dbconn, canvas_id, trace_id)
        .map_err(|e| format!("error finding event: {:?}", e).to_string())?;
    println!("latest event for canvas and trace: {:?}", latest_event);

    push_event(&latest_event).map(|result| {
        println!("pushed event: {:?}", result);
        ()
    })
}

fn get_latest_event(
    dbconn: &PgConnection,
    for_canvas_id: Uuid,
    for_trace_id: Uuid,
) -> Result<StoredEvent, diesel::result::Error> {
    use diesel::debug_query;
    use diesel::pg::Pg;
    use diesel::prelude::*;

    use db::schema::stored_events_v2::dsl::*;

    let latest_event_query = stored_events_v2
        .filter(canvas_id.eq(&for_canvas_id).and(trace_id.eq(&for_trace_id)))
        .order(timestamp.desc())
        .limit(1);
    println!("{}", debug_query::<Pg, _>(&latest_event_query));

    latest_event_query.first(dbconn)
}

fn push_event(event: &StoredEvent) -> Result<pusher::TriggeredEvents, String> {
    // TODO reuse Pusher connection!
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
    m.insert("message", event.value()?);

    // TODO use event.canvas_id (and event.trace_id?) to route event to the right user
    pusher.trigger("my-channel", "my-event", m)
    // TODO make actual Pusher call in the background without blocking the caller (since that's the
    // whole point of having this in a separate process)
}

fn main() {
    let addr = ([0, 0, 0, 0], PORT).into();

    let dbpool = db::new_pool();

    let make_service = move || {
        let dbpool_ = dbpool.clone();

        service_fn_ok(move |req| {
            let dbconn = dbpool_.get().expect("failed to obtain DB connection");
            service(&dbconn, req)
        })
    };

    let server = Server::bind(&addr)
        .serve(make_service)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responds_ok() {
        let req = Request::get("/").body(Body::empty()).unwrap();
        let resp = service(&db::connect(), req);

        assert_eq!(resp.status(), 200);
    }

    #[test]
    fn responds_404() {
        let req = Request::get("/nonexistent").body(Body::empty()).unwrap();
        let resp = service(&db::connect(), req);

        assert_eq!(resp.status(), 404);
    }
}
