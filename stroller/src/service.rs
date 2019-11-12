// HTTP service - all HTTP endpoints and handler functions are defined here.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::Sender;
use std::sync::Arc;
use std::time::SystemTime;

use futures::future;
use futures::future::Either;
use hyper::header::HeaderValue;
use hyper::rt::{Future, Stream};
use hyper::{Body, Method, Request, Response, StatusCode};
use uuid::Uuid;

use crate::segment::SegmentMessage;
use crate::worker::Message;

use slog::{o, slog_error, slog_info};
use slog_scope::{error, info};

fn handle_result(
    r: Result<(), ()>,
    uuid: String,
    request_id: String,
    event: String,
    mut response: Response<Body>,
) -> Response<Body> {
    match r {
        Ok(()) => {
            *response.status_mut() = StatusCode::ACCEPTED;
            *response.body_mut() = Body::from("OK");
            response
        }
        Err(_) => {
            error!("Tried to send event to worker, but it was dropped!"; o!("canvas" => &uuid, "event" => &event, "x-request-id" => &request_id));
            *response.status_mut() = StatusCode::ACCEPTED;
            *response.body_mut() = Body::empty();
            response
        }
    }
}

pub fn handle(
    shutting_down: &Arc<AtomicBool>,
    sender: Sender<Message>,
    segment_sender: Sender<SegmentMessage>,
    req: Request<Body>,
) -> impl Future<Item = Response<Body>, Error = hyper::Error> {
    let start = SystemTime::now();
    let mut response = Response::new(Body::empty());
    let request_id = Uuid::new_v4().to_string();
    response
        .headers_mut()
        .insert("x-request-id", request_id.parse::<HeaderValue>().unwrap());

    if shutting_down.load(Ordering::Acquire) {
        *response.status_mut() = StatusCode::SERVICE_UNAVAILABLE;
        return Either::A(future::ok(response));
    }

    let uri = req.uri().to_string();
    let method = req.method().to_string();
    let (parts, body) = req.into_parts();
    let path_segments: Vec<&str> = uri.split('/').collect();
    // only used for segment service, but extracting here for borrow-checker reasons
    let msg_type = path_segments.get(2).map(|s| s.to_string());
    let m = parts.method;
    let event_type = uri.split('/').collect::<Vec<&str>>()[1].to_string();
    let req_body = body.fold(Vec::new(), |mut acc, chunk| {
        acc.extend_from_slice(&*chunk);
        // this horrible type annotation is from an aturon suggestion:
        // https://github.com/hyperium/hyper/issues/953#issuecomment-273568843
        future::ok::<_, hyper::Error>(acc)
    });

    match (&m, path_segments.as_slice()) {
        (&Method::GET, ["", ""]) => {
            *response.body_mut() = Body::from("OK");
        }
        (&Method::POST, ["", "pkill"]) => {
            slog_info!(
                slog_scope::logger(),
                "pkill: Entering shutdown mode, no more requests will be processed."
            );

            shutting_down.store(true, Ordering::Release);
            if segment_sender.send(SegmentMessage::Die).is_err() {
                slog_error!(
                    slog_scope::logger(),
                    "Tried to send `Die` to segment worker, but it was dropped!"
                );
            };
            if sender.send(Message::Die).is_err() {
                slog_error!(
                    slog_scope::logger(),
                    "Tried to send `Die` to pusher worker, but it was dropped!"
                );
            };

            *response.status_mut() = StatusCode::ACCEPTED;
            *response.body_mut() = Body::from("OK");
        }
        (&Method::POST, ["", "canvas", uuid, "events", event])
        | (&Method::POST, ["", "segment", "track", uuid, "event", event]) => {
            let uuid = uuid.to_string();
            let event = event.to_string();
            let moved_request_id = request_id.clone();
            let handled = req_body
                .map(move |req_body| {
                    let result = match event_type.as_ref() {
                        "canvas" => {
                            let msg = Message::CanvasEvent(
                                uuid.clone(),
                                event.clone(),
                                req_body,
                                moved_request_id.clone(),
                            );
                            sender.send(msg).map_err(|_| ())
                        }
                        "segment" => {
                            let msg = crate::segment::new_message(msg_type.unwrap(), uuid.to_string(), event.clone(), req_body, moved_request_id.clone());

                            msg.map_or(Ok(()), |msg| segment_sender.send(msg).map_err(|_| ()))
                        }
                        _ => panic!("Unhandled case!"),
                    };

                    handle_result(result, uuid.to_string(), moved_request_id, event, response)
                })
                .or_else(|_| {
                    error!("Couldn't read request body from client!");
                    Ok(Response::builder()
                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                        .body(Body::empty())
                        .unwrap())
                });
            let req_time = start.elapsed().unwrap();
            let ms = 1000 * req_time.as_secs() + u64::from(req_time.subsec_millis());
            info!("handle(...):";
               o!(
            "uri" => uri,
            "method" => method,
            "dur (ms)" => ms,
            "x-request-id" => &request_id
            ));
            return Either::B(handled);
        }
        _ => {
            *response.status_mut() = StatusCode::NOT_FOUND;
        }
    }

    Either::A(future::ok(response))
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::mpsc;
    fn test_channel() -> Sender<Message> {
        let (sender, _) = mpsc::channel();
        sender
    }

    fn not_shutting_down() -> Arc<AtomicBool> {
        Arc::new(AtomicBool::new(false))
    }

    fn get(path: &str) -> Request<Body> {
        Request::get(path).body(Body::empty()).unwrap()
    }

    #[test]
    fn responds_ok() {
        let resp = handle(&not_shutting_down(), test_channel(), get("/")).wait();

        assert_eq!(resp.unwrap().status(), 200);
    }

    #[test]
    fn responds_404() {
        let resp = handle(&not_shutting_down(), test_channel(), get("/nonexistent")).wait();

        assert_eq!(resp.unwrap().status(), 404);
    }

    #[test]
    fn receives_post() {
        let req = Request::post("/canvas/8afcbf52-2954-4353-9397-b5f417c08ebb/events/traces")
            .body(Body::from("{\"foo\":\"bar\"}"))
            .unwrap();
        let resp = handle(&not_shutting_down(), test_channel(), req).wait();

        assert_eq!(resp.unwrap().status(), 202);
    }

    #[test]
    fn stops_accepting_after_pre_stop() {
        let shutting_down = Arc::new(AtomicBool::new(false));

        let req = Request::post("/pkill").body(Body::empty()).unwrap();
        let resp = handle(&shutting_down, test_channel(), req).wait();

        assert_eq!(resp.unwrap().status(), 202);
        assert!(shutting_down.load(Ordering::Acquire));

        let resp = handle(&shutting_down, test_channel(), get("/")).wait();
        assert_eq!(resp.unwrap().status(), 503);
    }
}
