// HTTP service - all HTTP endpoints and handler functions are defined here.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::Sender;
use std::sync::Arc;

use futures::future;
use hyper::rt::{Future, Stream};
use hyper::{Body, Method, Request, Response, StatusCode};

use crate::worker::Message;

type BoxFut<T, E> = Box<Future<Item = T, Error = E> + Send>;

pub fn handle(
    shutting_down: &Arc<AtomicBool>,
    sender: Sender<Message>,
    req: Request<Body>,
) -> BoxFut<Response<Body>, hyper::Error> {
    let mut response = Response::new(Body::empty());

    println!("{:?}", req);

    if shutting_down.load(Ordering::Acquire) {
        *response.status_mut() = StatusCode::SERVICE_UNAVAILABLE;
        return Box::new(future::ok(response));
    }

    let uri = req.uri().clone();
    let path_segments: Vec<&str> = uri.path().split('/').collect();

    match (req.method(), path_segments.as_slice()) {
        (&Method::GET, ["", ""]) => {
            *response.body_mut() = Body::from("OK");
        }
        (&Method::POST, ["", "pkill"]) => {
            println!("Entering shutdown mode, no more requests will be processed.");

            shutting_down.store(true, Ordering::Release);
            if sender.send(Message::Die).is_err() {
                eprintln!("Tried to send `Die` to worker, but it was dropped!");
            };

            *response.status_mut() = StatusCode::ACCEPTED;
            *response.body_mut() = Body::from("OK");
        }
        (&Method::POST, ["", "canvas", canvas_uuid, "events", event]) => {
            let canvas_uuid = canvas_uuid.to_string();
            let event = event.to_string();
            let handled = req
                .into_body()
                .fold(Vec::new(), |mut acc, chunk| {
                    acc.extend_from_slice(&*chunk);
                    // this horrible type annotation is from an aturon suggestion:
                    // https://github.com/hyperium/hyper/issues/953#issuecomment-273568843
                    future::ok::<_, hyper::Error>(acc)
                })
                .map(move |req_body| {
                    let canvas_event = Message::CanvasEvent(canvas_uuid, event, req_body);
                    match sender.send(canvas_event) {
                        Ok(()) => {
                            *response.status_mut() = StatusCode::ACCEPTED;
                            *response.body_mut() = Body::from("OK");
                            response
                        }
                        Err(_) => {
                            eprintln!("Tried to send CanvasEvent to worker, but it was dropped!");
                            *response.status_mut() = StatusCode::ACCEPTED;
                            *response.body_mut() = Body::empty();
                            response
                        }
                    }
                })
                .or_else(|_| {
                    eprintln!("Couldn't read request body from client!");
                    Ok(Response::builder()
                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                        .body(Body::empty())
                        .unwrap())
                });
            return Box::new(handled);
        }
        _ => {
            *response.status_mut() = StatusCode::NOT_FOUND;
        }
    }

    Box::new(future::ok(response))
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
