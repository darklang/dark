// HTTP service - all HTTP endpoints and handler functions are defined here.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use futures::future;
use hyper::rt::{spawn, Future, Stream};
use hyper::{Body, Method, Request, Response, StatusCode};

use push;

type BoxFut<T, E> = Box<Future<Item = T, Error = E> + Send>;

// Wrapper to generify ::handle so we can mock out PusherClient for testing
pub trait AsyncPush: Send + Sync {
    fn push(&mut self, canvas_uuid: &str, event_name: &str, json_bytes: &[u8]);
}

impl AsyncPush for r2d2::PooledConnection<push::PusherClientManager> {
    fn push(&mut self, canvas_uuid: &str, event_name: &str, json_bytes: &[u8]) {
        let event_name_ = event_name.to_string();
        spawn(
            self.push_canvas_event(canvas_uuid, event_name, json_bytes)
                .map_err(move |e| {
                    eprintln!("failed to push event {}: {}", event_name_, e);
                    ()
                }),
        );
    }
}

pub fn handle<PCM>(
    shutting_down: &Arc<AtomicBool>,
    pool: &r2d2::Pool<PCM>,
    req: Request<Body>,
) -> BoxFut<Response<Body>, hyper::Error>
where
    PCM: r2d2::ManageConnection,
    r2d2::PooledConnection<PCM>: AsyncPush,
{
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

            *response.status_mut() = StatusCode::ACCEPTED;
            *response.body_mut() = Body::from("OK");
        }
        (&Method::POST, ["", "canvas", canvas_uuid, "events", event]) => {
            let client = pool.get();
            match client {
                Ok(c) => {
                    let handled = handle_push(
                        c,
                        canvas_uuid.to_string(),
                        event.to_string(),
                        req.into_body(),
                    ).map(|_| {
                        *response.status_mut() = StatusCode::ACCEPTED;
                        response
                    }).or_else(|e| {
                        eprintln!("error trying to push trace: {}", e);
                        Ok(Response::builder()
                            .status(StatusCode::INTERNAL_SERVER_ERROR)
                            .body(Body::empty())
                            .unwrap())
                    });
                    return Box::new(handled);
                }
                Err(err) => {
                    eprintln!("Could not pull pusher client from pool, reason: {}", err);
                    *response.status_mut() = StatusCode::SERVICE_UNAVAILABLE;
                    return Box::new(future::ok(response));
                }
            }
        }
        _ => {
            *response.status_mut() = StatusCode::NOT_FOUND;
        }
    }

    Box::new(future::ok(response))
}

fn handle_push<PC>(
    mut client: PC,
    canvas_uuid: String,
    event_name: String,
    payload_body: Body,
) -> BoxFut<(), String>
where
    PC: AsyncPush,
    PC: 'static,
{
    Box::new(
        payload_body
            .map_err(|e| format!("error reading body: {}", e))
            .concat2()
            .map(move |payload_bytes| {
                println!(
                    "Got event \"{}\" for canvas \"{}\" ({} bytes)",
                    event_name,
                    canvas_uuid,
                    payload_bytes.len(),
                );

                client.push(&canvas_uuid, &event_name, &payload_bytes);
            }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    struct FakePushClient;

    struct FakeClientManager;
    impl r2d2::ManageConnection for FakeClientManager {
        type Connection = FakePushClient;
        type Error = push::PusherError;

        fn connect(&self) -> Result<FakePushClient, push::PusherError> {
            Ok(FakePushClient)
        }

        fn is_valid(&self, _client: &mut FakePushClient) -> Result<(), push::PusherError> {
            Ok(())
        }

        fn has_broken(&self, _client: &mut FakePushClient) -> bool {
            false
        }
    }
    impl AsyncPush for r2d2::PooledConnection<FakeClientManager> {
        fn push(&mut self, _canvas_uuid: &str, _event_name: &str, _json_bytes: &[u8]) {}
    }

    const MANAGER: FakeClientManager = FakeClientManager;

    fn not_shutting_down() -> Arc<AtomicBool> {
        Arc::new(AtomicBool::new(false))
    }

    fn test_pool() -> r2d2::Pool<FakeClientManager> {
        r2d2::Pool::builder()
            .max_size(1)
            .min_idle(Some(0))
            .build(MANAGER)
            .unwrap()
    }

    fn get(path: &str) -> Request<Body> {
        Request::get(path).body(Body::empty()).unwrap()
    }

    #[test]
    fn responds_ok() {
        let resp = handle(&not_shutting_down(), &test_pool(), get("/")).wait();

        assert_eq!(resp.unwrap().status(), 200);
    }

    #[test]
    fn responds_404() {
        let resp = handle(&not_shutting_down(), &test_pool(), get("/nonexistent")).wait();

        assert_eq!(resp.unwrap().status(), 404);
    }

    #[test]
    fn receives_post() {
        let req = Request::post("/canvas/8afcbf52-2954-4353-9397-b5f417c08ebb/events/traces")
            .body(Body::from("{\"foo\":\"bar\"}"))
            .unwrap();
        let resp = handle(&not_shutting_down(), &test_pool(), req).wait();

        assert_eq!(resp.unwrap().status(), 202);
    }

    #[test]
    fn stops_accepting_after_pre_stop() {
        let shutting_down = Arc::new(AtomicBool::new(false));

        let req = Request::post("/pkill").body(Body::empty()).unwrap();
        let resp = handle(&shutting_down, &test_pool(), req).wait();

        assert_eq!(resp.unwrap().status(), 202);
        assert!(shutting_down.load(Ordering::Acquire));

        let resp = handle(&shutting_down, &test_pool(), get("/")).wait();
        assert_eq!(resp.unwrap().status(), 503);
    }
}
