use futures::future;
use hyper::rt::{Future, Stream};
use hyper::{Body, Method, Request, Response, StatusCode};

use push;

type BoxFut<T, E> = Box<Future<Item = T, Error = E> + Send>;

pub trait Push {
    fn push(canvas: &str, event_name: &str, json_bytes: &[u8]) -> Result<(), String>;
}

impl Push for push::Client {
    fn push(canvas: &str, event_name: &str, json_bytes: &[u8]) -> Result<(), String> {
        // TODO reuse push client!
        let client = push::Client::connect();
        client.trigger(canvas.to_string(), event_name.to_string(), json_bytes)
    }
}

pub fn handle<PC>(req: Request<Body>) -> BoxFut<Response<Body>, hyper::Error>
where
    PC: Push,
{
    let mut response = Response::new(Body::empty());

    println!("{:?}", req);

    let uri = req.uri().clone();
    let path_segments: Vec<&str> = uri.path().split('/').collect();

    match (req.method(), path_segments.as_slice()) {
        (&Method::GET, ["", ""]) => {
            *response.body_mut() = Body::from("Try POSTing to /canvas/:name/events/:event");
        }
        (&Method::POST, ["", "canvas", canvas, "events", event]) => {
            let handled = handle_push::<PC>(canvas.to_string(), event.to_string(), req.into_body())
                .map(|_| {
                    *response.status_mut() = StatusCode::ACCEPTED;
                    response
                })
                .or_else(|e| {
                    eprintln!("error trying to push trace: {}", e);
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

fn handle_push<PC>(canvas: String, event_name: String, payload_body: Body) -> BoxFut<(), String>
where
    PC: Push,
{
    Box::new(
        payload_body
            .map_err(|e| format!("error reading body: {}", e))
            .concat2()
            .and_then(move |payload_bytes| {
                println!(
                    "Got event \"{}\" for canvas \"{}\" ({} bytes)",
                    event_name,
                    canvas,
                    payload_bytes.len(),
                );

                PC::push(&canvas, &event_name, &payload_bytes)
                    .map_err(|e| format!("failed to push event {}: {}", event_name, e))
            }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    struct FakePushClient;
    impl Push for FakePushClient {
        fn push(_canvas: &str, _event_name: &str, _json_bytes: &[u8]) -> Result<(), String> {
            Ok(())
        }
    }

    #[test]
    fn responds_ok() {
        let req = Request::get("/").body(Body::empty()).unwrap();
        let resp = handle::<FakePushClient>(req).wait();

        assert_eq!(resp.unwrap().status(), 200);
    }

    #[test]
    fn responds_404() {
        let req = Request::get("/nonexistent").body(Body::empty()).unwrap();
        let resp = handle::<FakePushClient>(req).wait();

        assert_eq!(resp.unwrap().status(), 404);
    }

    #[test]
    fn receives_post() {
        let req = Request::post("/canvas/8afcbf52-2954-4353-9397-b5f417c08ebb/events/traces")
            .body(Body::from("{\"foo\":\"bar\"}"))
            .unwrap();
        let resp = handle::<FakePushClient>(req).wait();

        assert_eq!(resp.unwrap().status(), 202);
    }
}
