use futures::future;
use hyper::rt::{Future, Stream};
use hyper::{Body, Method, Request, Response, StatusCode};

use push;

type BoxFut<T, E> = Box<Future<Item = T, Error = E> + Send>;

pub fn handle(req: Request<Body>) -> BoxFut<Response<Body>, hyper::Error> {
    let mut response = Response::new(Body::empty());

    let uri = req.uri().clone();
    let path_segments: Vec<&str> = uri.path().split('/').collect();

    match (req.method(), path_segments.as_slice()) {
        (&Method::GET, ["", ""]) => {
            *response.body_mut() = Body::from("Try POSTing to /canvas/:uuid/traces");
        }
        (&Method::POST, ["", "canvas", canvas_uuid, "traces"]) => {
            println!("Got an event for canvas {}", canvas_uuid);
            let handled = handle_push_trace(canvas_uuid.to_string(), req.into_body())
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

fn handle_push_trace(canvas_uuid: String, trace_body: Body) -> BoxFut<(), push::Error> {
    Box::new(
        trace_body
            .map_err(|e| format!("error reading body: {}", e))
            .concat2()
            .and_then(move |trace_bytes| {
                println!(
                    "{}-byte trace for canvas {}",
                    trace_bytes.len(),
                    canvas_uuid
                );

                // TODO reuse push client!
                let client = push::Client::connect();
                client
                    .trigger(&trace_bytes.to_vec())
                    .map_err(|e| format!("failed to push trace: {}", e))
            }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responds_ok() {
        let req = Request::get("/").body(Body::empty()).unwrap();
        let resp = handle(req).wait();

        assert_eq!(resp.unwrap().status(), 200);
    }

    #[test]
    fn responds_404() {
        let req = Request::get("/nonexistent").body(Body::empty()).unwrap();
        let resp = handle(req).wait();

        assert_eq!(resp.unwrap().status(), 404);
    }

    #[test]
    fn receives_post() {
        let req = Request::post("/canvas/8afcbf52-2954-4353-9397-b5f417c08ebb/traces")
            .body(Body::from("{\"foo\":\"bar\"}"))
            .unwrap();
        let resp = handle(req).wait();

        assert_eq!(resp.unwrap().status(), 202);
    }

    #[test]
    fn rejects_invalid_post() {
        let req = Request::post("/canvas/8afcbf52-2954-4353-9397-b5f417c08ebb/traces")
            .body(Body::from("not json"))
            .unwrap();
        let resp = handle(req).wait();

        assert!(!resp.unwrap().status().is_success());
    }
}
