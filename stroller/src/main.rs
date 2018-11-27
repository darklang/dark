extern crate hyper;

use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};

const GREETING: &str = "Hello Dark!";

const PORT: u16 = 3000;

fn hello_world(_req: Request<Body>) -> Response<Body> {
    Response::new(Body::from(GREETING))
}

fn main() {
    let addr = ([0, 0, 0, 0], PORT).into();

    let hello_svc = || service_fn_ok(hello_world);

    let server = Server::bind(&addr)
        .serve(hello_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
