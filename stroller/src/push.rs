// Wrapper for the Pusher REST API (https://pusher.com/docs/rest_api). Not
// intended to be comprehensive.

use std::fmt;
use std::str;
use std::time::{Duration, SystemTime};

use futures::future;
use hmac::{Hmac, Mac};
use hyper::client::HttpConnector;
use hyper::header;
use hyper::rt::{Future, Stream};
use hyper::{Body, Client as HClient, Request as HRequest, StatusCode, Uri};
use hyper_tls::HttpsConnector;
use native_tls::TlsConnector;
use r2d2::ManageConnection;
use sha2::Sha256;

use config::*;

#[derive(Debug)]
pub struct PusherError(String);
impl fmt::Display for PusherError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str("Pusher error: ")?;
        self.0.fmt(f)
    }
}
// TODO yuck, make it an error enum
impl std::error::Error for PusherError {}
fn err(msg: &str) -> PusherError {
    PusherError(msg.into())
}

pub struct PusherClient {
    pub host: String,
    pub app_id: String,
    pub key: String,

    http: HClient<HttpsConnector<HttpConnector>>,
    mac: Hmac<Sha256>,
}

type BoxFut<T> = Box<Future<Item = T, Error = PusherError> + Send>;

impl PusherClient {
    const AUTH_VERSION: &'static str = "1.0";

    pub fn new() -> Self {
        let mut http = HttpConnector::new(4);
        http.enforce_http(false);
        http.set_keepalive(Some(Duration::from_secs(30)));
        let mut https = HttpsConnector::from((http, TlsConnector::new().unwrap()));
        https.https_only(true);
        let http = HClient::builder().build::<_, Body>(https);

        Self {
            host: pusher_host(),
            app_id: pusher_app_id(),
            key: pusher_key(),

            http,
            mac: Hmac::<Sha256>::new_varkey(pusher_secret().as_bytes()).unwrap(),
        }
    }

    fn build_request(
        &mut self,
        canvas_uuid: &str,
        event_name: &str,
        json_bytes: &[u8],
    ) -> Result<HRequest<Body>, PusherError> {
        let json_str = str::from_utf8(json_bytes).map_err(|e| {
            err(&format!(
                "malformed payload (should be UTF-8-encoded JSON): {}",
                e
            ))
        })?;

        let channel_name = format!("canvas_{}", canvas_uuid);

        // TODO use a struct for type safety?
        let pusher_msg = json!({
            "name": event_name,
            "channels": [channel_name],
            "data": json_str
        });

        println!(
            "json: {}",
            serde_json::to_string_pretty(&pusher_msg).expect("GAH")
        ); // TODO

        let pusher_msg_bytes =
            serde_json::to_vec(&pusher_msg).map_err(|e| err(&format!("uh oh TODO {}", e)))?;

        let checksum = md5::compute(pusher_msg_bytes);

        let request_path = format!("/apps/{}/events", self.app_id);

        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map_err(|e| err(&format!("SystemTime before UNIX epoch!: {}", e)))?;

        let query_params = format!(
            // N.B. these params must be sorted alphabetically for the signature to match
            "auth_key={}&auth_timestamp={}&auth_version={}&body_md5={:x}",
            self.key,
            timestamp.as_secs(),
            Self::AUTH_VERSION,
            checksum
        );

        let to_sign = format!("{}\n{}\n{}", "POST", request_path, query_params);
        self.mac.input(to_sign.as_bytes());
        let signature = self.mac.result_reset();

        let uri: Uri = format!(
            "https://{}{}?{}&auth_signature={:x}",
            self.host,
            request_path,
            query_params,
            signature.code()
        )
        .parse()
        .map_err(|e| err(&format!("couldn't build request URI: {}", e)))?;

        HRequest::post(uri)
            .header(header::CONTENT_TYPE, "application/json")
            .body(Body::from(pusher_msg.to_string() /* TODO no */))
            .map_err(|e| err(&format!("{}", e)))
    }

    pub fn trigger(
        &mut self,
        canvas_uuid: String,
        event_name: String,
        json_bytes: &[u8],
    ) -> BoxFut<()> {
        let pusher_request = match self.build_request(&canvas_uuid, &event_name, json_bytes) {
            Ok(req) => req,
            Err(e) => {
                return Box::new(future::err(err(&format!(
                    "couldn't build Pusher request: {}",
                    e
                ))))
            }
        };

        println!("sending request: {:?}", pusher_request);

        let start = SystemTime::now();

        Box::new(
            self.http
                .request(pusher_request)
                .map_err(|e| err(&format!("Error pushing event: {}", e)))
                .and_then(move |resp| {
                    let req_time = start.elapsed().expect("FFS TODO");
                    let result: BoxFut<()> = match resp.status() {
                        StatusCode::OK => {
                            println!(
                                "Pushed event in {}ms",
                                1000 * req_time.as_secs() + (req_time.subsec_millis() as u64)
                            );
                            Box::new(future::ok(()))
                        }
                        code => Box::new(
                            resp.into_body()
                                .concat2()
                                .map(move |bytes| {
                                    eprintln!("Failed to push event: {} ({:?})", code, bytes);
                                    ()
                                })
                                .map_err(|e| err(&format!("Error reading push error: {:?}", e))),
                        ),
                    };
                    result
                }),
        )
    }
}

pub struct PusherClientManager;

impl ManageConnection for PusherClientManager {
    type Connection = PusherClient;
    type Error = PusherError;

    fn connect(&self) -> Result<PusherClient, PusherError> {
        println!("connect"); // TODO
        Ok(PusherClient::new())
    }

    fn is_valid(&self, client: &mut PusherClient) -> Result<(), PusherError> {
        println!("is_valid"); // TODO
        Ok(()) // TODO
    }

    fn has_broken(&self, client: &mut PusherClient) -> bool {
        println!("has_broken"); // TODO
        false
    }
}
