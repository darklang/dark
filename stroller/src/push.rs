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

/*
 * N.B. the test builds_signed_request_per_spec below depends on the derived
 * Serialize implementation outputting keys in the same order as they're
 * declared in the struct, so don't reorder the struct fields :)
 */
#[derive(Serialize)]
struct PusherEvent {
    name: String,
    channels: Vec<String>,
    data: String,
}

type BoxFut<T> = Box<Future<Item = T, Error = PusherError> + Send>;

fn pusher_host(cluster: &str) -> String {
    format!("api-{}.pusher.com", cluster)
}

impl PusherClient {
    const AUTH_VERSION: &'static str = "1.0";

    pub fn new(cluster: &str, app_id: &str, key: &str, secret: &str) -> Self {
        let mut http = HttpConnector::new(4);
        http.enforce_http(false);
        http.set_keepalive(Some(Duration::from_secs(30)));
        let mut https = HttpsConnector::from((http, TlsConnector::new().unwrap()));
        https.https_only(true);
        let http = HClient::builder().build::<_, Body>(https);

        Self {
            host: pusher_host(cluster),
            app_id: app_id.to_string(),
            key: key.to_string(),

            http,
            mac: Hmac::<Sha256>::new_varkey(secret.as_bytes()).unwrap(),
        }
    }

    fn build_push_request(
        &mut self,
        timestamp: SystemTime,
        channel_name: &str,
        event_name: &str,
        json_bytes: &[u8],
    ) -> Result<HRequest<Body>, PusherError> {
        let json_str = str::from_utf8(json_bytes).map_err(|e| {
            err(&format!(
                "malformed payload (should be UTF-8-encoded JSON): {}",
                e
            ))
        })?;

        let pusher_msg = PusherEvent {
            name: event_name.to_string(),
            channels: vec![channel_name.to_string()],
            data: json_str.to_string(),
        };

        println!(
            "json: {}",
            serde_json::to_string_pretty(&pusher_msg).expect("GAH")
        ); // TODO

        let pusher_msg_bytes =
            serde_json::to_vec(&pusher_msg).map_err(|e| err(&format!("uh oh TODO {}", e)))?;

        let checksum = md5::compute(pusher_msg_bytes);

        let request_path = format!("/apps/{}/events", self.app_id);

        let timestamp = timestamp
            .duration_since(SystemTime::UNIX_EPOCH)
            .map_err(|e| err(&format!("SystemTime before UNIX epoch!: {}", e)))?
            .as_secs();

        let query_params = format!(
            // N.B. these params must be sorted alphabetically for the signature to match
            "auth_key={}&auth_timestamp={}&auth_version={}&body_md5={:x}",
            self.key,
            timestamp,
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
            .body(Body::from(
                serde_json::to_string(&pusher_msg).expect("GAH"), /* TODO no */
            ))
            .map_err(|e| err(&format!("{}", e)))
    }

    pub fn push_canvas_event(
        &mut self,
        canvas_uuid: &str,
        event_name: &str,
        json_bytes: &[u8],
    ) -> BoxFut<()> {
        let channel_name = format!("canvas_{}", canvas_uuid);
        let timestamp = SystemTime::now();

        let pusher_request =
            match self.build_push_request(timestamp, &channel_name, &event_name, json_bytes) {
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
                                1000 * req_time.as_secs() + u64::from(req_time.subsec_millis())
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

/*
 * Minimal implementation of r2d2::ManageConnection.
 *
 * Our PusherClient is just a wrapper for a hyper HTTP client, which does its
 * own handling of persistent connections, reconnecting if necessary, etc, so we
 * don't need to implement is_valid or has_broken. (Individual requests may
 * fail, but any PusherClient checked out from the pool should always be
 * usable.)
 */
pub struct PusherClientManager {
    pub cluster: String,
    pub app_id: String,
    pub key: String,
    secret: String,
}

impl PusherClientManager {
    pub fn new(cluster: &str, app_id: &str, key: &str, secret: &str) -> Self {
        Self {
            cluster: cluster.to_string(),
            app_id: app_id.to_string(),
            key: key.to_string(),
            secret: secret.to_string(),
        }
    }
}

impl ManageConnection for PusherClientManager {
    type Connection = PusherClient;
    type Error = PusherError;

    fn connect(&self) -> Result<PusherClient, PusherError> {
        println!("connect"); // TODO
        Ok(PusherClient::new(
            &self.cluster,
            &self.app_id,
            &self.key,
            &self.secret,
        ))
    }

    fn is_valid(&self, _client: &mut PusherClient) -> Result<(), PusherError> {
        println!("is_valid"); // TODO
        Ok(())
    }

    fn has_broken(&self, _client: &mut PusherClient) -> bool {
        println!("has_broken"); // TODO
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_signed_request_per_spec() {
        /*
         * The Pusher HTTP API docs specify how to construct a Pusher request
         * and sign it for authentication.
         *
         * https://pusher.com/docs/rest_api#authentication
         *
         * The signing protocol is fairly fiddly and the implementation is easy
         * to break, so this test follows the "worked example" from the docs,
         * and verifies we get exactly the auth_signature they do.
         *
         * Note that there are a couple of subtle ordering guarantees we depend
         * on here:
         *
         *  - the URI querystring we generate must alphabetically order its
         *    params, with the exception of the auth_signature param which comes
         *    last. This is actually specified in the docs, so that the HMAC we
         *    compute matches the one they compute on their server.
         *
         *  - the keys in the JSON request body we generate must be ordered
         *    specifically "name", "channels", "data". This is not specified in
         *    the docs, and is not required for correctness (i.e. their server
         *    will accept the keys in any order), but _is_ required for this
         *    test to pass (i.e. to obtain the exact same auth_signature as the
         *    worked example).
         *    If some change to serde breaks this in the future, it's not the
         *    end of the world (and shouldn't break the actual sending to
         *    Pusher), we'll just need to find another way to test this.
         */
        let mut client = PusherClient::new(
            "testcluster",
            "3",
            "278d425bdf160c739803",
            "7ad3773142a6692b25b8",
        );
        let timestamp = SystemTime::UNIX_EPOCH + Duration::from_secs(1_353_088_179);
        let channel = "project-3";
        let event_name = "foo";
        let json_data = b"{\"some\":\"data\"}";

        let req = client
            .build_push_request(timestamp, channel, event_name, json_data)
            .expect("yay"); // TODO
        let uri = req.uri();

        assert_eq!(uri.scheme_part().map(|s| s.as_str()), Some("https"));
        assert_eq!(uri.host(), Some("api-testcluster.pusher.com"));
        assert_eq!(uri.path(), "/apps/3/events");
        assert_eq!(uri.query(), Some("auth_key=278d425bdf160c739803&auth_timestamp=1353088179&auth_version=1.0&body_md5=ec365a775a4cd0599faeb73354201b6f&auth_signature=da454824c97ba181a32ccc17a72625ba02771f50b50e1e7430e47a1f3f457e6c"));
    }
}
