// Wrapper for the Pusher REST API (https://pusher.com/docs/rest_api). Not
// intended to be comprehensive.

use std::fmt;
use std::str;
use std::time::{Duration, SystemTime};

use hmac::{Hmac, Mac};
use reqwest::header::CONTENT_TYPE;
use sha2::Sha256;

use reqwest::StatusCode;
use serde::Serialize;

use slog::o;
use slog_scope::info;

#[derive(Debug)]
pub enum PusherError {
    MalformedPayload(String),
    InvalidTimestamp,
    HttpError(String),
    HttpRequestUnsuccessful(hyper::StatusCode, String),
    Other(String),
}
impl fmt::Display for PusherError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Pusher error: ")?;
        match self {
            PusherError::MalformedPayload(msg) => write!(
                f,
                "malformed payload (should be UTF-8-encoded JSON): {}",
                msg
            ),
            PusherError::InvalidTimestamp => write!(f, "invalid timestamp (before epoch)"),
            PusherError::HttpError(msg) => write!(f, "HTTP error: {}", msg),
            PusherError::HttpRequestUnsuccessful(code, response) => {
                write!(f, "HTTP request unsuccessful ({}): {}", code, response)
            }
            PusherError::Other(msg) => msg.fmt(f),
        }
    }
}
impl std::error::Error for PusherError {}
impl From<String> for PusherError {
    fn from(s: String) -> Self {
        PusherError::Other(s)
    }
}

pub struct PusherClient {
    pub host: String,
    pub app_id: String,
    pub key: String,

    http: reqwest::Client,
    mac: Hmac<Sha256>,
}

/*
 * N.B. the test builds_signed_request_per_spec below depends on the derived
 * Serialize implementation outputting keys in the same order as they're
 * declared in the struct, so don't reorder the struct fields :)
 */
#[derive(Serialize)]
struct PusherEvent<'a> {
    name: &'a str,
    channels: Vec<&'a str>,
    data: &'a str,
}

fn pusher_host(cluster: &str) -> String {
    format!("api-{}.pusher.com", cluster)
}

impl PusherClient {
    const AUTH_VERSION: &'static str = "1.0";

    pub fn new(cluster: &str, app_id: &str, key: &str, secret: &str) -> Self {
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .unwrap();

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
    ) -> Result<reqwest::Request, PusherError> {
        let json_str =
            str::from_utf8(json_bytes).map_err(|e| PusherError::MalformedPayload(e.to_string()))?;

        let pusher_msg = PusherEvent {
            name: event_name,
            channels: vec![channel_name],
            data: json_str,
        };

        let pusher_msg_bytes = serde_json::to_vec(&pusher_msg).unwrap();

        let checksum = md5::compute(&pusher_msg_bytes);

        let request_path = format!("/apps/{}/events", self.app_id);

        let timestamp = timestamp
            .duration_since(SystemTime::UNIX_EPOCH)
            .map_err(|_| PusherError::InvalidTimestamp)?
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

        let uri: reqwest::Url = format!(
            "https://{}{}?{}&auth_signature={:x}",
            self.host,
            request_path,
            query_params,
            signature.code()
        )
        .parse()
        .unwrap();

        self.http
            .post(uri)
            .header(CONTENT_TYPE, "application/json")
            .body(pusher_msg_bytes)
            .build()
            .map_err(|e| PusherError::MalformedPayload(format!("{}", e)))
    }

    pub fn push_canvas_event(
        &mut self,
        canvas_uuid: &str,
        event_name: &str,
        json_bytes: &[u8],
        request_id: &str,
    ) -> Result<(), PusherError> {
        let channel_name = format!("canvas_{}", canvas_uuid);
        let timestamp = SystemTime::now();

        let pusher_request =
            self.build_push_request(timestamp, &channel_name, &event_name, json_bytes)?;
        info!("sending pusher request"; o!(
                "x-request-id" => request_id,
                "channel" => channel_name,
                "event" => event_name));
        // TODO ismith:
        // recursion limit reached while expanding the macro `kv`
        // (on o!(...))
        //"json_bytes" => json_bytes.to_string()));

        let start = SystemTime::now();

        self.http
            .execute(pusher_request)
            .map_err(|e| PusherError::HttpError(e.to_string()))
            .and_then(move |mut resp| {
                let req_time = start.elapsed().unwrap();
                match resp.status() {
                    StatusCode::OK => {
                        let ms = 1000 * req_time.as_secs() + u64::from(req_time.subsec_millis());
                        info!(
                                    "Pushed event in {}ms",
                                    ms;
                                    o!("dur_ms" => ms,
                        "x-request-id" => request_id)
                                );
                        Ok(())
                    }
                    // TODO time to failure might be nice to log here
                    code => resp
                        .text()
                        .map_err(|e| format!("Error reading push error: {:?}", e).into())
                        .and_then(move |msg| Err(PusherError::HttpRequestUnsuccessful(code, msg))),
                }
            })
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
            .expect("should successfully build the request");
        let uri = req.url();

        assert_eq!(uri.scheme(), "https");
        assert_eq!(uri.host_str(), Some("api-testcluster.pusher.com"));
        assert_eq!(uri.path(), "/apps/3/events");
        assert_eq!(uri.query(), Some("auth_key=278d425bdf160c739803&auth_timestamp=1353088179&auth_version=1.0&body_md5=ec365a775a4cd0599faeb73354201b6f&auth_signature=da454824c97ba181a32ccc17a72625ba02771f50b50e1e7430e47a1f3f457e6c"));
    }

    #[test]
    fn rejects_invalid_payload() {
        let mut client = PusherClient::new("dummy", "dummy", "dummy", "dummy");
        let invalid_utf8 = [0x80, 0x0a];

        let err = client
            .build_push_request(SystemTime::now(), "dummy", "dummy", &invalid_utf8)
            .expect_err("should reject the payload");
        assert!(err.to_string().contains("UTF-8"));
    }
}
