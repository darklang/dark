// Wrapper for the Pusher REST API (https://pusher.com/docs/rest_api). Not
// intended to be comprehensive.

use futures::future;
use hyper::client::HttpConnector;
use hyper::header;
use hyper::rt::{Future, Stream};
use hyper::{Body, Client as HClient, Request as HRequest, StatusCode, Uri};
use hyper_tls::HttpsConnector;

use config::*;

pub type Error = String;

pub struct Client {
    pub host: String,
    pub app_id: String,
    pub key: String,
    pub secret: String,

    http: HClient<HttpsConnector<HttpConnector>>,
}

type BoxFut<T> = Box<Future<Item = T, Error = Error> + Send>;

impl Client {
    pub fn connect() -> Self {
        let mut https = HttpsConnector::new(4).unwrap();
        https.https_only(true);
        let http = HClient::builder().build::<_, Body>(https);

        Client {
            host: pusher_host(),
            app_id: pusher_app_id(),
            key: pusher_key(),
            secret: pusher_secret(),

            http,
        }
    }

    pub fn trigger(
        &self,
        canvas_uuid: String,
        event_name: String,
        json_bytes: &[u8],
    ) -> BoxFut<()> {
        let uri: Uri = format!("https://{}/apps/{}/events", self.host, self.app_id)
            .parse()
            .unwrap();

        // TODO we need to wrap the data with params, e.g. channel, event name
        // TODO auth

        Box::new(
            self.http
                .request(
                    HRequest::post(uri)
                        .header(header::CONTENT_TYPE, "application/json")
                        .body(Body::from(json_bytes.to_vec()))
                        .unwrap(),
                )
                .map_err(|e| format!("Error pushing event: {:?}", e))
                .and_then(|resp| {
                    let result: BoxFut<()> = match resp.status() {
                        StatusCode::OK => {
                            println!("Pushed event");
                            Box::new(future::ok(()))
                        }
                        code => Box::new(
                            resp.into_body()
                                .concat2()
                                .map(move |bytes| {
                                    eprintln!("Failed to push event: {} ({:?})", code, bytes);
                                    ()
                                })
                                .map_err(|e| format!("Error reading push error: {:?}", e)),
                        ),
                    };
                    result
                }),
        )
    }
}
