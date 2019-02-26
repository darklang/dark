extern crate clap;
extern crate humansize;
extern crate regex;
extern crate reqwest;
extern crate serde;
extern crate walkdir; // could probs replace this with std::fs

#[macro_use]
extern crate failure;

use clap::{App, Arg};
use humansize::{file_size_opts as options, FileSize};
use regex::Regex;
use reqwest::{multipart, StatusCode};
use walkdir::WalkDir;

#[derive(Debug, Fail)]
enum DarkError {
    #[fail(display = "Failure to auth: {}", status_code)]
    AuthError { status_code: u16 },
    #[fail(display = "Failure to build multipart request")]
    NoFilesFound { paths: String },
    #[fail(display = "Upload failure")]
    Upload(#[cause] reqwest::Error),
    #[fail(display = "Missing argument: {}", missing_arg)]
    MissingArgument { missing_arg: String },
    #[fail(display = "Missing filename (can't happen).")]
    MissingFilename {},
    #[fail(display = "Regex error.")]
    Regex {},
    #[fail(display = "No SET-COOKIE header received.")]
    MissingSetCookie {},
    #[fail(display = "Unknown failure")]
    Unknown {},
}

impl From<regex::Error> for DarkError {
    fn from(_err: regex::Error) -> Self {
        DarkError::Unknown {}
    }
}

impl From<reqwest::Error> for DarkError {
    fn from(_err: reqwest::Error) -> Self {
        DarkError::Unknown {}
    }
}

impl From<reqwest::header::ToStrError> for DarkError {
    fn from(_err: reqwest::header::ToStrError) -> Self {
        DarkError::Unknown {}
    }
}

// use of unstable library feature 'try_trait' (see issue #42327)
/*
   impl From<std::option::NoneError> for DarkError {
   fn from(_err: std::option::NoneError) -> Self {
   DarkError::Unknown{}
   }
   }
   */

impl From<std::io::Error> for DarkError {
    fn from(_err: std::io::Error) -> Self {
        DarkError::Unknown {}
    }
}

impl From<std::string::String> for DarkError {
    fn from(_err: std::string::String) -> Self {
        DarkError::Unknown {}
    }
}

impl From<walkdir::Error> for DarkError {
    fn from(_err: walkdir::Error) -> Self {
        DarkError::Unknown {}
    }
}

fn cookie_and_csrf(
    user: String,
    password: String,
    host: &str,
    canvas: &str,
) -> Result<(String, String), DarkError> {
    let requri = format!("{}/a/{}", host, canvas);
    let _authresp = reqwest::Client::new()
        .get(&requri)
        .basic_auth(user, Some(password))
        .send()
        .map_err(|_| DarkError::AuthError { status_code: 0 })
        .and_then(move |mut resp| match resp.status() {
            StatusCode::OK => {
                let cookie: String = resp
                    .headers()
                    .get(reqwest::header::SET_COOKIE)
                    .ok_or(DarkError::MissingSetCookie {})?
                    .to_str()?
                    .to_string();
                let csrf_re: Regex = Regex::new("const csrfToken = \"([^\"]*)\";")?;
                let csrf: String = csrf_re
                    .captures_iter(&resp.text()?)
                    .next()
                    .ok_or(DarkError::Regex {})?[1]
                    .to_string();

                Ok((cookie, csrf))
            }
            _ => Err(DarkError::AuthError {
                status_code: resp.status().as_u16(),
            }),
        });

    // shouldn't be reachable
    Err(DarkError::Unknown {})
}

fn form_body(paths: &str) -> Result<(reqwest::multipart::Form, u64), DarkError> {
    let mut files = paths
        .split(' ')
        .map(WalkDir::new)
        .flat_map(|entry| entry.follow_links(true).into_iter())
        .filter_map(|e| e.ok())
        .filter(|entry| entry.file_type().is_file())
        .peekable();

    // "is_empty()"
    if files.peek().is_none() {
        return Err(DarkError::NoFilesFound {
            paths: paths.to_string(),
        });
    };

    let mut len = 0;

    let mut form = multipart::Form::new();
    for file in files {
        len += file.metadata()?.len();
        form = file
            .path()
            .file_name()
            .ok_or_else(|| DarkError::MissingFilename {})
            .and_then(|e| Ok(e.to_string_lossy().to_string()))
            .and_then(move |f| form.file(f, file.path()).map_err(DarkError::from))?;
    }

    Ok((form, len))
}

fn main() -> Result<(), DarkError> {
    let matches = App::new("dark")
        .version("0.1.0")
        .author("Ian Smith <ismith@darklang.com")
        .about("dark cli")
        .arg(
            Arg::with_name("user")
                .long("user")
                .required(true)
                .takes_value(true)
                .help("Your dark username"),
        ).arg(
            Arg::with_name("password")
                .long("password")
                .required(true)
                .takes_value(true)
                .requires("user")
                .help("Your dark password"),
        ).arg(
            Arg::with_name("canvas")
                .long("canvas")
                .required(true)
                .takes_value(true)
                .help("Your canvas"),
        ).arg(
            Arg::with_name("paths")
                .required(true)
                .takes_value(true)
                .help("files to upload"),
        ).arg(
            Arg::with_name("dry-run")
                .long("dry-run")
                .required(false)
                .takes_value(false)
                .help("Don't upload to canvas"),
        ).arg(
            Arg::with_name("dev")
                .long("dev")
                .required(false)
                .takes_value(false)
                .help("Run against localhost - debug only."),
        ).get_matches();

    // TODO: impl --dry-run

    let paths = matches
        .value_of("paths")
        .ok_or(DarkError::MissingArgument {
            missing_arg: "paths".to_string(),
        })?;
    let canvas = matches
        .value_of("canvas")
        .ok_or(DarkError::MissingArgument {
            missing_arg: "canvas".to_string(),
        })?;
    let user = matches.value_of("user").ok_or(DarkError::MissingArgument {
        missing_arg: "user".to_string(),
    })?;
    let password = matches
        .value_of("password")
        .ok_or(DarkError::MissingArgument {
            missing_arg: "password".to_string(),
        })?;
    let host = if matches.is_present("dev") {
        "http://darklang.localhost:8000"
    } else {
        "https://darklang.com"
    };

    let (cookie, csrf) = cookie_and_csrf(
        user.to_string(),
        password.to_string(),
        &host.to_string(),
        &canvas.to_string(),
    )?;

    let (form, size) = form_body(&paths.to_string())?;

    // TODO: what is that size?
    println!("Going to attempt to upload files totalling {}; note that this may error if your request is over a certain size; ask Dark for help with that.", size.file_size(options::DECIMAL)?);

    let requri = format!("{}/api/{}/static_assets", host, canvas);
    let mut resp = reqwest::Client::new()
        .post(&requri)
        .header("cookie", cookie)
        .header("x-csrf-token", csrf)
        .multipart(form)
        .send()
        .or_else(|error| Err(DarkError::Upload(error)))?;

    println!("{}", resp.text()?);

    Ok(())
}
