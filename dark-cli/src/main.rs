extern crate clap;
extern crate regex;
extern crate reqwest;
extern crate walkdir; // could probs replace this with std::fs
extern crate humansize;
use humansize::{FileSize, file_size_opts as options};

use clap::{App, Arg};
use regex::Regex;
use reqwest::{multipart, StatusCode};
use std::process::exit;
use walkdir::WalkDir;

// TODO handle response errors

fn cookie_and_csrf(user: String, password: String, host: &str, canvas: &str) -> (String, String) {
    let requri = format!("{}/a/{}", host, canvas);
    let mut authresp = match reqwest::Client::new()
        .get(&requri)
        .basic_auth(user, Some(password))
        .send()
    {
        Ok(r) => r,
        Err(error) => panic!("Error authing: {:?}", error),
    };

    match authresp.status() {
        StatusCode::OK => (),
        StatusCode::UNAUTHORIZED => {
            println!("Failed to auth: bad username/password");
            exit(1)
        }
        StatusCode::INTERNAL_SERVER_ERROR => {
            println!("Internal server error - maybe a bad canvas?")
        }
        e => {
            println!(
                "Failed to auth: {}",
                e.canonical_reason().expect("Could not parse status.")
            );
            exit(1)
        }
    }

    let cookie: String = authresp
        .headers()
        .get(reqwest::header::SET_COOKIE)
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    let csrf_re: Regex = Regex::new("const csrfToken = \"([^\"]*)\";").unwrap();
    let csrf: String = csrf_re
        .captures_iter(&authresp.text().unwrap())
        .next()
        .unwrap()[1]
        .to_string();

    (cookie, csrf)
}

fn form_body(paths: &str) -> (reqwest::multipart::Form, u64) {
    let mut files = paths
        .split(' ')
        .map(WalkDir::new)
        .flat_map(|entry| entry.follow_links(true).into_iter())
        .filter_map(|e| e.ok())
        .filter(|entry| entry.file_type().is_file())
        .peekable();

    // "is_empty()"
    if files.peek().is_none() {
        println!("No files found in {}!", paths);
        exit(1);
    };

    let mut len = 0;

    let mut form = multipart::Form::new();
    for file in files {
        len += file.metadata().unwrap().len();
        println!("File: {}", file.path().file_name().unwrap().to_string_lossy());
        let filename = file
            .path()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();
        form = form.file(filename, file.path()).unwrap();
    }

    (form, len)
}

fn main() {
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
    // TODO: can we allow --dev only in debug build?

    let paths = matches.value_of("paths").unwrap();
    let canvas = matches.value_of("canvas").unwrap();
    let user = matches.value_of("user").unwrap();
    let password = matches.value_of("password").unwrap();
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
    );

    let (form, size) = form_body(&paths.to_string());

    // TODO: what is that size?
    println!("Going to attempt to upload files totalling {}; note that this may error if your request is over a certain size; ask Dark for help with that.", size.file_size(options::DECIMAL).unwrap());

    let requri = format!("{}/api/{}/static_assets", host, canvas);
    let mut resp = match reqwest::Client::new()
        .post(&requri)
        .header("cookie", cookie)
        .header("x-csrf-token", csrf)
        .multipart(form)
        .send()
    {
        Ok(r) => r,
        Err(error) => panic!("Error uploading assets: {:?}", error),
    };
    let _ = resp;

    println!("{}", resp.text().unwrap());
}
