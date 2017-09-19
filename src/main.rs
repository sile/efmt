extern crate clap;
extern crate efmt;
#[macro_use]
extern crate trackable;

use std::fs::File;
use std::io::Read;
use clap::{App, Arg};
use efmt::Error;
use efmt::formatter::ModuleFormatter;

fn main() {
    let matches = App::new("efmt")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(Arg::with_name("ERL_FILE").index(1).required(true))
        .get_matches();
    let file = matches.value_of("ERL_FILE").unwrap();

    let mut code = String::new();
    let mut file = track_try_unwrap!(File::open(file).map_err(Error::from));
    track_try_unwrap!(file.read_to_string(&mut code).map_err(Error::from));

    let mut formatter = track_try_unwrap!(ModuleFormatter::new(&code, std::io::stdout()));
    track_try_unwrap!(formatter.format());
}
