extern crate clap;
extern crate efmt;
#[macro_use]
extern crate trackable;

use clap::{App, Arg};

fn main() {
    let matches = App::new("efmt")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(Arg::with_name("ERL_FILE").index(1).required(true))
        .get_matches();
    let file = matches.value_of("ERL_FILE").unwrap();

    let module = track_try_unwrap!(efmt::parse_erl_file(file));
    let mut formatter = efmt::Formatter::new(std::io::stdout());
    track_try_unwrap!(formatter.format(&module));
}
