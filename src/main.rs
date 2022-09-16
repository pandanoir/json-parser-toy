pub mod parser;
pub mod print;
use parser::json_parser;
use print::print_json;

use std::{
    env, fmt,
    fs::File,
    io::{self, Read},
};

use combine::{stream::position, *};

enum Error<E> {
    Io(io::Error),
    Parse(E),
}
impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::Io(ref err) => write!(f, "{}", err),
            Error::Parse(ref err) => write!(f, "{}", err),
        }
    }
}

fn main() {
    let result = match env::args().nth(1) {
        Some(file) => File::open(file).map_err(Error::Io).and_then(main_),
        None => main_(io::stdin()),
    };
    match result {
        Ok(_) => (),
        Err(err) => println!("{}", err),
    }
}

fn main_<R>(mut read: R) -> Result<(), Error<::combine::error::StringStreamError>>
where
    R: Read,
{
    let mut text = String::new();
    read.read_to_string(&mut text).map_err(Error::Io)?;
    let (res, _) = json_parser()
        .parse(position::Stream::new(&*text))
        .map_err(Error::Parse)?;

    Ok(print_json(res.data))
}
