use std::io::{Error, ErrorKind, stdout, Write};
use std::process::exit;
use std::str::FromStr;
use std::sync::mpsc;
use std::thread;

use structopt::{*};

use wasmin::parse::new_parser;
use wasmin::sink::{DebugSink, Wasm, WasminSink, Wat};

// test program
const PROGRAM: &str = "let constant-ten = 10;

def add-10 [i32] i32;
fun add-10 n = add n constant-ten;

def add-10 [i64] i64;
fun add-10 n = add n constant-ten;

pub let one, two = 1, 2;

def three i64;
let three, four = (
    let t = 3;
    let f = 4;
    t, add-10 f
)";

fn main() -> std::io::Result<()> {
    let opts: CliOptions = CliOptions::from_args();

    let (sender, rcvr) = mpsc::channel();

    let parser_handle = thread::spawn(move || {
        let mut chars = PROGRAM.chars();
        let mut parser = new_parser(&mut chars, sender);
        parser.parse()
    });

    {
        let sink: Box<dyn WasminSink> = match opts.output {
            OutputType::DEBUG => Box::new(DebugSink {}),
            OutputType::WAT => Box::new(Wat {}),
            OutputType::WASM => Box::new(Wasm {}),
        };

        for expr in rcvr {
            match sink.receive(expr) {
                Ok(bytes) => {
                    let stdout = stdout();
                    let mut h = stdout.lock();
                    h.write_all(&bytes)?;
                }
                Err(code) => {
                    println!("ERROR: An error has occurred, aborting!");
                    exit(code)
                }
            }
        }
    }

    parser_handle.join()
        .map(|_| ())
        .map_err(|e| Error::new(ErrorKind::Other, format!("{:?}", e)))
}

#[derive(StructOpt, Debug)]
enum OutputType {
    DEBUG,
    WAT,
    WASM,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "wasmin", about = "Wasmin Compiler.")]
struct CliOptions {
    #[structopt(short = "f", long = "format", default_value = "wat",
    help = "output format (wat, wasm, debug)")]
    output: OutputType,
}

impl FromStr for OutputType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "debug" => Ok(OutputType::DEBUG),
            "wat" => Ok(OutputType::WAT),
            "wasm" => Ok(OutputType::WASM),
            _ => Err("Cannot parse output option".to_owned()),
        }
    }
}
