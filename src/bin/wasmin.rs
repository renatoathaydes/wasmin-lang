use std::io::{Read, stdin, stdout, Write};
use std::io::stderr;
use std::process::exit;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::thread;

use structopt::{*};

use wasmin::ast::TopLevelElement;
use wasmin::parse::new_parser;
use wasmin::sink::{DebugSink, Wasm, WasminSink, Wat};
use wasmin::wasm_parse;

fn main() {
    let opts: CliOptions = CliOptions::from_args();
    match opts {
        CliOptions::Build { output_format, file } => {
            match compile(&output_format, file) {
                Ok(_) => {}
                Err(e) => {
                    stderr().lock().write_all(format!("ERROR: {}", e).as_bytes())
                        .expect("cannot write to stderr");
                    exit(-1);
                }
            }
        }
        CliOptions::Run => {
            println!("ERROR: the 'run' sub-command is not supported yet!");
            exit(-1);
        }
        CliOptions::Parse { file, verbose } => {
            match wasm_parse::parse(file, verbose) {
                Ok(_) => {}
                Err(e) => {
                    stderr().lock().write_all(format!("ERROR: {}", e).as_bytes())
                        .expect("cannot write to stderr");
                    exit(-1);
                }
            }
        }
    }
}

fn compile(output_format: &FormatType, file: Option<String>) -> Result<(), String> {
    let (sender, rcvr) = mpsc::channel();

    let file_name = file.clone();
    let parser_handle = thread::spawn(move || {
        let text = read_program(file_name);
        let mut chars = text.chars();
        let mut parser = new_parser(&mut chars, sender);
        parser.parse()
    });

    {
        let mut writer: Box<dyn Write> = Box::new(stdout());

        match output_format {
            FormatType::DEBUG => {
                push_to_sink(DebugSink::default(), file, rcvr, &mut writer)?;
            }
            FormatType::WAT => {
                push_to_sink(Wat::default(), file, rcvr, &mut writer)?;
            }
            FormatType::WASM => {
                push_to_sink(Wasm::default(), file, rcvr, &mut writer)?;
            }
        };
    }

    parser_handle.join().expect("ERROR: parser thread error.");
    Ok(())
}

fn push_to_sink<T>(
    mut sink: impl WasminSink<T>,
    file: Option<String>,
    rcvr: Receiver<TopLevelElement>,
    writer: &mut Box<dyn Write>,
) -> Result<(), String> {
    let mut ctx = {
        let mod_name = file.map(|n|
            n[0..n.rfind(".").unwrap_or(n.len())].to_owned()
        ).unwrap_or("std_module".to_owned());

        sink.start(mod_name, writer).map_err(|e| e.to_string())?
    };

    for expr in rcvr {
        sink.receive(expr, writer, &mut ctx)
            .map_err(|e| e.to_string())?
    }

    sink.flush(writer, ctx).map_err(|e| e.to_string())
}

fn read_program(file: Option<String>) -> String {
    match file {
        Some(f) => {
            std::fs::read_to_string(&f)
                .expect(&format!("could not read program from file {}", f))
        }
        None => {
            let mut text = String::with_capacity(512);
            stdin().lock().read_to_string(&mut text)
                .expect("could not read program from stdin");
            text
        }
    }
}

#[derive(StructOpt, Debug)]
enum FormatType {
    DEBUG,
    WAT,
    WASM,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "wasmin", about = "Wasmin Compiler.")]
enum CliOptions {
    /// Build a Wasmin module.
    Build {
        #[structopt(short = "f", long = "format", default_value = "wat",
        help = "output format (wat, wasm, debug)")]
        output_format: FormatType,
        /// Wasmin module file. If not given, the module is read from stdin.
        file: Option<String>,
    },
    /// Run a Wasmin module using an interpreter.
    Run,
    /// Parse a WASM file.
    Parse {
        /// WASM file.
        file: String,
        #[structopt(short = "v", long = "verbose", help = "verbose output")]
        verbose: bool,
    },
}

impl FromStr for FormatType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "debug" => Ok(FormatType::DEBUG),
            "wat" => Ok(FormatType::WAT),
            "wasm" => Ok(FormatType::WASM),
            _ => Err("Cannot parse output option".to_owned()),
        }
    }
}
