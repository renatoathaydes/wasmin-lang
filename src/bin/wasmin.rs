use std::io::{Read, stdin, stdout, Write};
use std::io::stderr;
use std::process::exit;
use std::str::FromStr;
use std::sync::mpsc;
use std::thread;

use structopt::{*};

use wasmin::parse::new_parser;
use wasmin::sink::{DebugSink, Wasm, WasminSink, Wat};

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
        let mut sink: Box<dyn WasminSink> = match output_format {
            FormatType::DEBUG => Box::new(DebugSink {}),
            FormatType::WAT => Box::<Wat>::default(),
            FormatType::WASM => Box::new(Wasm {}),
        };

        let mut writer: Box<dyn Write> = Box::new(stdout());

        {
            let mod_name = file.map(|n|
                n[0..n.rfind(".").unwrap_or(n.len())].to_owned()
            ).unwrap_or("std_module".to_owned());

            sink.start(mod_name, &mut writer).map_err(|e| e.to_string())?
        }

        for expr in rcvr {
            sink.receive(expr, &mut writer).map_err(|e| e.to_string())?
        }
        sink.flush(&mut writer).map_err(|e| e.to_string())?
    }

    parser_handle.join().expect("ERROR: parser thread error.");
    Ok(())
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
