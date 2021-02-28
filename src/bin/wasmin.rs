use std::io::{Read, stdin, stdout, Write, BufWriter};
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
use ansi_term::Colour::{Red, Yellow};
use ansi_term::Style;

fn err_style() -> Style { Style::new().bold().fg(Red) }

fn warn_style() -> Style { Style::new().bold().fg(Yellow) }

macro_rules! exit_with_error {
    ($code:literal, $template:literal, $($args:expr)*) => {{
       stderr().lock().write_all(format!("{}", err_style().paint("error: ")).as_bytes())
            .expect("cannot write to stderr");
       stderr().lock().write_all(format!($template, $($args),*).as_bytes())
            .expect("cannot write to stderr");
       stderr().lock().write_all(b"\n")
            .expect("cannot write to stderr");
       exit($code);
    }};
}

fn main() {
    let opts: CliOptions = CliOptions::from_args();
    match opts {
        CliOptions::Build { output_format, input, output } => {
            match build(&output_format, input, output) {
                Ok(_) => {}
                Err(e) => {
                    exit_with_error!(1, "{}", e);
                }
            }
        }
        CliOptions::Run => {
            exit_with_error!(2, "the '{}' sub-command is not supported yet!",
                warn_style().paint("run"));
        }
        CliOptions::Parse { file, verbose } => {
            match wasm_parse::parse(file, verbose) {
                Ok(_) => {}
                Err(e) => {
                    exit_with_error!(1, "{}", e);
                }
            }
        }
    }
}

fn build(output_format: &FormatType, input: Option<String>, output: Option<String>) -> Result<(), String> {
    let (sender, rcvr) = mpsc::channel();

    let input_file = input.clone();
    let parser_handle = thread::spawn(move || {
        let text = read_program(input_file);
        let mut chars = text.chars();
        let mut parser = new_parser(&mut chars, sender);
        parser.parse()
    });

    {
        let mut writer: Box<dyn Write> = if let Some(out) = output {
            let out_file = std::fs::File::create(out)
                .map_err(|e| e.to_string())?;
            let w = BufWriter::new(out_file);
            Box::new(w)
        } else {
            Box::new(stdout())
        };

        match output_format {
            FormatType::DEBUG => {
                push_to_sink(DebugSink::default(), input, rcvr, &mut writer)?;
            }
            FormatType::WAT => {
                push_to_sink(Wat::default(), input, rcvr, &mut writer)?;
            }
            FormatType::WASM => {
                push_to_sink(Wasm::default(), input, rcvr, &mut writer)?;
            }
        };

        writer.flush().map_err(|e| e.to_string())?;
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
            n[0..n.rfind('.').unwrap_or_else(|| n.len())].to_owned()
        ).unwrap_or_else(|| "std_module".to_owned());

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
                .unwrap_or_else(|_| panic!("could not read program from file {}", f))
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
        /// Wasmin source file. If not given, the source is read from stdin.
        input: Option<String>,
        #[structopt(short = "o", long = "output",
        help = "output file. If not given, the output is sent to stdout.")]
        output: Option<String>,
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
