use std::io::stderr;
use std::io::{stdin, stdout, BufWriter, Read, Write};
use std::process::exit;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::thread;

use ansi_term::Colour::{Red, Yellow};
use ansi_term::Style;
use structopt::*;

use wasmin::ast::TopLevelElement;
use wasmin::errors::{Error as WError, Result as WResult};
use wasmin::parse::new_parser;
use wasmin::sink::{DebugSink, Wasm, WasminSink, Wat};
use wasmin::wasm_parse;

fn err_style() -> Style {
    Style::new().bold().fg(Red)
}

fn warn_style() -> Style {
    Style::new().bold().fg(Yellow)
}

macro_rules! exit_with_error {
    ($code:literal, $template:literal, $($args:expr),*) => {{
       stderr().lock().write_all(format!("{}", err_style().paint("error: ")).as_bytes())
            .expect("cannot write to stderr");
       stderr().lock().write_all(format!($template, $($args),*).as_bytes())
            .expect("cannot write to stderr");
       stderr().lock().write_all(b"\n")
            .expect("cannot write to stderr");
       exit($code)
    }};
}

fn main() {
    let opts: CliOptions = CliOptions::from_args();
    match opts {
        CliOptions::Build {
            output_format,
            input,
            output,
        } => {
            build(&output_format, input, output);
        }
        CliOptions::Run => {
            exit_with_error!(
                2,
                "the '{}' sub-command is not supported yet!",
                warn_style().paint("run")
            );
        }
        CliOptions::Parse { file, verbose } => match wasm_parse::parse(file, verbose) {
            Ok(_) => {}
            Err(e) => {
                exit_with_error!(1, "{}", e);
            }
        },
    }
}

fn build(output_format: &FormatType, input: Option<String>, output: Option<String>) {
    let (sender, rcvr) = mpsc::channel();

    let text = read_program(&input)
        .unwrap_or_else(|e| exit_with_error!(2, "I/O read [{:?}]: {}", e.kind(), e));
    let parser_text = text.clone();

    let parser_handle = thread::spawn(move || {
        let mut chars = parser_text.chars();
        let mut parser = new_parser(&mut chars, sender);
        parser.parse()
    });

    {
        let mut writer: Box<dyn Write> = if let Some(out) = output {
            let out_file = std::fs::File::create(out)
                .unwrap_or_else(|e| exit_with_error!(2, "I/O write error [{:?}]: {}", e.kind(), e));
            let w = BufWriter::new(out_file);
            Box::new(w)
        } else {
            Box::new(stdout())
        };

        match output_format {
            FormatType::DEBUG => {
                connect_sink_to_receiver(DebugSink::default(), rcvr, &text, input, &mut writer);
            }
            FormatType::WAT => {
                connect_sink_to_receiver(Wat::default(), rcvr, &text, input, &mut writer);
            }
            FormatType::WASM => {
                connect_sink_to_receiver(Wasm::default(), rcvr, &text, input, &mut writer);
            }
        };

        writer
            .flush()
            .unwrap_or_else(|e| exit_with_error!(2, "I/O write error [{:?}]: {}", e.kind(), e));
    }

    parser_handle.join().expect("ERROR: parser thread error.");
}

fn connect_sink_to_receiver<T>(
    sink: impl WasminSink<T>,
    rcvr: Receiver<TopLevelElement>,
    text: &str,
    file: Option<String>,
    writer: &mut Box<dyn Write>,
) {
    sink_to_receiver(sink, rcvr, file, writer).map_err(|e| match e {
        WError::IO(e) => exit_with_error!(3, "{}", e),
        WError::Wasmin(werr) => {
            let relevant_text = werr.relevant_text(text);
            exit_with_error!(1, "{}\n\n{}", werr, relevant_text)
        }
        WError::Validation(e) => exit_with_error!(1, "{}", e),
    });
}

fn sink_to_receiver<T>(
    mut sink: impl WasminSink<T>,
    rcvr: Receiver<TopLevelElement>,
    file: Option<String>,
    writer: &mut Box<dyn Write>,
) -> WResult<()> {
    let mut ctx = {
        let mod_name = file
            .map(|n| n[0..n.rfind('.').unwrap_or_else(|| n.len())].to_owned())
            .unwrap_or_else(|| "std_module".to_owned());

        sink.start(mod_name, writer)?
    };

    for expr in rcvr {
        sink.receive(expr, writer, &mut ctx)?
    }

    sink.flush(writer, ctx)
}

fn read_program(file: &Option<String>) -> std::io::Result<String> {
    match file {
        Some(f) => std::fs::read_to_string(f),
        None => {
            let mut text = String::with_capacity(512);
            stdin().lock().read_to_string(&mut text)?;
            Ok(text)
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
        #[structopt(
        short = "f",
        long = "format",
        default_value = "wat",
        help = "output format (wat, wasm, debug)"
        )]
        output_format: FormatType,
        /// Wasmin source file. If not given, the source is read from stdin.
        input: Option<String>,
        #[structopt(
        short = "o",
        long = "output",
        help = "output file. If not given, the output is sent to stdout."
        )]
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
