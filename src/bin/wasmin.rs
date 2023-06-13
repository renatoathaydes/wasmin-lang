use std::io::{BufWriter, Read, stdin, stdout, Write};
use std::io::stderr;
use std::process::exit;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::thread;

use ansi_term::Colour::{Red, Yellow};
use ansi_term::Style;
use structopt::*;

use wasmin::ast::TopLevelElement;
use wasmin::cli::options::{*};
use wasmin::cli::runner::run_parser;
use wasmin::errors::Error as WError;
use wasmin::out::wasm::WasmContext;

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
        CliOptions::Parse { file, verbose } => {
            exit_with_error!(
                2,
                "the '{}' sub-command is not supported yet!",
                warn_style().paint("parse")
            );
        },
    }
}

fn build(output_format: &FormatType, input: String, output: Option<String>) {
    let (sender, rcvr) = mpsc::channel();

    let parser_handle = thread::spawn(move || {
        run_parser(input, sender)
    });

    {
        let mut writer: Box<dyn Write> = if let Some(out) = output {
            let out_file = std::fs::File::create(out)
                .unwrap_or_else(|e| exit_with_error!(2, "output file write error: {}", e));
            let w = BufWriter::new(out_file);
            Box::new(w)
        } else {
            Box::new(stdout())
        };

        let mut wasm = WasmContext::new();
        wasm.write(&mut writer, rcvr)
            .unwrap_or_else(|e| exit_with_error!(2, "WASM writer error: {}", e));

        // match output_format {
        //     FormatType::DEBUG => {
        //         connect_sink_to_receiver(DebugSink::default(), rcvr, &text, input, &mut writer);
        //     }
        //     FormatType::WAT => {
        //         connect_sink_to_receiver(Wat::default(), rcvr, &text, input, &mut writer);
        //     }
        //     FormatType::WASM => {
        //         connect_sink_to_receiver(Wasm::default(), rcvr, &text, input, &mut writer);
        //     }
        // };

        writer
            .flush()
            .unwrap_or_else(|e| exit_with_error!(2, "I/O write error: {}", e));
    }

    parser_handle.join().expect("ERROR: parser thread error.")
        .unwrap_or_else(|e| exit_with_error!(2, "I/O write error: {}", e));
}
