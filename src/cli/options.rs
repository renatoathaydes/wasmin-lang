use std::str::FromStr;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub enum FormatType {
    DEBUG,
    WAT,
    WASM,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "wasmin", about = "Wasmin Compiler.")]
pub enum CliOptions {
    /// Build a Wasmin module.
    Build {
        #[structopt(
        short = "f",
        long = "format",
        default_value = "wasm",
        help = "output format (wat, wasm, debug)"
        )]
        output_format: FormatType,
        /// Wasmin main source file.
        input: String,
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
