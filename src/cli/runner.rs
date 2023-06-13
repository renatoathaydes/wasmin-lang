use std::sync::mpsc::Sender;

use crate::ast::TopLevelElement;
use crate::errors::Error;
use crate::parse::parser::Parser;

pub fn run_parser(file: String, sender: Sender<TopLevelElement>) -> Result<(), Error> {
    let text = read_file(file)?;
    let mut parser = Parser::new(&text);
    while let Some(element) = parser.parse_next() {
        match sender.send(element) {
            Ok(_) => {}
            Err(_) => {
                return Err(Error::Generic("Wasmin parser could not send out its output".to_owned()));
            }
        }
    }
    Ok(())
}

fn read_file(file: String) -> std::io::Result<String> {
    std::fs::read_to_string(file)
}