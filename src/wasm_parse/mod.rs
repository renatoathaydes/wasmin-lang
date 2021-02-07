use wasmparser::{BinaryReaderError, Parser, Payload};

pub fn parse(file: String, verbose: bool) -> Result<(), String> {
    let buf = std::fs::read(file).map_err(|e| e.to_string())?;
    parse_buf(&buf, verbose).map_err(|e| e.to_string())
}

pub fn parse_buf(buf: &[u8], verbose: bool) -> Result<(), BinaryReaderError> {
    let mut fun_index = 0;
    let mut imported_funs = 0;
    for payload in Parser::new(0).parse_all(&buf) {
        match payload? {
            Payload::Version { num, .. } => {
                println!("====== Module (v{})", num);
            }
            Payload::ExportSection(s) => {
                println!("------ Exports Section");
                for export in s {
                    let export = export?;
                    println!("  Export {} {:?} [{}]", export.field, export.kind, export.index);
                }
            }
            Payload::ImportSection(s) => {
                println!("------ Imports Section");
                for import in s {
                    let import = import?;
                    println!("  [{}] Function {}::{} -> {:?}", fun_index, import.module, import.field.unwrap(), import.ty);
                    fun_index += 1;
                    imported_funs += 1;
                }
            }
            Payload::TypeSection(t) => {
                println!("------ Type Section");
                let mut i = 0;
                for typ in t {
                    let typ = typ?;
                    println!("  [{}] {:?}", i, typ);
                    i += 1;
                }
            }
            Payload::FunctionSection(f) => {
                println!("------ Functions Section");
                for fun in f {
                    let fun = fun?;
                    println!("  [{}] Function (type {})", fun_index, fun);
                    fun_index += 1;
                }
            }
            Payload::CodeSectionStart { count, .. } => {
                println!("------ Code Section ({} functions)", count);
                fun_index = imported_funs;
            }
            Payload::CodeSectionEntry(body) => {
                println!("  [{}] Function Body", fun_index);
                fun_index += 1;
                if verbose {
                    let locals = body.get_locals_reader()?;
                    for local in locals {
                        let (idx, typ) = local?;
                        println!("    local[{}] -> ${:?}", idx, typ);
                    }
                    let mut reader = body.get_binary_reader();
                    while !reader.eof() {
                        let op = reader.read_operator();
                        println!("    {:?}", op?);
                    }
                }
            }
            Payload::GlobalSection(g) => {
                println!("------ Globals Section");
                let mut i = 0;
                for global in g {
                    let global = global?;
                    println!("  [{}] {:?} = {:?}", i, global.ty, global.init_expr.get_binary_reader().read_operator()?);
                    i += 1;
                }
            }
            Payload::End => {
                println!("======");
            }
            _other => {
                println!("  Unsupported Payload: {:?}", _other);
            }
        }
    }
    Ok(())
}
