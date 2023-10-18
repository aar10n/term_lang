#![allow(warnings)]
#![feature(let_chains)]
mod command;
mod driver;
mod repl;

use crate::repl::repl_main;

use term_common::source::SourceId;
use term_core::Context;
use term_print::{PrettyPrint, PrettyString};

use atty::Stream;
use clap::Parser;
use std::{io::Read, process};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Evaluate the given expression")]
    expr: Vec<String>,

    #[arg(short, long, help = "Evaluate the given file")]
    file: Vec<String>,

    #[arg(short, long, help = "Run in interactive mode")]
    interactive: bool,
}

fn main() {
    let args = Args::parse();

    let mut interactive = args.interactive;
    let mut sources = Vec::new();
    if !args.expr.is_empty() {
        sources.push(("<expr>".to_owned(), args.expr.join("\n")));
    }
    if !args.file.is_empty() {
        for path in args.file {
            sources.push((path.clone(), read_from_file(&path)));
        }
    }
    if atty::is(Stream::Stdin) {
        interactive |= sources.is_empty();
    } else {
        sources.push(("<stdin>".to_owned(), read_from_stdin()));
    }

    let mut ctx = driver::new_context();
    for (file, src) in sources.into_iter() {
        let source_id = ctx.sources.add(file, src);
        if let Err(report) = driver::evaluate(&mut ctx, source_id, false) {
            report.print_stderr(&ctx.sources);
            // ctx.print_stdout(&());
            process::exit(1);
        }
    }

    if interactive {
        repl_main(&mut ctx, execute);
    }
}

fn execute<'a>(ctx: &mut Context, source_id: SourceId, exit: bool) {
    match driver::evaluate(ctx, source_id, true) {
        Ok(_) => (),
        Err(report) => {
            report.print_stderr(&ctx.sources);
            if exit {
                process::exit(1);
            }
        }
    }
}

//

fn read_from_file(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("error: {:?}", err);
            process::exit(1);
        }
    }
}

fn read_from_stdin() -> String {
    let mut code = String::new();
    match std::io::stdin().read_to_string(&mut code) {
        Ok(_) => code,
        Err(err) => {
            eprintln!("error: {:?}", err);
            process::exit(1);
        }
    }
}
