use std::fmt::format;

use term_common::source::SourceId;
use term_core::Context;
use term_parse as parse;
use term_print::PrettyPrint;

use rustyline::error::ReadlineError;
use rustyline::{Cmd, DefaultEditor, EventHandler, KeyCode, KeyEvent, Modifiers};

pub fn repl_main(ctx: &mut Context, execute: fn(&mut Context, SourceId, bool)) {
    let mut rl = DefaultEditor::new().unwrap();
    _ = rl.load_history("target/history.txt");
    rl.bind_sequence(
        KeyEvent(KeyCode::Tab, Modifiers::NONE),
        EventHandler::Simple(Cmd::Insert(1, "  ".into())),
    );
    rl.bind_sequence(
        KeyEvent(KeyCode::Char('N'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );

    let mut count = 0;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let source_id = ctx.sources.add(format!("<repl {count}>"), line.clone());
                execute(ctx, source_id, false);
                rl.add_history_entry(line).unwrap();
                count += 1;
            }
            Err(ReadlineError::Interrupted) => {
                println!("ctrl-c");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("ctrl-d");
                break;
            }
            Err(err) => {
                eprintln!("error: {:?}", err);
                break;
            }
        }
    }

    _ = rl.save_history("target/history.txt");
}
