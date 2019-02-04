use luster::{
    compile, sequence_fn, Closure, Error, Function, Lua, ParserError, SequenceExt, StaticError,
    ThreadSequence,
};
use rustyline::Editor;

fn main() {
    let mut lua = Lua::new();
    let mut editor = Editor::<()>::new();

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            match editor.readline(prompt) {
                Ok(input) => line.push_str(&input),
                Err(_) => return,
            }

            let line_clone = line.clone();

            match lua.sequence(move |_| {
                Box::new(
                    sequence_fn(move |mc, lc| {
                        let result = compile(mc, lc.interned_strings, line_clone.as_bytes());
                        let result = match result {
                            Ok(res) => Ok(res),
                            err @ Err(Error::ParserError(ParserError::EndOfStream { expected: _ })) => err,
                            Err(_) => compile(
                                mc,
                                lc.interned_strings,
                                (String::new() + "return " + &line_clone).as_bytes(),
                            ),
                        };
                        Ok(Closure::new(mc, result?, Some(lc.globals))?)
                    })
                    .and_then(|mc, lc, closure| {
                        Ok(ThreadSequence::call_function(
                            mc,
                            lc.main_thread,
                            Function::Closure(closure),
                            &[],
                        )?)
                    })
                    .flatten()
                    .map_result(|values| match values {
                        Ok(values) => {
                            let output = values
                                .iter()
                                .map(|value| format!("{:?}", value))
                                .collect::<Vec<_>>()
                                .join("\t");
                            Ok(output)
                        }
                        Err(e) => Err(e.to_static()),
                    }),
                )
            }) {
                err @ Err(StaticError::ParserError(ParserError::EndOfStream { expected: _ })) => {
                    match line.chars().last() {
                        Some(c) => {
                            if c == '\n' {
                                editor.add_history_entry(line);
                                eprintln!("error: {}", err.err().unwrap());
                                break;
                            }
                            prompt = ">> ";
                            line.push_str("\n"); // separate input lines
                        }
                        _ => {}
                    }
                }
                Ok(out_string) => {
                    editor.add_history_entry(line);
                    println!("{}", out_string);
                    break;
                }
                Err(e) => {
                    editor.add_history_entry(line);
                    eprintln!("error: {}", e);
                    break;
                }
            }
        }
    }
}
