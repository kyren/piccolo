use luster::{
    compile, sequence_fn, Closure, Error, Function, Lua, ParserError, SequenceExt, ThreadSequence,
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
                    .map_result(|values| {
                        match values {
                            Ok(values) => {
                                let output = values
                                    .iter()
                                    .map(|value| format!("{:?}", value))
                                    .collect::<Vec<_>>()
                                    .join("\t");
                                Ok(Ok(output))
                            }
                            Err(Error::ParserError(ParserError::EndOfStream { expected: e })) => {
                                // continue reading input and append it to `line`
                                Ok(Err(Error::ParserError(ParserError::EndOfStream {
                                    expected: e,
                                })))
                            }
                            Err(e) => Err(format!("error: {}", e)),
                        }
                    }),
                )
            }) {
                Ok(Ok(out_string)) | Err(out_string) => {
                    editor.add_history_entry(line);
                    println!("{}", out_string);
                    break;
                }
                Ok(Err(out_string)) => {
                    match line.chars().last() {
                        Some(c) => {
                            if c == '\n' {
                                editor.add_history_entry(line);
                                println!("{}", out_string);
                                break;
                            }
                            prompt = ">> ";
                            line.push_str("\n"); // separate input lines
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}
