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
                        Ok(Closure::new(
                            mc,
                            compile(mc, lc.interned_strings, line_clone.as_bytes())?,
                            Some(lc.globals),
                        )?)
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
                                Ok(Some(output))
                            }
                            Err(Error::ParserError(ParserError::EndOfStream { expected: _ })) => {
                                // continue reading input and append it to `line`
                                Ok(None)
                            }
                            Err(e) => Err(format!("error: {}", e)),
                        }
                    }),
                )
            }) {
                Ok(Some(out_string)) | Err(out_string) => {
                    editor.add_history_entry(line);
                    println!("{}", out_string);
                    break;
                }
                Ok(None) => {
                    prompt = ">> ";
                    line.push_str("\n"); // separate input lines
                }
            }
        }
    }
}
