use std::fs::File;
use std::io::Read;

use piccolo::{io::buffered_read, Closure, Executor, Lua};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load the Lua file
    let mut file = buffered_read(File::open("./examples/execute.lua")?)?;
    let mut source = Vec::new();
    file.read_to_end(&mut source)?;

    // Instantiate the Lua instance
    let mut lua = Lua::full();

    // Enter a context
    let ex = lua.try_enter(|ctx| {
        // Get the global env
        let env = ctx.globals();
        // Run the lua script in the global context
        let closure = Closure::load_with_env(ctx, None, &*source, env)?;
        // Create an executor that will run the lua script
        let ex = Executor::start(ctx, closure.into(), ());

        // Return the executor to ouside the scope. We must stash it to allow it to escape the scope.
        Ok(ctx.stash(ex))
    })?;

    // Use the `execute` helper to execute the lua script
    lua.execute::<()>(&ex)?;

    // Enter a new context
    let ex = lua.try_enter(|ctx| {
        // Get the global env
        let env = ctx.globals();

        // Get the function out of the lua context
        let foo = env.get::<_, Closure>(ctx, "foo")?;

        // Create an executor to run it, and pass it our arguments list.
        let ex = Executor::start(ctx, foo.into(), "this is my message");

        // Return it stashed so we can pull it out of the context
        Ok(ctx.stash(ex))
    })?;

    // Get the return value
    let result = lua.execute::<String>(&ex)?;

    dbg!(result);

    Ok(())
}
