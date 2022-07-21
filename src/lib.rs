pub mod interpreter;

mod parsed;

pub use interpreter::Interpreter;

/*pub fn run_program(funcs: Vec<Function>, start_func: &str) -> Vec<String> {
    let funcs = get_functions(dir).unwrap();

    let idx = funcs.iter().enumerate().find(|(_, f)| {
        f.id.path == "main"
    }).unwrap_or_else(|| {
        eprintln!("Failed to find {:?}", "setup");
        panic!();
    }).0;

    let mut i = Interpreter::new(funcs, idx);

    i.run_to_end().unwrap();

    i.output
}*/

#[cfg(test)]
mod tests {}
