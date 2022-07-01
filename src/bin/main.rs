use std::{path::Path, io::{BufRead, Write}};

fn main() {
	let args = std::env::args().skip(1).collect::<Vec<String>>();

	match &args[..] {
		[] => eprintln!("not enough arguments"),
		[path] => {
			run(Path::new(&path));
		}
		_ => eprintln!("too many arguments"),
	}
}

fn run(path: &Path) {
	let dir = datapack_common::vfs::Directory::open(path).unwrap();
	let datapack = datapack_common::Datapack::from_directory(&dir).unwrap();

	let mut interp = datapack_vm::Interpreter::new(datapack.functions, 0);

	let mut stdout = std::io::stdout();

	let stdin = std::io::stdin();
	let mut stdin = stdin.lock();

	loop {
		print!("> ");
		stdout.flush();
		let mut input = String::new();
		stdin.read_line(&mut input).unwrap();
		let input = input.trim();

		if input == "exit" {
			break;
		} else if let Some(func) = input.strip_prefix("setpc ") {
			let idx = interp.program.iter().enumerate().find(|(_, f)| {
				f.id.to_string() == func
			}).map(|(i, _)| i);

			if let Some(idx) = idx {
				interp.set_pos(idx);
				println!("program will now start at {}", func);
			} else {
				println!("couldn't find function {:?}", func);
			}
		} else if input == "run" {
			match interp.run_to_end() {
				Ok(()) => println!("ran successfully"),
				Err(err) => println!("encountered an error: {}", err),
			}
		} else {
			println!("invalid command {:?}", input)
		}
	}
}