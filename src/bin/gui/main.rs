#[cfg(feature = "gui")]
mod gui {
	use std::{io::Read, io::Write, io::BufRead, sync::Mutex, collections::HashMap};

	use sdl2::audio::AudioDevice;
	use sdl2::video::Window;
	use sdl2::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};

	use datapack_vm::interpreter;

	use std::path::Path;

	use datapack_vm::gui::Cfg;

	use command_parser::CommandParse;

	pub fn main() {
		let args = std::env::args().skip(1).collect::<Vec<String>>();

		match &args[..] {
			[] => {
				eprintln!("not enough arguments!");
				datapack_vm::gui::print_usage();
				std::process::exit(1);
			}
			[flags@.., path] => {
				let cfg = match Cfg::new(flags) {
					Ok(cfg) => cfg,
					Err(()) => {
						eprintln!("invalid arguments!");
						datapack_vm::gui::print_usage();
						std::process::exit(1);
					}
				};

				run(cfg, Path::new(path));
			}
		}
	}

	pub fn run(vm_cfg: Cfg, path: &Path) {
		let dir = datapack_common::vfs::Directory::open(path).unwrap();
		let datapack = datapack_common::Datapack::from_directory(&dir).unwrap().functions;

		let indiv_time = vec![0; datapack.len()];
		let intrin_cum_times = vec![0; datapack.len()];
		let intrin_visited = vec![false; datapack.len()];
		let intrin_funcs = datapack.iter().map(|func| func.id.namespace == "intrinsic").collect::<Vec<_>>();

		let mut interp = datapack_vm::Interpreter::new(datapack, 0);

		interp.max_total_commands = 1_500_000_000;
		interp.max_tick_commands = 60_000_000;

		let (sdl, state) = datapack_vm::gui::setup(&mut interp, vm_cfg);

		let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:init").unwrap();

		let interp_idx = interp.get_func_idx(&func_name);
		interp.set_pos(interp_idx);

		interp.run_to_end().unwrap();

		interp.max_tick_commands = 500_000;

		let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:_start").unwrap();

		let interp_idx = interp.get_func_idx(&func_name);
		interp.set_pos(interp_idx);

		std::thread::spawn(move || {
			let mut stdout = std::io::stdout();

			let stdin = std::io::stdin();
			let mut stdin = stdin.lock();

			loop {
				print!("> ");
				stdout.flush().unwrap();
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
				} else if let Some(amt) = input.strip_prefix("set max_total_commands ") {
					let amt = amt.parse::<u64>().unwrap();

					interp.max_total_commands = amt;
					println!("set max total commands to {}", amt);
				} else if input == "run" {
					match interp.run_to_end() {
						Ok(()) => println!("ran successfully"),
						Err(err) => println!("encountered an error: {}", err),
					}
				} else {
					println!("invalid command {:?}", input)
				}
			}
		});

		datapack_vm::gui::run(sdl, state);
	}
}

#[cfg(feature = "gui")]
fn main() {
	gui::main();
}

#[cfg(not(feature = "gui"))]
fn main() {
	println!("must be run with the gui feature enabled.");
}