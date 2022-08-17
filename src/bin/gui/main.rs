#[cfg(feature = "gui")]
mod gui {
	use pixels::Pixels;
	use std::{io::Read, io::Write, io::BufRead, sync::Mutex, collections::HashMap};

	use sdl2::audio::AudioDevice;
	use sdl2::video::Window;
	use sdl2::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};

	use datapack_vm::interpreter;

	use std::path::Path;

	pub struct SDLSystem {
		pub ctx: Sdl,
		pub video_subsystem: VideoSubsystem,
		pub window: Window,
	}

	impl SDLSystem {
		pub fn new() -> SDLSystem {
			let ctx = sdl2::init().unwrap();
			let video_subsystem = ctx.video().unwrap();
			let window = video_subsystem
				.window("Terrible NES", 1024 + 256, 480)
				.position_centered()
				.build()
				.unwrap();
			SDLSystem {
				ctx,
				video_subsystem,
				window,
			}
		}
	}

	impl Default for SDLSystem {
		fn default() -> SDLSystem {
			SDLSystem::new()
		}
	}

	struct McState {
		pixels: Pixels,
	}


	const SCREEN_WIDTH: u32 = 1024 + 256;
	const SCREEN_HEIGHT: u32 = 480;

	impl McState {
		/*pub fn set(&mut self, block: i32) {
			self.set_at(block, self.turtle_x, self.turtle_y, self.turtle_z);
		}*/

		pub fn set_at(&mut self, block: i32, x: i32, y: i32, z: i32) {
			//self.blocks.insert((x, y, z), block);

			if x >= 0 && y >= 0 && z == -20 {
				let index = 4 * ((500 - x as u32) + (300 - y) as u32 * SCREEN_WIDTH);

				let pixel = &mut self.pixels.get_frame()[index as usize..][..4];
				match block {
					0  /* Air      */ => pixel.copy_from_slice(&[0x00, 0x00, 0x00, 0xFF]),
					1  /* Cobble   */ => pixel.copy_from_slice(&[0x64, 0x64, 0x64, 0xFF]),
					2  /* Granite  */ => pixel.copy_from_slice(&[0x7A, 0x55, 0x48, 0xFF]),
					3  /* Andesite */ => pixel.copy_from_slice(&[0x69, 0x69, 0x69, 0xFF]),
					4  /* Diorite  */ => pixel.copy_from_slice(&[0x9A, 0x9A, 0x9B, 0xFF]),
					5  /* Lapis    */ => pixel.copy_from_slice(&[0x00, 0x00, 0xFF, 0xFF]),
					6  /* Iron     */ => pixel.copy_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF]),
					7  /* Gold     */ => pixel.copy_from_slice(&[0xBB, 0xA0, 0x37, 0xFF]),
					8  /* Diamond  */ => pixel.copy_from_slice(&[0x5A, 0xB1, 0xCA, 0xFF]),
					9  /* Redstone */ => pixel.copy_from_slice(&[0xFF, 0x00, 0x00, 0xFF]),
					10 /* Emerald  */ => pixel.copy_from_slice(&[0x00, 0xFF, 0x00, 0xFF]),
					11 /* Dirt     */ => pixel.copy_from_slice(&[0x69, 0x2D, 0x00, 0xFF]),
					12 /* Oak Log  */ => pixel.copy_from_slice(&[0x5D, 0x49, 0x2B, 0xFF]),
					13 /* Oak Leaf */ => pixel.copy_from_slice(&[0x2F, 0x47, 0x20, 0xFF]),
					14 /* Coal     */ => pixel.copy_from_slice(&[0x0D, 0x0D, 0x0D, 0xFF]),
					_ => pixel.copy_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF]),
				}
			}

			if block == 9 && x == 129 && y == 1 {
				self.pixels.render().unwrap();
				std::thread::sleep(std::time::Duration::from_millis(100));
			}
		}
	}

	pub fn main() {
		let args = std::env::args().skip(1).collect::<Vec<String>>();

		match &args[..] {
			[] => eprintln!("not enough arguments"),
			[path] => {
				run(Path::new(&path));
			}
			_ => eprintln!("too many arguments"),
		}
	}

	pub fn run(path: &Path) {
		let dir = datapack_common::vfs::Directory::open(path).unwrap();
		let datapack = datapack_common::Datapack::from_directory(&dir).unwrap();

		let sdl_system = SDLSystem::new();
		let pixels = Pixels::new(SCREEN_WIDTH, SCREEN_HEIGHT, pixels::SurfaceTexture::new(SCREEN_WIDTH, SCREEN_HEIGHT, &sdl_system.window)).unwrap();

		let state = Mutex::new(McState { pixels });
		let state = Box::leak(Box::new(state));

		let handle = std::thread::spawn(|| {
			let mut interp = datapack_vm::Interpreter::new(datapack.functions, 0);

			use interpreter::Block;

			let setblock_callback = |_old_block: Option<interpreter::Block>, new_block: Option<&Block>, x: i32, y: i32, z: i32| {
				let block_idx = match new_block {
					None => 0, /* Air */
					Some(Block::Other(id)) if id == "minecraft:cobblestone" => 1, /* Cobblestone */
					Some(Block::Other(id)) if id == "minecraft:granite" => 2,
					Some(Block::Other(id)) if id == "minecraft:andesite" => 3,
					Some(Block::Other(id)) if id == "minecraft:diorite" => 4,
					Some(Block::Other(id)) if id == "minecraft:lapis_block" => 5,
					Some(Block::Other(id)) if id == "minecraft:iron_block" => 6,
					Some(Block::Other(id)) if id == "minecraft:gold_block" => 7,
					Some(Block::Other(id)) if id == "minecraft:diamond_block" => 8,
					Some(Block::Redstone) => 9, /* Redstone */
					Some(Block::Other(id)) if id == "minecraft:emerald_block" => 10,
					Some(Block::Other(id)) if id == "minecraft:dirt" => 11,
					Some(Block::Other(id)) if id == "minecraft:green_wool" => 13,
					Some(Block::Other(id)) if id == "minecraft:coal_block" => 14,

					Some(Block::Jukebox(_)) => return,
					Some(Block::Command(_)) => return,

					_ => { println!("unknown block {:?}", new_block); return },
				};

				let mut s = state.lock().unwrap();

				s.set_at(block_idx, x, y, z);
			};

			interp.setblock_callback = Some(Box::new(setblock_callback));

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

		let mut event_pump = sdl_system.ctx.event_pump().unwrap();

		loop {

			for event in event_pump.poll_iter() {
				match event {
					sdl2::event::Event::Quit { .. }
					| sdl2::event::Event::KeyDown {
						keycode: Some(sdl2::keyboard::Keycode::Escape),
						..
					} => {
						std::process::exit(0);
					}
					_ => {}
				}
			}

			std::thread::sleep(std::time::Duration::from_millis(16));

			let l = state.lock().unwrap();
			l.pixels.render().unwrap();
		}
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