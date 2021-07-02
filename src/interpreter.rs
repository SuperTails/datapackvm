use crate::cir::*;
//use crate::compile_ir::{get_index, pos_to_func_idx, OBJECTIVE};
//use crate::Datapack;
use std::collections::HashMap;
use std::str::FromStr;

pub fn get_index(x: i32, y: i32, z: i32) -> Result<i32, InterpError> {
    if 0 <= x && x < 8 && 0 <= y && y < 256 && 0 <= z && z < 8 {
        Ok((x * 8 * 8 + y * 8 + z) * 4)
    } else {
        Err(InterpError::OutOfBoundsAccess(x, y, z))
    }
}

// FIXME: Multiple conditions and a `store success` does not work like I think it does!!!

#[derive(Debug, Clone, PartialEq)]
pub enum InterpError {
    OutOfBoundsAccess(i32, i32, i32),
    MaxCommandsRun,
    EnteredUnreachable,
    EnteredTodo,
    AssertionFailed,
    BreakpointHit,
    InvalidBranch(usize),
    MultiBranch(FunctionId, Option<FunctionId>),
    NoBlockData(i32, i32, i32, String),
}

impl std::fmt::Display for InterpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpError::OutOfBoundsAccess(x, y, z) => {
                write!(f, "out of bounds access at x={}, y={}, z={}", x, y, z)
            }
            InterpError::MaxCommandsRun => write!(f, "ran too many commands at once"),
            InterpError::EnteredUnreachable => write!(f, "entered unreachable code"),
            InterpError::EnteredTodo => write!(f, "entered code not yet implemented"),
            InterpError::AssertionFailed => write!(f, "assertion failed"),
            InterpError::BreakpointHit => write!(f, "breakpoint hit"),
            InterpError::InvalidBranch(b) => write!(f, "invalid branch to {}", b),
            InterpError::MultiBranch(prev, att) => {
                write!(f, "branch to more than one block (previous was {}, attempted was ", prev)?;
                if let Some(att) = att {
                    write!(f, "{}", att)?;
                } else {
                    write!(f, "invalid")?;
                }
                write!(f, ")")
            }
            InterpError::NoBlockData(x, y, z, path) => write!(f, "couldn't read path {} at {} {} {}", path, x, y, z),
        }
    }
}

impl std::error::Error for InterpError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BreakKind {
    Read,
    Write,
    Access,
}

#[derive(Default, Debug)]
pub struct Scoreboard(pub HashMap<String, HashMap<ScoreHolder, i32>>);

impl Scoreboard {
    pub fn get(&self, holder: &ScoreHolder, obj: &String) -> Option<i32> {
        let scores = self.0.get(obj)?;
        scores.get(holder).copied()
    }

    pub fn set(&mut self, holder: &ScoreHolder, obj: &String, value: i32) {
        let scores = self.0.get_mut(obj).unwrap();
        scores.insert(holder.to_owned(), value);

    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Block {
    Command(String),
    Redstone,
    Jukebox(i32),
    Other(String),
}

impl FromStr for Block {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "minecraft:redstone_block" {
            Ok(Block::Redstone)
        } else if s.starts_with("minecraft:jukebox") {
            let idx = s.find("Memory:").unwrap();
            let rest = &s[idx + "Memory:".len()..];
            let end_idx = rest.find('}').unwrap();

            let val = rest[..end_idx].parse().unwrap();
            Ok(Block::Jukebox(val))
        } else if s.starts_with("minecraft:command_block") {
            let start = s.find("{Command:\"").unwrap() + "{Command:\"".len();
            let end = s.find("\"}").unwrap();
            Ok(Block::Command(s[start..end].to_string()))
        } else {
            Ok(Block::Other(s.to_string()))
        }
    }
}

pub struct Interpreter {
    pub scoreboard: Scoreboard,

    pub(crate) call_stack: Vec<(usize, usize)>,
    ctx_pos: Option<(i32, i32, i32)>,
    program: Vec<Function>,

    memory: [i32; 8 * 256 * 8],

    blocks: HashMap<(i32, i32, i32), Block>,

    memory_ptr_pos: (i32, i32, i32),
    local_ptr_pos: (i32, i32, i32),
    global_ptr_pos: (i32, i32, i32),
    frame_ptr_pos: (i32, i32, i32),
    stack_ptr_pos: (i32, i32, i32),

    ptr_pos: (i32, i32, i32),
    turtle_pos: (i32, i32, i32),
    next_pos: Option<(usize, usize, (i32, i32, i32))>,
    letters: HashMap<(i32, i32, i32), char>,
    pub output: Vec<String>,
    pub tick: usize,
    commands_run: usize,

    memory_points: HashMap<usize, BreakKind>,

    /// FIXME: Add support for the *real* commands
    stdout_buffer: String,
}

#[derive(Default, Clone)]
struct Context {
    pos: Option<(i32, i32, i32)>,
    ident: Option<String>,
}

impl Interpreter {
    pub fn set_pos(&mut self, func_idx: usize) {
        self.call_stack = vec![(func_idx, 0)];
        self.ctx_pos = None;
    }

    pub fn new_wasm(program: Vec<Function>, func_idx: usize, input: &str) -> Self {
        let mut letters = HashMap::new();
        for (z, letter) in input.chars().enumerate() {
            if letter != ' ' {
                letters.insert((-16, 16, -(z as i32)), letter);
            }
        }

        Interpreter {
            program,
            ctx_pos: None,
            call_stack: vec![(func_idx, 0)],
            blocks: HashMap::new(),
            memory_ptr_pos: Default::default(),
            frame_ptr_pos: Default::default(),
            local_ptr_pos: Default::default(),
            stack_ptr_pos: Default::default(),
            global_ptr_pos: Default::default(),
            memory: [0x55_55_55_55; 8 * 256 * 8],
            scoreboard: Scoreboard::default(),
            ptr_pos: (0, 0, 0),
            turtle_pos: (0, 0, 0),
            next_pos: None,
            commands_run: 0,
            tick: 0,
            output: Vec::new(),
            memory_points: HashMap::new(),
            letters,
            stdout_buffer: String::new(),
        }

    }

    /// Does not include any intrinsics
    pub fn new_raw(program: Vec<Function>, input: &str) -> Self {
        let func_idx = program.len() - 1;

        let mut letters = HashMap::new();
        for (z, letter) in input.chars().enumerate() {
            if letter != ' ' {
                letters.insert((-16, 16, -(z as i32)), letter);
            }
        }

        Interpreter {
            program,
            ctx_pos: None,
            call_stack: vec![(func_idx, 0)],
            blocks: HashMap::new(),
            memory_ptr_pos: Default::default(),
            frame_ptr_pos: Default::default(),
            local_ptr_pos: Default::default(),
            stack_ptr_pos: Default::default(),
            global_ptr_pos: Default::default(),
            memory: [0x55_55_55_55; 8 * 256 * 8],
            scoreboard: Scoreboard::default(),
            ptr_pos: (0, 0, 0),
            turtle_pos: (0, 0, 0),
            next_pos: None,
            commands_run: 0,
            tick: 0,
            output: Vec::new(),
            memory_points: HashMap::new(),
            letters,
            stdout_buffer: String::new(),
        }
    }

    pub fn new(datapack: Vec<Function>, start_idx: usize, input: &str) -> Self {
        let mut letters = HashMap::new();
        let mut z = 0;
        let mut y = 32;
        for letter in input.chars() {
            match letter {
                '\n' => {
                    z = 0;
                    y -= 2;
                }
                ' ' => {
                    z -= 1;
                }
                _ => {
                    letters.insert((-16, y, z), letter);
                    z -= 1;
                }
            }
        }

        Interpreter {
            program: datapack,
            ctx_pos: None,
            call_stack: vec![(start_idx, 0)],
            blocks: HashMap::new(),
            memory: [0; 8 * 256 * 8],
            memory_ptr_pos: Default::default(),
            frame_ptr_pos: Default::default(),
            local_ptr_pos: Default::default(),
            stack_ptr_pos: Default::default(),
            global_ptr_pos: Default::default(),
            scoreboard: Scoreboard::default(),
            ptr_pos: (0, 0, 0),
            turtle_pos: (0, 0, 0),
            next_pos: None,
            tick: 0,
            commands_run: 0,
            output: Vec::new(),
            memory_points: HashMap::new(),
            letters,
            stdout_buffer: String::new(),
        }
    }

    pub fn program(&self) -> &[Function] {
        &self.program
    }

    pub fn call_stack(&self) -> Vec<(&Function, usize)> {
        self.call_stack
            .iter()
            .copied()
            .map(|(f, c)| (&self.program[f], c))
            .collect()
    }

    /// `word_start` is in bytes, must be aligned to a multiple of 4
    pub fn set_mem_breakpoint(&mut self, word_start: usize, kind: BreakKind) {
        assert_eq!(word_start % 4, 0);

        self.memory_points.insert(word_start / 4, kind);
    }

    pub fn set_next_pos(&mut self, func_idx: usize, pos: (i32, i32, i32)) -> Result<(), InterpError> {
        if let Some((f, _, _)) = self.next_pos {
            let att = self.program.get(func_idx).map(|f| f.id.clone());
           Err(InterpError::MultiBranch(self.program[f].id.clone(), att))
        } else if func_idx >= self.program.len() {
            Err(InterpError::InvalidBranch(func_idx))
        } else {
            self.next_pos = Some((func_idx, 0, pos));
            Ok(())
        }
    }

    pub fn get_word(&self, addr: usize) -> Result<i32, InterpError> {
        assert_eq!(addr % 4, 0);

        match self.memory_points.get(&(addr / 4)) {
            Some(BreakKind::Access) | Some(BreakKind::Read) => {
                return Err(InterpError::BreakpointHit)
            }
            _ => {}
        };

        Ok(self.memory[addr / 4])
    }

    pub fn get_byte(&self, addr: usize) -> Result<u8, InterpError> {
        let word_addr = (addr / 4) * 4;

        Ok(self.get_word(word_addr)?.to_le_bytes()[addr % 4])
    }

    pub fn set_word(&mut self, value: i32, addr: usize) -> Result<(), InterpError> {
        assert_eq!(addr % 4, 0);

        match self.memory_points.get(&(addr / 4)) {
            Some(BreakKind::Access) | Some(BreakKind::Write) => {
                return Err(InterpError::BreakpointHit)
            }
            _ => {}
        };

        self.memory[addr / 4] = value;

        Ok(())
    }

    pub fn set_byte(&mut self, value: u8, addr: usize) -> Result<(), InterpError> {
        match self.memory_points.get(&(addr / 4)) {
            Some(BreakKind::Access) | Some(BreakKind::Write) => {
                return Err(InterpError::BreakpointHit)
            }
            _ => {}
        };

        let mut bytes = self.memory[addr / 4].to_le_bytes();
        bytes[addr % 4] = value;
        self.memory[addr / 4] = i32::from_le_bytes(bytes);

        Ok(())
    }

    /// Runs until the program halts
    pub fn run_to_end(&mut self) -> Result<(), InterpError> {
        while !self.halted() {
            self.step()?
        }
        Ok(())
    }

    pub fn next_command(&self) -> Option<&Command> {
        if !self.halted() {
            let (func_idx, cmd_idx) = self.call_stack.last().unwrap();
            Some(&self.program[*func_idx].cmds[*cmd_idx])
        } else {
            None
        }
    }

    pub fn eval_message(&self, msg: &[TextComponent]) -> String {
        let mut result = String::new();
        let score_getter = |name: &ScoreHolder, obj: &Objective| -> Option<i32> {
            self.scoreboard.get(name, obj)
        };

        for s in msg.iter().map(|m| m.as_string(&score_getter).unwrap()) {
            result.push_str(&s);
        }
        result
    }

    pub fn get_score(&self, holder: &ScoreHolder, obj: &Objective) -> Result<i32, String> {
        self.scoreboard.get(holder, obj).ok_or_else(|| format!("read from uninitialized variable {} {}", holder, obj))
    }

    pub fn check_cond(&self, is_unless: bool, cond: &ExecuteCondition) -> bool {
        let result = match cond {
            ExecuteCondition::Score {
                target: Target::Uuid(target),
                target_obj,
                kind,
            } => {
                let target = self.get_score(target, target_obj).unwrap();

                match kind {
                    ExecuteCondKind::Relation {
                        relation,
                        source: Target::Uuid(source),
                        source_obj,
                    } => {
                        let source = self.get_score(source, source_obj).unwrap();

                        match relation {
                            Relation::LessThan => target < source,
                            Relation::LessThanEq => target <= source,
                            Relation::Eq => target == source,
                            Relation::GreaterThan => target > source,
                            Relation::GreaterThanEq => target >= source,
                        }
                    }
                    ExecuteCondKind::Matches(m) => m.contains(target),
                    _ => todo!("{:?}", kind),
                }
            }
            _ => todo!("{:?}", cond),
        };

        if is_unless {
            !result
        } else {
            result
        }
    }

    fn read_mem(&self) -> Result<i32, InterpError> {
        let index = get_index(self.ptr_pos.0, self.ptr_pos.1, self.ptr_pos.2)?;
        self.get_word(index as usize)
    }

    fn set_block(&mut self, pos: (i32, i32, i32), block: &str, mode: SetBlockKind) {
        if block == "minecraft:air" {
            self.blocks.remove(&pos);
        } else {
            let block = block.parse::<Block>().unwrap();

            if block == Block::Redstone && (self.blocks.get(&pos) != Some(&Block::Redstone) || mode == SetBlockKind::Destroy) {
                let cmd = (pos.0, pos.1 - 1, pos.2);
                let b = self.blocks.get(&cmd).unwrap();
                if let Block::Command(c) = b {
                    let c = c.parse::<Command>().unwrap();
                    let id = if let Command::FuncCall(FuncCall { id }) = c {
                        id
                    } else {
                        todo!("{}", c)
                    };

                    let mut idx = None;
                    for (i, f) in self.program.iter().enumerate() {
                        if f.id == id {
                            idx = Some(i);
                            break;
                        }
                    }
                    let idx = idx.unwrap();

                    self.set_next_pos(idx, cmd).unwrap();
                }
            }

            self.blocks.insert(pos, block);
        }
    }

    fn get_pos(&self, target: &Target) -> (i32, i32, i32) {
        match target {
            Target::Selector(_) => {
                match target.to_string().as_str() {
                    "@e[tag=memoryptr]" => self.memory_ptr_pos,
                    "@e[tag=localptr]" => self.local_ptr_pos,
                    "@e[tag=frameptr]" => self.frame_ptr_pos,
                    "@e[tag=globalptr]" => self.global_ptr_pos,
                    "@e[tag=stackptr]" => self.stack_ptr_pos,
                    "@e[tag=turtle]" => self.turtle_pos,
                    t => todo!("{:?}", t),
                }
            }
            _ => todo!("{:?}", target)
        }
    }

    fn get_ident(&self, target: &Target) -> String {
        match target {
            Target::Selector(_) => {
                match target.to_string().as_str() {
                    "@e[tag=memoryptr]" => "memoryptr",
                    "@e[tag=localptr]" => "localptr",
                    "@e[tag=frameptr]" => "frameptr",
                    "@e[tag=globalptr]" => "globalptr",
                    "@e[tag=stackptr]" => "stackptr",
                    "@e[tag=turtle]" => "turtle",
                    t => todo!("{:?}", t),
                }.to_string()
            }
            _ => todo!("{:?}", target)
        }
    }

    fn get_block_data(&self, pos: (i32, i32, i32), path: &str) -> Result<i32, InterpError> {
        match self.blocks.get(&pos) {
            Some(Block::Jukebox(v)) => {
                if path == "RecordItem.tag.Memory" {
                    Ok(*v)
                } else {
                    Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string()))
                }
            }
            None => Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string())),
            b => todo!("{:?} {:?}", b, pos)
        }
    }

    fn set_block_data(&mut self, pos: (i32, i32, i32), path: &str, value: i32) -> Result<(), InterpError> {
        match self.blocks.get_mut(&pos) {
            Some(Block::Jukebox(v)) => {
                if path == "RecordItem.tag.Memory" {
                    *v = value;
                    Ok(())
                } else {
                    Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string()))
                }
            }
            None => Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string())),
            b => todo!("{:?}", b)
        }

    }

    fn execute_cmd(&mut self, cmd: &Command) -> Result<Option<i32>, InterpError> {
        let mut ctx = Context::default();

        self.execute_cmd_ctx(cmd, &mut ctx)
    }

    fn execute_cmd_ctx(&mut self, cmd: &Command, ctx: &mut Context) -> Result<Option<i32>, InterpError> {
        //println!("{}", cmd);
        /*if !self
            .call_stack
            .iter()
            .any(|(i, _)| self.program[*i].id.name.contains("intrinsic"))
        {
            eprintln!("{}", cmd);
        }*/

        match cmd {
            Command::ScoreAdd(ScoreAdd { target: Target::Uuid(target), target_obj, score }) => {
                let mut lhs = self.get_score(target, target_obj).unwrap();
                lhs = lhs.wrapping_add(*score);
                self.scoreboard.set(target, target_obj, lhs);

                Ok(None)
            }
            Command::ScoreOp(ScoreOp { target, target_obj, kind, source, source_obj }) => {
                if let Target::Uuid(target) = target {
                    if let Target::Uuid(source) = source {
                        let rhs = self.get_score(source, source_obj).unwrap();

                        match kind {
                            ScoreOpKind::Assign => {
                                self.scoreboard.set(target, target_obj, rhs);
                            }
                            ScoreOpKind::AddAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                val = val.wrapping_add(rhs);
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::SubAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                val -= rhs;
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::MulAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                val = val.wrapping_mul(rhs);
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::DivAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();

                                // Minecraft div rounds towards -infinity
                                if (val < 0 && rhs < 0) || (val > 0 && rhs > 0) {
                                    val /= rhs;
                                } else {
                                    val = val.abs();
                                    let rhs = rhs.abs();

                                    // -1 / 3

                                    val = -((val + rhs - 1) / rhs);
                                }

                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::ModAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                if rhs < 0 {
                                    todo!("DETERMINE BEHAVIOR")
                                } else {
                                    val = val.rem_euclid(rhs);
                                }
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::Min => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                val = val.min(rhs);
                                self.scoreboard.set(target, target_obj, val);
                            }
                            _ => todo!("{}", kind)
                        }
                    } else {
                        todo!("{}", source)
                    }
                } else {
                    todo!("{}", target)
                }

                Ok(None)
            }
            Command::FuncCall(FuncCall { id }) => {
                if id.name == "stdout:putc" {
                    todo!();
                    /*
                    let c = self.get_rust_score(&ScoreHolder::new("%%temp0_putc".into()).unwrap()).unwrap();
                    let c = char::from(u8::try_from(c).unwrap_or_else(|_| panic!("invalid argument to stdout:putc {}", c)));

                    if c == '\n' {
                        let out = std::mem::take(&mut self.stdout_buffer);
                        self.output.push(out);
                    } else if c.is_ascii() && !c.is_control() {
                        self.stdout_buffer.push(c);
                    } else {
                        panic!("invalid argument to stdout:putc `{}`", c)
                    }
                    */
                } else {
                    let called_idx = self.program.iter().enumerate().find(|(_, f)| &f.id == id).unwrap_or_else(|| todo!("{:?}", id)).0;
                    self.call_stack.push((called_idx, 0));
                }

                Ok(None)
            }
            Command::Fill(Fill { start, end, block }) => {
                let start = start.maybe_based(ctx.pos);
                let end = end.maybe_based(ctx.pos);

                assert!(start.0 <= end.0);
                assert!(start.1 <= end.1);
                assert!(start.2 <= end.2);

                for x in start.0..=end.0 {
                    for y in start.1..=end.1 {
                        for z in start.2..=end.2 {
                            // TODO: Kind??
                            self.set_block((x, y, z), block, SetBlockKind::Replace)
                        }
                    }
                } 

                Ok(None)
            }
            Command::Data(Data { target, kind }) => {
                match (target, kind) {
                    (DataTarget::Block(block), DataKind::Modify { path, kind: DataModifyKind::Set, source }) => {
                        let pos = block.maybe_based(ctx.pos);

                        if let DataModifySource::Value(score) = source {
                            self.set_block_data(pos, path, *score)?;
                            Ok(None)
                        } else {
                            todo!()
                        }
                    }
                    (DataTarget::Block(block), DataKind::Get { path, scale }) => {
                        let pos = block.maybe_based(ctx.pos);

                        // The number in the file is *always* 1.0
                        #[allow(clippy::float_cmp)]
                        { assert_eq!(*scale, 1.0); }

                        println!("Getting block data at {:?}", pos);

                        Ok(Some(self.get_block_data(pos, path)?))
                    }
                    _ => todo!("{:?} {:?}", target, kind),
                }
            }
            Command::ScoreSet(ScoreSet { target, target_obj, score }) => {
                match target {
                    Target::Uuid(target) => {
                        self.scoreboard.set(target, target_obj, *score);
                    }
                    _ => todo!("{:?}", target)
                }

                Ok(None)
            }
            Command::ScoreGet(ScoreGet { target, target_obj }) => {
                match target {
                    Target::Uuid(holder) => {
                        Ok(Some(self.scoreboard.get(holder, target_obj).unwrap_or_else(|| panic!("{:?} {:?}", holder, target_obj))))
                    }
                    _ => todo!("{:?}", target)
                }
            }
            Command::Tellraw(b) => {
                let Tellraw { message, target: _target } = &**b;
                let msg = self.eval_message(&message);
                println!("\n{}\n", msg);
                self.output.push(msg);

                Ok(None)
            }
            Command::SetBlock(SetBlock { pos, block, kind }) => {
                let pos = pos.maybe_based(ctx.pos);

                self.set_block(pos, block, *kind);

                Ok(None)
            }
            Command::Execute(Execute { subcommands, run }) => {
                let mut ctx = ctx.clone();

                let mut store = None;

                let mut cnd = Vec::new();

                for subcmd in subcommands.iter() {
                    match subcmd {
                        // TODO: DETERMINE HOW THIS INTERACTS WITH OTHER SELECTORS
                        ExecuteSubCmd::Condition { is_unless, cond } => {
                            cnd.push((*is_unless, cond));
                        }
                        ExecuteSubCmd::At { target } => {
                            ctx.pos = Some(self.get_pos(target));
                        }
                        ExecuteSubCmd::As { target } => {
                            ctx.ident = Some(self.get_ident(target));
                        }
                        ExecuteSubCmd::Store { is_success, kind } => {
                            assert!(store.is_none());
                            store = Some((*is_success, kind));
                        }
                        _ => todo!("{} ({:?})", cmd, subcmd),
                    }
                }

                if let Some(run) = run {
                    let do_run = cnd.into_iter().all(|(is_unless, cond)| self.check_cond(is_unless, cond));

                    if do_run {
                        let val = self.execute_cmd_ctx(run, &mut ctx)?;

                        if let Some((is_success, kind)) = store {
                            let val = val.unwrap();

                            if is_success {
                                todo!()
                            } else {
                                match kind {
                                    ExecuteStoreKind::Score { target, objective } => {
                                        match target {
                                            Target::Uuid(holder) => {
                                                self.scoreboard.set(holder, objective, val);
                                            }
                                            _ => todo!("{:?}", target)
                                        }
                                    }
                                    ExecuteStoreKind::Data { target, path, ty, scale } => {
                                        // The number in the file is *always* 1.0
                                        #[allow(clippy::float_cmp)]
                                        { assert_eq!(*scale, 1.0); }

                                        match target {
                                            DataTarget::Block(pos) => {
                                                assert!(ty == "int");

                                                let pos = pos.maybe_based(ctx.pos);

                                                self.set_block_data(pos, path, val)?;
                                            }
                                            DataTarget::Entity(target) => {
                                                assert!(ty == "double");

                                                let target = match target {
                                                    Target::Selector(Selector { var: SelectorVariable::ThisEntity, .. }) => {
                                                        ctx.ident.as_ref().unwrap()
                                                    }
                                                    _ => todo!("{:?}", target)
                                                };

                                                let pos = match target.as_str() {
                                                    "memoryptr" => &mut self.memory_ptr_pos,
                                                    "stackptr" => &mut self.stack_ptr_pos,
                                                    "frameptr" => &mut self.frame_ptr_pos,
                                                    "localptr" => &mut self.local_ptr_pos,
                                                    "globalptr" => &mut self.global_ptr_pos,
                                                    "turtle" => &mut self.turtle_pos,
                                                    _ => todo!("{:?}", target)
                                                };

                                                match path.as_str() {
                                                    "Pos[0]" => pos.0 = val,
                                                    "Pos[1]" => pos.1 = val,
                                                    "Pos[2]" => pos.2 = val,
                                                    _ => todo!("{:?}", path)
                                                }
                                            }

                                            _ => todo!("{:?}", target)
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // TODO: Determine if the return value passes through
                    // TODO: What happens if the condition is false?
                    Ok(None)
                    } else if let (Some((is_success, store_kind)), (is_unless, cond)) = (store, cnd[0]) {

                    assert!(cnd.len() == 1);
                    assert!(is_success);

                    let val = self.check_cond(is_unless, cond);

                    match store_kind {
                        ExecuteStoreKind::Score { target, objective } => {
                            match target {
                                Target::Uuid(holder) => {
                                    self.scoreboard.set(holder, objective, val as i32);
                                }
                                _ => todo!("{:?}", target),
                            }
                        }
                        _ => todo!("{:?}", store_kind)
                    }

                    // TODO: ???
                    Ok(None)
                } else {
                    todo!()
                }
            }

            /*
            cmd if cmd.to_string().starts_with("execute at @e[tag=ptr] run") => {
                let run = if let Command::Execute(Execute { run: Some(d), .. }) = cmd {
                    &**d
                } else {
                    unreachable!()
                };

                if let Command::Data(Data { target: DataTarget::Block(block), kind: DataKind::Modify { path, kind: DataModifyKind::Set, source: DataModifySource::Value(v) } }) = run {
                    let index = match block.as_str() {
                        "~ ~ ~" => {
                            get_index(self.ptr_pos.0, self.ptr_pos.1, self.ptr_pos.2)
                        }
                        "~-2 1 ~" => {
                            get_index(self.ptr_pos.0 - 2, 1, self.ptr_pos.2)
                        }
                        _ => todo!("{:?}", block)
                    };

                    if path != "RecordItem.tag.Memory" {
                        todo!("{:?}", path);
                    }

                    self.set_word(*v, index.unwrap() as usize)?;
                } else {
                    todo!("{:?}", cmd)
                }
            }
            cmd if cmd.to_string().starts_with("execute at @e[tag=ptr]") => {
                let (subcmds, run) = if let Command::Execute(Execute { run: Some(run), subcommands }) = cmd {
                    (subcommands, run)
                } else {
                    todo!("{:?}", cmd)
                };

                if subcmds.len() == 1 {
                    todo!("{}", cmd)
                }

                if let ExecuteSubCmd::Store { is_success: false, kind: ExecuteStoreKind::Score { target: Target::Uuid(target), objective } } = &subcmds[1] {
                    if run.to_string() == "data get block ~ ~ ~ RecordItem.tag.Memory 1" {
                        let index = get_index(self.ptr_pos.0, self.ptr_pos.1, self.ptr_pos.2)?;
                        let word = self.get_word(index as usize)?;
                        self.scoreboard.set(target, objective, word);
                    } else {
                        todo!()
                    }
                } else if subcmds[1].to_string() == "store result block ~ ~ ~ RecordItem.tag.Memory int 1" {
                    if let Command::ScoreGet(ScoreGet { target: Target::Uuid(target), target_obj }) = &**run {
                        let val = self.get_score(target, target_obj).unwrap();
                        let index = get_index(self.ptr_pos.0, self.ptr_pos.1, self.ptr_pos.2)?;
                        self.set_word(val, index as usize)?;
                    }
                } else {
                    todo!("{:?} {}", subcmds[1].to_string(), cmd)
                }
            }
            */
            /*
            Command::Execute(Execute { run: Some(run), subcommands }) => {
                if subcommands.iter().all(|s| matches!(s, ExecuteSubCmd::Condition { .. })) {
                    if subcommands.iter().all(|s| if let ExecuteSubCmd::Condition { is_unless, cond } = s {
                        self.check_cond(*is_unless, cond)
                    } else {
                        unreachable!()
                    }) {
                        self.execute_cmd_ctx(run, ctx)?;
                    }
                } else if cmd.to_string().starts_with("execute at @e[tag=turtle] run setblock ~ ~ ~") {
                    if let Command::SetBlock(SetBlock { pos: _, block, kind: _ }) = &**run {
                        eprintln!(
                            "Placed block at {} {} {}: {:?}",
                            self.turtle_pos.0,
                            self.turtle_pos.1,
                            self.turtle_pos.2,
                            block
                        );
                    } else {
                        unreachable!()
                    }
                } else if cmd.to_string().starts_with("execute if score %%cmdcount rust < %%CMD_LIMIT rust at @e[tag=next] run data modify block ~ ~ ~ Command set value ") {
                    if let Command::Data(Data { kind: DataKind::Modify { source: DataModifySource::ValueString(func), .. }, .. }) = &**run {
                        todo!("{:?}", func);
                    } else {
                        unreachable!()
                    }
                } else {
                    todo!("{}", cmd)
                }
            }
            Command::Execute(Execute { run: None, subcommands }) => {
                let store_target = if let ExecuteSubCmd::Store { is_success: true, kind: ExecuteStoreKind::Score { target: Target::Uuid(target), objective } } = &subcommands[0] {
                    (target, objective)
                } else {
                    todo!("{:?}", subcommands[0])
                };

                if subcommands[1..].iter().all(|sc| matches!(sc, ExecuteSubCmd::Condition { .. })) {
                    let mut result = true;
                    for subcmd in subcommands[1..].iter() {
                        if let ExecuteSubCmd::Condition { is_unless, cond } = subcmd {
                            result = result && self.check_cond(*is_unless, cond);
                        } else {
                            unreachable!()
                        }
                    }

                    self.scoreboard.set(store_target.0, store_target.1, result as i32);
                } else {
                    todo!()
                }
            }
            */
            Command::Comment(c) if c.starts_with("!INTERPRETER: SYNC ") => {
                //let c = c.strip_prefix("!INTERPRETER: SYNC ").unwrap();

                Err(InterpError::BreakpointHit)
            }
            Command::Comment(c) if c == "!INTERPRETER: TODO" => {
                Err(InterpError::EnteredTodo)
            }
            Command::Comment(c) if c == "!INTERPRETER: UNREACHABLE" => {
                Err(InterpError::EnteredUnreachable)
            }
            Command::Comment(c) if c.starts_with("!INTERPRETER: ASSERT ") => {
                let c = &c["!INTERPRETER: ASSERT ".len()..];

                let (c, is_unless) = if let Some(c) = c.strip_prefix("unless ") {
                    (c, true)
                } else if let Some(c) = c.strip_prefix("if ") {
                    (c, false)
                } else {
                    todo!()
                };

                let cond = ExecuteCondition::from_str(c).unwrap();

                if !self.check_cond(is_unless, &cond) {
                    eprintln!("Currently at:");
                    for (f, c) in self.call_stack.iter() {
                        eprintln!("{}, {}", self.program[*f].id, c);
                    }
                    return Err(InterpError::AssertionFailed);
                }

                Ok(None)
            }
            Command::Comment(_) => { Ok(None) }
            Command::Kill(_) => {
                // TODO:
                Ok(None)
            }
            Command::Summon(Summon { entity, pos, data }) => {
                let pos = pos.maybe_based(ctx.pos);

                if entity == "minecraft:armor_stand" {
                    if let Some(data) = data {

                        if data.contains("frameptr") {
                            self.frame_ptr_pos = pos;
                            Ok(None)
                        } else if data.contains("stackptr") {
                            self.stack_ptr_pos = pos;
                            Ok(None)
                        } else if data.contains("globalptr") {
                            self.global_ptr_pos = pos;
                            Ok(None)
                        } else if data.contains("localptr") {
                            self.local_ptr_pos = pos;
                            Ok(None)
                        } else if data.contains("memoryptr") {
                            self.memory_ptr_pos = pos;
                            Ok(None)
                        } else if data.contains("turtle") {
                            self.turtle_pos = pos;
                            Ok(None)
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }
            Command::ObjAdd(ObjAdd { obj, criteria }) => {
                if criteria == "dummy" {
                    self.scoreboard.0.insert(obj.clone(), HashMap::new());
                    Ok(None)
                } else {
                    todo!()
                }
            }
            Command::ObjRemove(obj) => {
                self.scoreboard.0.remove(&obj.0);
                Ok(None)
            }
            Command::Teleport(Teleport { target, pos }) => {
                let target = match target {
                    Target::Selector(Selector { var: SelectorVariable::ThisEntity, .. }) => {
                        ctx.ident.as_ref().unwrap()
                    }
                    _ => todo!("{:?}", target)
                };

                let pos = pos.maybe_based(ctx.pos);

                match target.as_str() {
                    "stackptr" => self.stack_ptr_pos = pos,
                    "memoryptr" => self.memory_ptr_pos = pos,
                    "localptr" => self.local_ptr_pos = pos,
                    "globalptr" => self.global_ptr_pos = pos,
                    "frameptr" => self.frame_ptr_pos = pos,
                    _ => todo!("{:?}", target)
                };

                Ok(None)
            }

            cmd => todo!("{}", cmd)
        }
    }

    pub fn get_top_func(&self) -> String {
        let (top_func_idx, _) = self.call_stack.first().unwrap();
        self.program[*top_func_idx].id.to_string()
    }

    pub fn step(&mut self) -> Result<(), InterpError> {
        if self.commands_run >= 60_000 {
            return Err(InterpError::MaxCommandsRun);
        }

        let (top_func_idx, _) = self.call_stack.first().unwrap();
        let top_func = self.program[*top_func_idx].id.to_string();

        let (func_idx, cmd_idx) = self.call_stack.last_mut().unwrap();

        //println!("Function {} at command {}", self.program[*func_idx].id, cmd_idx);

        let cmd = &self.program[*func_idx].cmds[*cmd_idx].clone();
        *cmd_idx += 1;

        let mut ctx = Context { pos: self.ctx_pos, ident: None };

        self.execute_cmd_ctx(cmd, &mut ctx)?;


        // TODO: This doesn't get incremented on breakpoints
        self.commands_run += 1;
        
        self.finish_unwind(top_func);

        Ok(())
    }

    pub fn finish_unwind(&mut self, top_func: String) {
        //let (top_func_idx, _) = self.call_stack.first().unwrap();
        //let top_func = self.program[*top_func_idx].id.to_string();

        loop {
            if self.call_stack.is_empty() {
                self.tick += 1;
                println!(
                    "Executed {} commands from function '{}'",
                    self.commands_run, top_func,
                );
                self.commands_run = 0;
                self.ctx_pos = None;
                if let Some(n) = self.next_pos.take() {
                    eprintln!("\nNow about to execute {}", &self.program[n.0].id);
                    self.call_stack.push((n.0, n.1));
                    self.ctx_pos = Some(n.2);

                    eprintln!("Stackptr at beginning of {} is {:?}", self.program[n.0].id, self.stack_ptr_pos);
                }
                break;
            }

            let (func_idx, cmd_idx) = self.call_stack.last().unwrap();

            if self.program[*func_idx].cmds.len() == *cmd_idx {
                self.call_stack.pop();
            } else {
                break;
            }
        }

    }

    pub fn halted(&self) -> bool {
        self.call_stack.last() == Some(&(0xFFFF_FFFF_FFFF_FFFF, 0)) || self.call_stack.is_empty()
    }
}
