use datapack_common::functions::command::*;
use datapack_common::functions::raw_text::TextComponent;
use datapack_common::functions::*;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpError {
    OutOfBoundsAccess(i32, i32, i32),
    MaxCommandsRun,
    EnteredUnreachable,
    EnteredTodo,
    AssertionFailed,
    BreakpointHit,
    SyncHit(usize, usize),
    InvalidBranch(usize),
    MultiBranch(FunctionIdent, Option<FunctionIdent>),
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
            InterpError::SyncHit(f_idx, i_idx) => write!(f, "sync hit at {} {}", f_idx, i_idx),
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
pub struct Scoreboard(pub HashMap<Objective, HashMap<ScoreHolder, i32>>);

impl Scoreboard {
    pub fn get(&self, holder: &ScoreHolder, obj: &Objective) -> Option<i32> {
        let scores = self.0.get(obj)?;
        scores.get(holder).copied()
    }

    pub fn set(&mut self, holder: &ScoreHolder, obj: &Objective, value: i32) {
        let scores = self.0.get_mut(obj).unwrap();
        scores.insert(holder.to_owned(), value);

    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CmdBlockKind {
    Impulse,
    Chain,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Facing {
    North,
    South,
    East,
    West,
}

impl FromStr for Facing {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "north" => Ok(Facing::North),
            "south" => Ok(Facing::South),
            "east" => Ok(Facing::East),
            "west" => Ok(Facing::West),
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CommandBlock {
    kind: CmdBlockKind,
    facing: Facing,
    command: String,
    conditional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Block {
    Command(CommandBlock),
    Redstone,
    Jukebox(i32),
    Other(String),
}

impl TryFrom<&BlockSpec> for Block {
    type Error = String;

    fn try_from(block: &BlockSpec) -> Result<Self, Self::Error> {
        match block.id.as_str() {
            "minecraft:redstone_block" => Ok(Block::Redstone),
            "minecraft:jukebox" => {
                assert!(block.state.is_empty());

                let idx = block.nbt.find("Memory:").unwrap();
                let rest = &block.nbt[idx + "Memory:".len()..];
                let end_idx = rest.find('}').unwrap();

                let val = rest[..end_idx].parse().unwrap();
                Ok(Block::Jukebox(val))
            }
            "minecraft:command_block" => {
                let conditional = block.state.get("conditional").unwrap_or("false").parse::<bool>().unwrap();
                let facing = block.state.get("facing").unwrap_or("north").parse::<Facing>().unwrap();

                let command = if let Some(cmd_start) = block.nbt.find("{Command:\"") {
                    let start = cmd_start + "{Command:\"".len();
                    let end = block.nbt.find("\"}").unwrap();
                    block.nbt[start..end].to_string()
                } else {
                    String::new()
                };

                Ok(Block::Command(CommandBlock {
                    kind: CmdBlockKind::Impulse,
                    conditional,
                    command,
                    facing,
                }))
            }
            "minecraft:chain_command_block" => {
                let conditional = block.state.get("conditional").unwrap_or("false").parse::<bool>().unwrap();
                let facing = block.state.get("facing").unwrap_or("north").parse::<Facing>().unwrap();

                let command = if let Some(cmd_start) = block.nbt.find("{Command:\"") {
                    let start = cmd_start + "{Command:\"".len();
                    let end = block.nbt.find("\"}").unwrap();
                    block.nbt[start..end].to_string()
                } else {
                    String::new()
                };

                Ok(Block::Command(CommandBlock {
                    kind: CmdBlockKind::Chain,
                    conditional,
                    command,
                    facing,
                }))
            }
            id => Ok(Block::Other(id.to_string()))
        }

    }
}

#[derive(Debug, Clone, Default)]
/// Represents the state of 
struct Context {
    pub pos: Option<(i32, i32, i32)>,
    pub ident: Option<String>,
}

pub struct Interpreter {
    pub program: Vec<Function>,

    pub scoreboard: Scoreboard,

    call_stack: Vec<(usize, usize)>,
    ctx_pos: Option<(i32, i32, i32)>,


    blocks: HashMap<(i32, i32, i32), Block>,

    memory_ptr_pos: (i32, i32, i32),
    local_ptr_pos: (i32, i32, i32),
    global_ptr_pos: (i32, i32, i32),
    frame_ptr_pos: (i32, i32, i32),
    stack_ptr_pos: (i32, i32, i32),
    cond_stack_ptr_pos: (i32, i32, i32),

    turtle_pos: (i32, i32, i32),
    next_chain_pos: (i32, i32, i32),

    next_pos: Option<(usize, usize, (i32, i32, i32))>,

    pub output: Vec<String>,
    pub tick: usize,

    pub last_commands_run: usize,
    commands_run: usize,
}

impl Interpreter {
    pub fn set_pos(&mut self, func_idx: usize) {
        self.call_stack = vec![(func_idx, 0)];
        self.ctx_pos = None;
    }

    pub fn new(program: Vec<Function>, func_idx: usize) -> Self {
        Interpreter {
            program,
            ctx_pos: None,
            call_stack: vec![(func_idx, 0)],
            blocks: HashMap::new(),
            memory_ptr_pos: Default::default(),
            frame_ptr_pos: Default::default(),
            local_ptr_pos: Default::default(),
            stack_ptr_pos: Default::default(),
            cond_stack_ptr_pos: Default::default(),
            global_ptr_pos: Default::default(),
            scoreboard: Scoreboard::default(),
            turtle_pos: (0, 0, 0),
            next_chain_pos: (0, 0, 0),
            next_pos: None,
            last_commands_run: 0,
            commands_run: 0,
            tick: 0,
            output: Vec::new(),
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

    fn set_next_pos(&mut self, func_idx: usize, pos: (i32, i32, i32)) -> Result<(), InterpError> {
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

    fn eval_message(&self, msg: &[TextComponent]) -> String {
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

    fn check_cond(&self, is_unless: bool, cond: &ExecuteCondition) -> bool {
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
                            ScoreboardComparison::Less => target < source,
                            ScoreboardComparison::LessOrEqual => target <= source,
                            ScoreboardComparison::Equal => target == source,
                            ScoreboardComparison::Greater => target > source,
                            ScoreboardComparison::GreaterOrEqual => target >= source,
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

    fn set_block(&mut self, pos: (i32, i32, i32), block: &BlockSpec, mode: SetBlockKind) {
        if block.id == "minecraft:air" {
            self.blocks.remove(&pos);
        } else {
            println!("{:?}", pos);

            let block = Block::try_from(block).unwrap();

            if block == Block::Redstone && (self.blocks.get(&pos) != Some(&Block::Redstone) || mode == SetBlockKind::Destroy) {
                let cmd = (pos.0, pos.1 - 1, pos.2);
                println!("Checking for command block at {:?}", cmd);
                let b = self.blocks.get(&cmd).unwrap_or_else(|| panic!("{:?}", cmd));
                if let Block::Command(CommandBlock { kind: CmdBlockKind::Impulse, command, .. }) = b {
                    let c = command.parse::<Command>().unwrap();
                    let id = if let Command::FuncCall(FuncCall { id }) = c {
                        id
                    } else {
                        todo!("{}", c)
                    };

                    let idx = self.get_func_idx(&id);


                    self.set_next_pos(idx, cmd).unwrap();
                }
            }

            self.blocks.insert(pos, block);
        }
    }

    /// `pos` is the position of the original command block
    /// returns true if execution should continue
    fn trigger_chain(&mut self, pos: (i32, i32, i32)) -> bool {
        let facing = if let Some(Block::Command(CommandBlock { facing, .. })) = self.blocks.get(&pos) {
            *facing
        } else {
            panic!()
        };

        let next_pos = match facing {
            Facing::East => (pos.0 + 1, pos.1, pos.2),
            Facing::West => (pos.0 - 1, pos.1, pos.2),
            Facing::South => (pos.0, pos.1, pos.2 + 1),
            Facing::North => (pos.0, pos.1, pos.2 - 1),
        };

        self.trigger_at(next_pos)
    }

    /// returns true if execution should continue
    fn trigger_at(&mut self, pos: (i32, i32, i32)) -> bool {
        if let Some(Block::Command(CommandBlock { kind: CmdBlockKind::Chain, command, .. })) = self.blocks.get(&pos) {
            if !command.is_empty() {
                let c = command.parse::<Command>().unwrap();
                let id = if let Command::FuncCall(FuncCall { id }) = c {
                    id
                } else {
                    todo!("{}", c)
                };

                let idx = self.get_func_idx(&id);

                assert!(self.call_stack.is_empty());

                self.call_stack.push((idx, 0));
                self.ctx_pos = Some(pos);

                eprintln!("Stackptr at beginning of {} [chained at {:?}] is {:?}", self.program[idx].id, pos, self.stack_ptr_pos);

                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn get_func_idx(&self, id: &FunctionIdent) -> usize {
        let mut idx = None;
        for (i, f) in self.program.iter().enumerate() {
            if &f.id == id {
                idx = Some(i);
                break;
            }
        }
        idx.unwrap()
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
                    "@e[tag=condstackptr]" => self.cond_stack_ptr_pos,
                    "@e[tag=turtle]" => self.turtle_pos,
                    "@e[tag=nextchain]" => self.next_chain_pos,
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
                    "@e[tag=condstackptr]" => "condstackptr",
                    "@e[tag=turtle]" => "turtle",
                    "@e[tag=nextchain]" => "nextchain",
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

    fn set_block_data_str(&mut self, pos: (i32, i32, i32), path: &str, value: &str) -> Result<(), InterpError> {
        match self.blocks.get_mut(&pos) {
            Some(Block::Command(c)) => {
                if path == "Command" {
                    println!("Setting at {:?}", pos);
                    c.command = value.to_string();
                    Ok(())
                } else {
                    Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string()))
                }
            }
            _ => Err(InterpError::NoBlockData(pos.0, pos.1, pos.2, path.to_string())),
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

    pub fn execute_cmd(&mut self, cmd: &Command) -> Result<Option<i32>, InterpError> {
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
            Command::Gamerule(Gamerule { rule, value, }) => {
                // TODO:
                match rule.as_str() {
                    "maxCommandChainLength" => {
                        assert!(value.is_none());

                        Ok(Some(600_000))
                    }
                    _ => todo!("{} {:?}", rule, value)
                }
            }
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
                                val = val.wrapping_sub(rhs);
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::MulAssign => {
                                let mut val = self.get_score(target, target_obj).unwrap();
                                val = val.wrapping_mul(rhs);
                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::DivAssign => {
                                let lhs = self.get_score(target, target_obj).unwrap();

                                // Minecraft div rounds towards -infinity
                                let val = if (lhs >= 0) == (rhs >= 0) {
                                    lhs.wrapping_div(rhs)
                                } else {
                                    let nat_div = lhs / rhs;
                                    if lhs != nat_div * rhs {
                                        nat_div - 1
                                    } else {
                                        nat_div
                                    }
                                };

                                self.scoreboard.set(target, target_obj, val);
                            }
                            ScoreOpKind::ModAssign => {
                                let lhs = self.get_score(target, target_obj).unwrap();

                                let quot = if (lhs >= 0) == (rhs >= 0) {
                                    lhs.wrapping_div(rhs)
                                } else {
                                    let nat_div = lhs / rhs;
                                    if lhs != nat_div * rhs {
                                        nat_div - 1
                                    } else {
                                        nat_div
                                    }
                                };

                                let rem = lhs.wrapping_sub(quot.wrapping_mul(rhs));

                                self.scoreboard.set(target, target_obj, rem);
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
                //if id"stdout:putc" {
                //    todo!();
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
                //} else {
                let called_idx = self.program.iter().enumerate().find(|(_, f)| &f.id == id).unwrap_or_else(|| todo!("{:?}", id)).0;
                self.call_stack.push((called_idx, 0));
                //}

                Ok(None)
            }
            Command::CloneCmd(CloneCmd { start, end, dest }) => {
                let start = start.maybe_based(ctx.pos);
                let end = end.maybe_based(ctx.pos);
                let dest = dest.maybe_based(ctx.pos);

                println!("{:?} {:?} {:?}", start, end, dest);


                for (sx, dx) in (start.0..=end.0).zip(dest.0..) {
                    for (sy, dy) in (start.1..=end.1).zip(dest.1..) {
                        for (sz, dz) in (start.2..=end.2).zip(dest.2..) {
                            assert!(!(
                                (start.0..=end.0).contains(&dx) &&
                                (start.1..=end.1).contains(&dy) &&
                                (start.2..=end.2).contains(&dz)
                            ));

                            // TODO: This should use the set_block function
                            let block = self.blocks.get(&(sx, sy, sz)).cloned();
                            if let Some(block) = block {
                                self.blocks.insert((dx, dy, dz), block);
                            } else {
                                self.blocks.remove(&(dx, dy, dz));
                            }
                        }
                    }
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
                        } else if let DataModifySource::ValueString(v) = source {
                            self.set_block_data_str(pos, path, v)?;
                            Ok(None)
                        } else {
                            todo!("{:?}", source)
                        }
                    }
                    (DataTarget::Block(block), DataKind::Get { path, scale }) => {
                        let pos = block.maybe_based(ctx.pos);

                        // The number in the file is *always* 1.0
                        #[allow(clippy::float_cmp)]
                        { assert_eq!(*scale, 1.0); }

                        //println!("Getting block data at {:?}", pos);

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
                                                    "condstackptr" => &mut self.cond_stack_ptr_pos,
                                                    "frameptr" => &mut self.frame_ptr_pos,
                                                    "localptr" => &mut self.local_ptr_pos,
                                                    "globalptr" => &mut self.global_ptr_pos,
                                                    "turtle" => &mut self.turtle_pos,
                                                    "nextchain" => &mut self.next_chain_pos,
                                                    _ => todo!("{:?}", target)
                                                };

                                                match path.as_str() {
                                                    "Pos[0]" => pos.0 = val,
                                                    "Pos[1]" => pos.1 = val,
                                                    "Pos[2]" => pos.2 = val,
                                                    _ => todo!("{:?}", path)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // TODO: Determine if the return value passes through
                    // TODO: What happens if the condition is false?
                    Ok(None)
                } else if let Some((is_success, store_kind)) = store {

                    assert!(!cnd.is_empty());

                    assert!(is_success);

                    let mut val = true;

                    for (is_unless, cond) in cnd.iter() {
                        val &= self.check_cond(*is_unless, cond);
                    }

                    match store_kind {
                        ExecuteStoreKind::Score { target, objective } => {
                            match target {
                                Target::Uuid(holder) => {
                                    println!("Storing into {}", holder);
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
                let c = c.strip_prefix("!INTERPRETER: SYNC ").unwrap();
                let (f, i) = c.split_once(' ').unwrap();
                let f = f.parse::<usize>().unwrap();
                let i = i.parse::<usize>().unwrap();

                Err(InterpError::SyncHit(f, i))
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
                        } else if data.contains("condstackptr") {
                            self.cond_stack_ptr_pos = pos;
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
                        } else if data.contains("nextchain") {
                            self.next_chain_pos = pos;
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
                if criteria == &ObjectiveCriterion::Dummy {
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
                    "condstackptr" => self.cond_stack_ptr_pos = pos,
                    "memoryptr" => self.memory_ptr_pos = pos,
                    "localptr" => self.local_ptr_pos = pos,
                    "globalptr" => self.global_ptr_pos = pos,
                    "frameptr" => self.frame_ptr_pos = pos,
                    "nextchain" => self.next_chain_pos = pos,
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

    /// Executes the next command
    pub fn step(&mut self) -> Result<(), InterpError> {
        if self.commands_run >= 600_000 {
            return Err(InterpError::MaxCommandsRun);
        }

        let (top_func_idx, _) = self.call_stack.first().unwrap();
        let top_func = self.program[*top_func_idx].id.to_string();

        let (func_idx, cmd_idx) = self.call_stack.last_mut().unwrap();

        //println!("Function {} at command {}", self.program[*func_idx].id, cmd_idx);

        let cmd = &self.program[*func_idx].cmds[*cmd_idx].clone();
        *cmd_idx += 1;

        let mut ctx = Context { pos: self.ctx_pos, ident: None };

        // TODO: This doesn't get incremented on breakpoints
        self.commands_run += 1;

        self.execute_cmd_ctx(cmd, &mut ctx)?;

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

                if let Some(ctx_pos) = self.ctx_pos {
                    println!("Pos was {:?}", ctx_pos);
                    if self.trigger_chain(ctx_pos) {
                        break;
                    }
                }

                self.last_commands_run = self.commands_run;
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
        self.call_stack.is_empty()
    }
}
