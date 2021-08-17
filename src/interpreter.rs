use datapack_common::functions::command::*;
use datapack_common::functions::raw_text::TextComponent;
use datapack_common::functions::*;
use datapack_common::functions::command_components::*;
use datapack_common::functions::command::execute_sub_commands::*;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;

fn get_name(target: &ScoreboardTarget) -> Option<&ScoreHolder> {
    match target {
        ScoreboardTarget::Target(target) => get_target_name(target),
        ScoreboardTarget::Asterisk => None,
    }
}

fn get_target_name(target: &Target) -> Option<&ScoreHolder> {
    match target {
        Target::Name(name) => Some(name),
        Target::Selector(_) => None
    }
}

fn maybe_based(pos: &RelBlockPos, base: Option<(i32, i32, i32)>) -> (i32, i32, i32) {
    match pos {
        RelBlockPos {
            x: Coord { value: x, kind: CoordKind::Absolute },
            y: Coord { value: y, kind: CoordKind::Absolute },
            z: Coord { value: z, kind: CoordKind::Absolute },
        } => {
            (*x, *y, *z)
        }
        RelBlockPos { x, y, z } => {
            let base = base.unwrap();
            let x = calc_coord(x, base.0);
            let y = calc_coord(y, base.1);
            let z = calc_coord(z, base.2);
            (x, y, z)
        }
    }
}

fn calc_coord(coord: &Coord, base: i32) -> i32 {
    match coord.kind {
        CoordKind::Absolute => coord.value,
        CoordKind::Relative => coord.value + base,
        CoordKind::Local => todo!(),
    }
}

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
        match block.id.as_ref() {
            "minecraft:redstone_block" => Ok(Block::Redstone),
            "minecraft:jukebox" => {
                assert!(block.state.is_empty());

                let nbt = block.nbt.to_string();

                let idx = nbt.find("Memory:").unwrap();
                let rest = &nbt[idx + "Memory:".len()..];
                let end_idx = rest.find('}').unwrap();

                let val = rest[..end_idx].parse().unwrap();
                Ok(Block::Jukebox(val))
            }
            "minecraft:command_block" => {
                let conditional = block.state.get("conditional").unwrap_or("false").parse::<bool>().unwrap();
                let facing = block.state.get("facing").unwrap_or("north").parse::<Facing>().unwrap();

                let nbt = block.nbt.to_string();

                let command = if let Some(cmd_start) = nbt.find("{Command:\"") {
                    let start = cmd_start + "{Command:\"".len();
                    let end = nbt.find("\"}").unwrap();
                    nbt[start..end].to_string()
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

                let nbt = block.nbt.to_string();

                let command = if let Some(cmd_start) = nbt.find("{Command:\"") {
                    let start = cmd_start + "{Command:\"".len();
                    let end = nbt.find("\"}").unwrap();
                    nbt[start..end].to_string()
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

    pub fn new_at_id(program: Vec<Function>, start_func_id: &FunctionIdent) -> Self {
        let start_func_idx = program
            .iter()
            .enumerate()
            .find(|(_, f)| &f.id == start_func_id )
            .unwrap()
            .0;
        
        Self::new(program, start_func_idx)
    }

    pub fn new(program: Vec<Function>, start_func_idx: usize) -> Self {
        Interpreter {
            program,
            ctx_pos: None,
            call_stack: vec![(start_func_idx, 0)],
            blocks: HashMap::new(),
            memory_ptr_pos: Default::default(),
            frame_ptr_pos: Default::default(),
            local_ptr_pos: Default::default(),
            stack_ptr_pos: Default::default(),
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

    fn check_cond(&self, subcmd: &ExecuteSubCommand) -> bool {
        match subcmd {
            ExecuteSubCommand::IfScoreMatches(cond) => {
                self.check_if_score_matches(cond)
            }
            ExecuteSubCommand::IfScoreRelation(cond) => {
                self.check_if_score_relation(cond)
            }
            ExecuteSubCommand::IfBlock(cond) => {
                self.check_if_block(cond)
            }
            _ => todo!(),
        }
    }

    fn check_if_score_matches(&self, cond: &IfScoreMatches) -> bool {
        let target = get_target_name(&cond.target).unwrap();

        let score = self.get_score(target, &cond.target_obj).unwrap();

        let result = cond.range.contains(score);

        if cond.is_unless { !result } else { result }
    }

    fn check_if_score_relation(&self, cond: &IfScoreRelation) -> bool {
        let lhs = self.get_score(get_target_name(&cond.target).unwrap(), &cond.target_obj).unwrap();
        let rhs = self.get_score(get_target_name(&cond.source).unwrap(), &cond.source_obj).unwrap();

        let result = cond.relation.evaluate(lhs, rhs);

        if cond.is_unless { !result } else { result }
    }

    fn check_if_block(&self, _cond: &IfBlock) -> bool {
        todo!()
    }

    fn set_block(&mut self, pos: (i32, i32, i32), block: &BlockSpec, mode: SetBlockKind) {
        use datapack_common::functions::command::commands::FuncCall;

        if block.id.as_ref() == "minecraft:air" {
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
        use datapack_common::functions::command::commands::FuncCall;

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

    fn set_block_data(&mut self, pos: (i32, i32, i32), path: &str, value: &SNbt) -> Result<(), InterpError> {
        let value = value.to_string();

        if let Ok(value) = value.parse::<i32>() {
            self.set_block_data_int(pos, path, value)
        } else if let Some(s) = value.strip_prefix('"') {
            let s = s.strip_suffix('"').unwrap();

            self.set_block_data_str(pos, path, s)
        } else {
            todo!("{:?}", value);
        }
    }

    fn set_block_data_int(&mut self, pos: (i32, i32, i32), path: &str, value: i32) -> Result<(), InterpError> {
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
        use datapack_common::functions::command::commands::*;

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
                        assert!(value.0.is_none());

                        Ok(Some(600_000))
                    }
                    _ => todo!("{} {:?}", rule, value)
                }
            }
            Command::ScoreAdd(ScoreAdd { target, target_obj, score, remove }) => {
                let target = get_name(target).unwrap();

                let mut lhs = self.get_score(target, target_obj).unwrap();

                if *remove {
                    lhs = lhs.wrapping_sub(*score);
                } else {
                    lhs = lhs.wrapping_add(*score);
                }

                self.scoreboard.set(target, target_obj, lhs);

                Ok(None)
            }
            Command::ScoreOp(ScoreOp { target, target_obj, op, source, source_obj }) => {
                let target = get_name(target).unwrap();
                let source = get_name(source).unwrap();

                let rhs = self.get_score(source, source_obj).unwrap();

                match op {
                    ScoreOpKind::Assign => {
                        self.scoreboard.set(target, target_obj, rhs);
                    }
                    ScoreOpKind::Add => {
                        let mut val = self.get_score(target, target_obj).unwrap();
                        val = val.wrapping_add(rhs);
                        self.scoreboard.set(target, target_obj, val);
                    }
                    ScoreOpKind::Sub => {
                        let mut val = self.get_score(target, target_obj).unwrap();
                        val = val.wrapping_sub(rhs);
                        self.scoreboard.set(target, target_obj, val);
                    }
                    ScoreOpKind::Mul => {
                        let mut val = self.get_score(target, target_obj).unwrap();
                        val = val.wrapping_mul(rhs);
                        self.scoreboard.set(target, target_obj, val);
                    }
                    ScoreOpKind::Div => {
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
                    ScoreOpKind::Mod => {
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
                    ScoreOpKind::Max => {
                        let mut val = self.get_score(target, target_obj).unwrap();
                        val = val.max(rhs);
                        self.scoreboard.set(target, target_obj, val);
                    }
                    ScoreOpKind::Swap => {
                        let lhs = self.get_score(target, target_obj).unwrap();

                        self.scoreboard.set(source, source_obj, lhs);
                        self.scoreboard.set(target, target_obj, rhs);
                    }
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
            Command::Clone(Clone { start, end, dest }) => {
                let start = maybe_based(start, ctx.pos);
                let end = maybe_based(end, ctx.pos);
                let dest = maybe_based(dest, ctx.pos);

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
            Command::Fill(Fill { start, end, block, place_kind }) => {
                let start = maybe_based(start, ctx.pos);
                let end = maybe_based(end, ctx.pos);

                assert!(start.0 <= end.0);
                assert!(start.1 <= end.1);
                assert!(start.2 <= end.2);

                let kind = match place_kind {
                    FillBlockKind::Destroy => SetBlockKind::Destroy,
                    FillBlockKind::Replace => SetBlockKind::Replace,
                    _ => todo!("{:?}", place_kind),
                };

                for x in start.0..=end.0 {
                    for y in start.1..=end.1 {
                        for z in start.2..=end.2 {
                            // TODO: Kind??
                            self.set_block((x, y, z), block, kind)
                        }
                    }
                } 

                Ok(None)
            }
            Command::DataGet(DataGet { target, path, scale }) => {
                match target {
                    DataTarget::Block(block) => {
                        let pos = maybe_based(block, ctx.pos);

                        // The number in the file is *always* 1.0
                        #[allow(clippy::float_cmp)]
                        { assert!(scale.0 == None || scale.0 == Some(1.0)); }

                        Ok(Some(self.get_block_data(pos, path)?))
                    }
                    _ => todo!("{:?}", target)
                }
            }
            Command::DataModify(DataModify { target, path, kind }) => {
                use datapack_common::functions::command::data_modify_kinds::Set;

                match target {
                    DataTarget::Block(block) => {
                        let pos = maybe_based(block, ctx.pos);

                        if let DataModifyKind::Set(Set { value }) = kind {
                            self.set_block_data(pos, path, value)?;
                            Ok(None)
                        } else {
                            todo!("{:?}", kind)
                        }
                    }
                    _ => todo!(),
                }
            }
            Command::ScoreSet(ScoreSet { target, target_obj, score }) => {
                let target = get_name(target).unwrap();

                self.scoreboard.set(target, target_obj, *score);

                Ok(None)
            }
            Command::ScoreGet(ScoreGet { target, target_obj }) => {
                let target = get_name(target).unwrap();

                Ok(Some(self.scoreboard.get(target, target_obj).unwrap_or_else(|| panic!("{:?} {:?}", target, target_obj))))
            }
            Command::Tellraw(Tellraw { message, target: _target }) => {
                let msg = self.eval_message(&message.components);
                println!("\n{}\n", msg);
                self.output.push(msg);

                Ok(None)
            }
            Command::SetBlock(SetBlock { pos, block, kind }) => {
                let pos = maybe_based(pos, ctx.pos);

                self.set_block(pos, block, *kind);

                Ok(None)
            }
            Command::Execute(Execute { subcommands, run }) => {
                let mut ctx = ctx.clone();

                let mut store = None;

                let mut cnd = Vec::new();

                for subcmd in subcommands.0.iter() {
                    match subcmd {
                        // TODO: DETERMINE HOW THIS INTERACTS WITH OTHER SELECTORS
                        ExecuteSubCommand::IfScoreMatches { .. } |
                        ExecuteSubCommand::IfScoreRelation { .. } |
                        ExecuteSubCommand::IfBlock { .. } => {
                            cnd.push(subcmd);
                        }
                        ExecuteSubCommand::At(At { target }) => {
                            ctx.pos = Some(self.get_pos(target));
                        }
                        ExecuteSubCommand::As(As { target }) => {
                            ctx.ident = Some(self.get_ident(target));
                        }
                        ExecuteSubCommand::StoreScore(..) |
                        ExecuteSubCommand::StoreStorage(..) => {
                            assert!(store.is_none());
                            store = Some(subcmd);
                        }
                        _ => todo!("{} ({:?})", cmd, subcmd),
                    }
                }

                if let Some(run) = run.0.as_deref() {
                    let do_run = cnd.into_iter().all(|cond| self.check_cond(cond));

                    if do_run {
                        let val = self.execute_cmd_ctx(run, &mut ctx)?;

                        if let Some(store) = store {
                            let val = val.unwrap();

                            match store {
                                ExecuteSubCommand::StoreScore(StoreScore { is_success, target, target_obj, }) => {
                                    if *is_success {
                                        todo!()
                                    }

                                    let target = get_target_name(target).unwrap();

                                    self.scoreboard.set(target, target_obj, val);
                                }
                                ExecuteSubCommand::StoreStorage(StoreStorage { is_success, target, path, ty, scale }) => {
                                    if *is_success {
                                        todo!()
                                    }

                                    // The number in the file is *always* 1.0
                                    #[allow(clippy::float_cmp)]
                                    { assert_eq!(*scale, 1.0); }

                                    match target {
                                        DataTarget::Block(pos) => {
                                            assert!(ty == "int");

                                            let pos = maybe_based(pos, ctx.pos);

                                            self.set_block_data_int(pos, path, val)?;
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
                                _ => unreachable!(),
                            }
                        }
                    }

                    // TODO: Determine if the return value passes through
                    // TODO: What happens if the condition is false?
                    Ok(None)
                } else if let Some(store) = store {
                    assert!(!cnd.is_empty());

                    let mut val = true;

                    for cond in cnd.iter() {
                        val &= self.check_cond(cond);
                    }

                    match store {
                        ExecuteSubCommand::StoreScore(StoreScore { is_success, target, target_obj }) => {
                            if !is_success {
                                todo!()
                            }

                            let target = get_target_name(target).unwrap();

                            println!("Storing into {}", target);
                            self.scoreboard.set(target, target_obj, val as i32);
                        }
                        _ => todo!("{:?}", store)
                    }

                    // TODO: ???
                    Ok(None)
                } else {
                    todo!()
                }
            }
            Command::Comment(c) => {
                let c = c.to_string();

                if let Some(c) = c.strip_prefix("# !INTERPRETER: SYNC ") {
                    let (f, i) = c.split_once(' ').unwrap();
                    let f = f.parse::<usize>().unwrap();
                    let i = i.parse::<usize>().unwrap();

                    Err(InterpError::SyncHit(f, i))
                } else if c == "# !INTERPRETER: TODO" {
                    Err(InterpError::EnteredTodo)
                } else if c == "# !INTERPRETER: UNREACHABLE" {
                    Err(InterpError::EnteredUnreachable)
                } else if let Some(cond) = c.strip_prefix("# !INTERPRETER: ASSERT ") {
                    /*let (c, is_unless) = if let Some(c) = cond.strip_prefix("unless ") {
                        (c, true)
                    } else if let Some(c) = c.strip_prefix("if ") {
                        (c, false)
                    } else {
                        todo!()
                    };

                    let cond = ExecuteSubCommand::from_str(c).unwrap();

                    if !self.check_cond(&cond) {
                        eprintln!("Currently at:");
                        for (f, c) in self.call_stack.iter() {
                            eprintln!("{}, {}", self.program[*f].id, c);
                        }
                        return Err(InterpError::AssertionFailed);
                    }

                    Ok(None)*/
                    todo!("{:?}", cond)
                } else if c.contains("!INTERPRETER") {
                    todo!("{}", c)
                } else {
                    Ok(None)
                }
            }
            Command::Kill(_) => {
                // TODO:
                Ok(None)
            }
            Command::Summon(Summon { entity, pos, data }) => {
                let pos = maybe_based(pos, ctx.pos);

                if entity == "minecraft:armor_stand" {
                    if let Some(data) = &data.0 {
                        let data = data.to_string();

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
            Command::ObjAdd(ObjAdd(obj, crit)) => {
                if crit == &ObjectiveCriterion::Dummy {
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

                let pos = maybe_based(pos, ctx.pos);

                match target.as_str() {
                    "stackptr" => self.stack_ptr_pos = pos,
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
