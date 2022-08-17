use std::collections::HashMap;

use datapack_common::functions::{Command, command::{commands::*, ExecuteSubCommand, execute_sub_commands::{IfBlock, IfData, StoreStorage, Positioned, At, As}}, command_components::{ScoreOpKind, FunctionIdent, ScoreHolder, Objective, MinecraftRange, Target, ScoreboardComparison, ScoreboardTarget}, Function};

struct Arena<T> {
	values: Vec<T>,
	positions: HashMap<T, usize>,
}

impl<T> Arena<T> {
	pub fn new() -> Self {
		Arena { values: Default::default(), positions: Default::default() }
	}

	pub fn len(&self) -> usize {
		self.values.len()
	}
}

impl<T: std::clone::Clone + PartialEq + Eq + std::hash::Hash> Arena<T> {
	pub fn intern(&mut self, t: T) -> usize {
		if let Some(idx) = self.positions.get(&t) {
			*idx
		} else {
			self.values.push(t.clone());
			self.positions.insert(t, self.values.len() - 1);
			self.values.len() - 1
		}
	}
}

impl<T> Default for Arena<T> {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Default)]
pub struct ScoreArena(Arena<(ScoreHolder, Objective)>);

impl ScoreArena {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn intern(&mut self, h: ScoreHolder, o: Objective) -> ScoreId {
		ScoreId(self.0.intern((h, o)))
	}

	pub fn get(&self, h: ScoreHolder, o: Objective) -> Option<ScoreId> {
		let tuple = (h, o);
		self.0.positions.get(&tuple).map(|p| ScoreId(*p))
	}

	pub fn get_id(&self, score_id: ScoreId) -> &(ScoreHolder, Objective) {
		&self.0.values[score_id.0]
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScoreId(pub usize);

#[derive(std::clone::Clone)]
pub struct PExecuteRun {
	pub command: Option<Box<ParsedCommand>>,
}

#[derive(std::clone::Clone, Debug)]
pub struct PIfScoreMatches {
	pub is_unless: bool,
	pub target: ScoreId,
	pub range: MinecraftRange,
}

#[derive(std::clone::Clone)]
pub struct PIfScoreRelation {
	pub is_unless: bool,
	pub target: ScoreId,
	pub relation: ScoreboardComparison,
	pub source: ScoreId,
}

#[derive(std::clone::Clone)]
pub struct PStoreScore {
	pub is_success: bool,
	pub target: ScoreId,
}

#[derive(std::clone::Clone)]
pub enum PExecuteSubCmd {
	IfScoreMatches(PIfScoreMatches),
	IfScoreRelation(PIfScoreRelation),
	IfBlock(IfBlock),
	IfData(IfData),
	StoreScore(PStoreScore),
	StoreStorage(StoreStorage),
	As(As),
	At(At),
	Positioned(Positioned),
}

impl PExecuteSubCmd {
	pub fn parse(c: ExecuteSubCommand, sa: &mut ScoreArena) -> Self {
		match c {
			ExecuteSubCommand::IfScoreMatches(s) => {
				match s.target {
					Target::Name(n) => Self::IfScoreMatches(PIfScoreMatches { is_unless: s.is_unless, target: sa.intern(n, s.target_obj), range: s.range }),
					_ => todo!(),
				}
			}
			ExecuteSubCommand::IfScoreRelation(s) => {
				match (s.target, s.source) {
					(Target::Name(tn), Target::Name(sn)) => Self::IfScoreRelation(PIfScoreRelation {
						is_unless: s.is_unless, target: sa.intern(tn, s.target_obj), relation: s.relation, source: sa.intern(sn, s.source_obj), 
					}),
					_ => todo!(),
				}
			}
			ExecuteSubCommand::IfBlock(s) => Self::IfBlock(s),
			ExecuteSubCommand::IfData(s) => Self::IfData(s),
			ExecuteSubCommand::StoreScore(s) => {
				match s.target {
					Target::Name(n) => Self::StoreScore(PStoreScore { is_success: s.is_success, target: sa.intern(n, s.target_obj) }),
					_ => todo!(),
				}
			}
			ExecuteSubCommand::StoreStorage(s) => Self::StoreStorage(s),
			ExecuteSubCommand::As(s) => Self::As(s),
			ExecuteSubCommand::At(s) => Self::At(s),
			ExecuteSubCommand::Positioned(s) => Self::Positioned(s),
		}
	}
}

#[derive(std::clone::Clone)]
pub struct PExecute {
	pub subcommands: Vec<PExecuteSubCmd>,
	pub run: Option<Box<ParsedCommand>>,
}

#[derive(std::clone::Clone)]
pub struct PScoreAdd {
	pub target: ScoreId,
	pub remove: bool,
	pub score: i32,
}

#[derive(std::clone::Clone)]
pub struct PScoreGet {
	pub target: ScoreId,
}

#[derive(std::clone::Clone)]
pub struct PScoreOp {
	pub target: ScoreId,
	pub op: ScoreOpKind,
	pub source: ScoreId,
}

#[derive(std::clone::Clone)]
pub struct PScoreSet {
	pub target: ScoreId,
	pub score: i32,
}

#[derive(std::clone::Clone)]
pub struct PSchedule {
	pub id: usize,
	pub delay: i32,
	pub replace: bool,
}

#[derive(std::clone::Clone)]
pub enum ParsedCommand {
	Clone(Clone),
	Comment(Comment),
	DataGet(DataGet),
	DataModify(DataModify),
	Gamerule(Gamerule),
	ExecuteRun(PExecuteRun),
	Execute(PExecute),
	Fill(Fill),
	FuncCall(usize),
	Kill(Kill),
	ObjAdd(ObjAdd),
	ObjRemove(ObjRemove),
	Schedule(PSchedule),
	ScheduleClear(ScheduleClear),
	ScoreAdd(PScoreAdd),
	ScoreGet(PScoreGet),
	ScoreOp(PScoreOp),
	ScoreSet(PScoreSet),
	SetBlock(SetBlock),
	Summon(Summon),
	Teleport(Teleport),
	Tellraw(Tellraw),
}

impl ParsedCommand {
	pub fn parse(command: Command, sa: &mut ScoreArena, program: &[Function]) -> Self {
		match command {
			Command::Clone(c) => Self::Clone(c),
			Command::Comment(c) => Self::Comment(c),
			Command::DataGet(c) => Self::DataGet(c),
			Command::DataModify(c) => Self::DataModify(c),
			Command::Gamerule(c) => Self::Gamerule(c),
			Command::ExecuteRun(c) => Self::ExecuteRun(PExecuteRun { command: c.command.0.map(|c| Box::new(Self::parse(*c, sa, program))) }),
			Command::Execute(c) => {
				let subcommands = c.subcommands.0.into_iter().map(|s| PExecuteSubCmd::parse(s, sa)).collect();
				let run = c.run.0.map(|c| Box::new(Self::parse(*c, sa, program)));
				Self::Execute(PExecute{ subcommands, run })
			},
			Command::Fill(c) => Self::Fill(c),
			Command::FuncCall(c) => {
				let idx = program.iter().enumerate().find(|(_, f)| f.id == c.id).unwrap_or_else(|| panic!("{:?}", c)).0;
				Self::FuncCall(idx)
			}
			Command::Kill(c) => Self::Kill(c),
			Command::ObjAdd(c) => Self::ObjAdd(c),
			Command::ObjRemove(c) => Self::ObjRemove(c),
			Command::Schedule(s) => {
				let idx = program.iter().enumerate().find(|(_, f)| f.id == s.id).unwrap().0;
				Self::Schedule(PSchedule { id: idx, delay: s.delay, replace: s.replace })
			},
			Command::ScheduleClear(c) => Self::ScheduleClear(c),
			Command::ScoreAdd(c) => {
				match c.target {
					ScoreboardTarget::Target(Target::Name(n)) => {
						Self::ScoreAdd(PScoreAdd { target: sa.intern(n, c.target_obj), remove: c.remove, score: c.score })
					}
					_ => todo!(),
				}
			}
			Command::ScoreGet(c) => {
				match c.target {
					ScoreboardTarget::Target(Target::Name(n)) => {
						Self::ScoreGet(PScoreGet { target: sa.intern(n, c.target_obj) })
					}
					_ => todo!(),
				}
			},
			Command::ScoreOp(c) => {
				match (c.target, c.source) {
					(ScoreboardTarget::Target(Target::Name(tn)), ScoreboardTarget::Target(Target::Name(sn))) => {
						Self::ScoreOp(PScoreOp { target: sa.intern(tn, c.target_obj), op: c.op, source: sa.intern(sn, c.source_obj) })
					}
					_ => todo!(),
				}
			}
			Command::ScoreSet(c) => {
				match c.target {
					ScoreboardTarget::Target(Target::Name(n)) => {
						Self::ScoreSet(PScoreSet { target: sa.intern(n, c.target_obj), score: c.score })
					}
					_ => todo!(),
				}
			},
			Command::SetBlock(c) => Self::SetBlock(c),
			Command::Summon(c) => Self::Summon(c),
			Command::Teleport(c) => Self::Teleport(c),
			Command::Tellraw(c) => Self::Tellraw(c),
		}
	}
}

pub struct ParsedFunction {
	pub id: FunctionIdent,
	pub cmds: Vec<ParsedCommand>,
}

impl ParsedFunction {
	pub fn parse(func: Function, sa: &mut ScoreArena, program: &[Function]) -> Self {
		let cmds = func.cmds.into_iter().map(|c| ParsedCommand::parse(c, sa, program)).collect();
		ParsedFunction { id: func.id, cmds }
	}
}