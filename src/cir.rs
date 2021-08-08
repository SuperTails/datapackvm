pub use raw_text::*;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::fmt;
use std::ops::{RangeFrom, RangeInclusive, RangeToInclusive};
use std::str::FromStr;
use std::string::ToString;

mod raw_text;


/// The name of an entity on the scoreboard.
///
/// Characters not allowed:
/// All non-printing characters
/// whitespace
/// '*'
/// '@' (as the first character)
/// '"' (technically allowed, but complicates JSON)
/// Length limit of 40 characters
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "String")]
pub struct ScoreHolder(String);

const MAX_HOLDER_LEN: usize = 40;

impl ScoreHolder {
    pub fn new(string: String) -> Result<Self, String> {
        if string.is_empty() {
            return Err(string);
        }

        let mut is_first = true;
        if string.contains(|c| !Self::legal(c, std::mem::replace(&mut is_first, false))) {
            return Err(string);
        }

        if string.len() > MAX_HOLDER_LEN {
            return Err(string);
        }

        Ok(ScoreHolder(string))
    }

    pub fn new_unchecked(string: String) -> Self {
        ScoreHolder(string)
    }

    pub fn legal(c: char, is_first: bool) -> bool {
        match c {
            '@' if is_first => false,
            '*' | '"' => false,
            _ if c.is_whitespace() => false,
            _ if c.is_control() => false,
            _ => true,
        }
    }

    /*
    /// Replaces any characters in the input string that are not valid in a score holder
    /// with '_', and possibly shortens it if necessary
    pub fn new_lossy(string: String) -> Self {
        let mut is_first = true;
        let result = string.replace(
            |c| !Self::legal(c, std::mem::replace(&mut is_first, false)),
            "_",
        );

        let mut name_alloc = NAME_ALLOC.lock().unwrap();
        let result = name_alloc.get_name(&result).into_owned();

        ScoreHolder::new(result).expect("still had illegal characters")
    }
    */

    /*
    pub fn from_local_name(name: llvm_ir::Name, type_size: usize) -> Vec<Self> {
        let prefix = match name {
            llvm_ir::Name::Number(n) => format!("%{}", n),
            llvm_ir::Name::Name(n) => n,
        };

        (0..((type_size + 3) / 4))
            .map(|idx| ScoreHolder::new_lossy(format!("{}%{}", prefix, idx)))
            .collect()
    }
    */
}

impl TryFrom<String> for ScoreHolder {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        ScoreHolder::new(s)
    }
}

impl AsRef<str> for ScoreHolder {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ScoreHolder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    Uuid(ScoreHolder),
    Selector(Selector),
    Asterisk,
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "*" {
            Ok(Target::Asterisk)
        } else if s.starts_with('@') {
            Ok(s.parse::<Selector>()?.into())
        } else {
            Ok(ScoreHolder::new(s.into())?.into())
        }
    }
}

impl From<ScoreHolder> for Target {
    fn from(score_holder: ScoreHolder) -> Self {
        Target::Uuid(score_holder)
    }
}

impl From<Selector> for Target {
    fn from(selector: Selector) -> Self {
        Target::Selector(selector)
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Selector(selector) => write!(f, "{}", selector),
            Self::Asterisk => write!(f, "*"),
        }
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(into = "String", try_from = "&str")]
pub struct Selector {
    pub var: SelectorVariable,
    pub args: Vec<SelectorArg>,
}

impl std::convert::TryFrom<&str> for Selector {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        s.parse()
    }
}

impl FromStr for Selector {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let var = s[0..2]
            .parse()
            .map_err(|_| format!("invalid selector {}", &s[0..2]))?;
        let args = &s[2..];
        let args = if args.is_empty() {
            Vec::new()
        } else if !args.starts_with('[') || !args.ends_with(']') {
            return Err(format!("incorrect brackets in '{}'", args));
        } else {
            args[1..args.len() - 1]
                .split(',')
                .map(|arg| SelectorArg(arg.to_owned()))
                .collect()
        };

        Ok(Selector { var, args })
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)?;
        let args = self
            .args
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>();
        if !args.is_empty() {
            write!(f, "[{}]", args.join(","))
        } else {
            Ok(())
        }
    }
}

impl From<Selector> for String {
    fn from(s: Selector) -> Self {
        s.to_string()
    }
}

// TODO: These support a much more limit set of characters than a scoreboard objective
// TODO: This should be an enum, probably
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SelectorArg(pub String);

impl fmt::Display for SelectorArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SelectorVariable {
    NearestPlayer,
    RandomPlayer,
    AllPlayers,
    AllEntities,
    ThisEntity,
}

impl FromStr for SelectorVariable {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "@p" => Ok(Self::NearestPlayer),
            "@r" => Ok(Self::RandomPlayer),
            "@a" => Ok(Self::AllPlayers),
            "@e" => Ok(Self::AllEntities),
            "@s" => Ok(Self::ThisEntity),
            _ => Err(()),
        }
    }
}

impl fmt::Display for SelectorVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NearestPlayer => write!(f, "@p"),
            Self::RandomPlayer => write!(f, "@r"),
            Self::AllPlayers => write!(f, "@a"),
            Self::AllEntities => write!(f, "@e"),
            Self::ThisEntity => write!(f, "@s"),
        }
    }
}

#[derive(Debug, PartialEq, Hash, Clone)]
pub enum McRange {
    To(RangeToInclusive<i32>),
    From(RangeFrom<i32>),
    Between(RangeInclusive<i32>),
}

impl McRange {
    pub fn contains(&self, item: i32) -> bool {
        match self {
            Self::To(r) => r.contains(&item),
            Self::From(r) => r.contains(&item),
            Self::Between(r) => r.contains(&item),
        }
    }
}

impl FromStr for McRange {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split("..").collect::<Vec<_>>()[..] {
            [start, end] => {
                let start = if start.is_empty() {
                    None
                } else {
                    Some(start.parse::<i32>()?)
                };

                let end = if end.is_empty() {
                    None
                } else {
                    Some(end.parse::<i32>()?)
                };

                match (start, end) {
                    (Some(start), Some(end)) => Ok(McRange::Between(start..=end)),
                    (Some(start), None) => Ok(McRange::From(start..)),
                    (None, Some(end)) => Ok(McRange::To(..=end)),
                    (None, None) => Err("at least one bound must be specified".into()),
                }
            }
            _ => Err("wrong number of '..'s in str".into()),
        }
    }
}

impl From<RangeToInclusive<i32>> for McRange {
    fn from(r: RangeToInclusive<i32>) -> Self {
        Self::To(r)
    }
}

impl From<RangeFrom<i32>> for McRange {
    fn from(r: RangeFrom<i32>) -> Self {
        Self::From(r)
    }
}

impl From<RangeInclusive<i32>> for McRange {
    fn from(r: RangeInclusive<i32>) -> Self {
        Self::Between(r)
    }
}

impl fmt::Display for McRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::To(r) => write!(f, "..{}", r.end),
            Self::From(r) => write!(f, "{}..", r.start),
            Self::Between(r) => write!(f, "{}..{}", r.start(), r.end()),
        }
    }
}

// TODO: There's many more variants
#[derive(Debug, PartialEq, Clone)]
pub enum Predicate {
    Inverted(Box<Predicate>),
    /// Minecraft calls this "alternative"
    Or(Vec<Predicate>),
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ ")?;

        match self {
            Predicate::Inverted(inner) => write!(
                f,
                "\"condition\": \"minecraft:inverted\", \"term\": {}",
                inner
            )?,
            Predicate::Or(inner) => {
                let inner = inner
                    .iter()
                    .map(|i| format!("{}", i))
                    .collect::<Vec<String>>();

                write!(
                    f,
                    "\tcondition\": \"minecraft:alternative\", \"term\": [{}]",
                    inner.join(", ")
                )?;
            }
        }

        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionId {
    pub name: String,
}

impl FunctionId {
    pub fn new<T: ToString>(name: T) -> Self {
        FunctionId { name: name.to_string() }
    }

    /*
    pub fn new_block<T: ToString>(name: T, block: Name) -> Self {
        Self::new_sub(name, block, 0)
    }

    pub fn new_sub<T: ToString>(name: T, mut block: Name, sub: usize) -> Self {
        let mut name = name.to_string();
        name = name.replace(|c| c == '$' || c == '.' || c == '-', "_");
        name = name.to_ascii_lowercase();

        if let Name::Name(n) = &mut block {
            *n = n.replace(|c| c == '$' || c == '.' || c == '-', "_");
            *n = n.to_ascii_lowercase();
        }

        FunctionId { name, block, sub }
    }
    */

    /// Gets the namespace of this function
    /// ```
    /// # use langcraft::cir::FunctionId;
    /// let id = FunctionId::new("intrinsic:lshr/inner");
    /// assert_eq!(id.namespace(), "intrinsic");
    /// ```
    pub fn namespace(&self) -> &str {
        if let Some(idx) = self.name.find(':') {
            &self.name[..idx]
        } else {
            //DEFAULT_NAMESPACE
            panic!()
        }
    }

    /// Gets the path within the namespace of this function ID
    /// ```
    /// # use langcraft::cir::FunctionId;
    /// let id = FunctionId::new("intrinsic:lshr/inner");
    /// assert_eq!(id.path(), vec!["lshr".to_string(), "inner".to_string()]);
    /// ```
    pub fn path(&self) -> Vec<String> {
        let name = self.to_string();

        let tail = if let Some(idx) = name.find(':') {
            &name[idx + 1..]
        } else {
            &name
        };

        tail.split('/').map(|s| s.to_owned()).collect()
    }
}

impl FromStr for FunctionId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FunctionId { name: s.to_string() })
        /*
        let sub = if let Some(idx) = s.find("-sub") {
            let sub = s[idx + 4..].parse().map_err(|_| format!("failed to parse sub {}", &s[idx + 4..]))?;
            s = &s[..idx];
            sub
        } else {
            0
        };

        let block = if let Some(idx) = s.find("-block") {
            let block = if let Ok(num) = s[idx + 6..].parse() {
                Name::Number(num)
            } else {
                Name::Name(s[idx + 6..].to_owned())
            };
            s = &s[..idx];
            block
        } else {
            Name::Number(0)
        };

        let name = s.to_owned();

        Ok(FunctionId {
            name,
            block,
            sub,
        })*/
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        /*
        if self.block != Name::Number(0) || self.sub != 0 {
            match &self.block {
                Name::Number(n) => write!(f, "-block{}", n)?,
                Name::Name(n) => write!(f, "-block{}", n)?,
            }
        }

        if self.sub != 0 {
            write!(f, "-sub{}", self.sub)?;
        }
        */

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub cmds: Vec<Command>,
}

impl Function {
    pub fn from_str(id: FunctionId, cmds: &str) -> Result<Self, CommandParseError> {
        let cmds = cmds
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.trim().parse())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Function { id, cmds })
    }

    pub fn get_line(&self, idx: usize) -> usize {
        let mut line = 1;
        for c in self.cmds[..idx].iter() {
            line += 1;
            if let Command::Comment(c) = c {
                line += c.chars().filter(|c| *c == '\n').count();
            }
        }
        line
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall {
    pub id: FunctionId,
}

impl fmt::Display for FuncCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.to_string();
        if id.contains(':') {
            write!(f, "function {}", id)
        } else {
            //write!(f, "function {}:{}", DEFAULT_NAMESPACE, id)
            panic!()
        }
    }
}

// Objectives are `i32`
//  - Name [a-zA-Z0-9_.\-+]
//  - Criterion
//  - Display name (JSON)

// Entities can have *scores* in certain objectives
// Score holder is a player's name or entity's UUID that has scores in some objective

/// Execute:
/// `<TARGET>` is the same as the one for the `data` command
///
/// ```text
/// execute
/// ... align <axes> -> execute
/// ... anchored <anchor> -> execute
/// ... as <targets> -> execute
/// ... at <targets> -> execute
/// ... facing (<pos>|entity <targets> <anchor>) -> execute
/// ... in <dimension> -> execute
/// ... positioned (<pos>|as <targets>) -> execute
/// ... rotated (<rot>|as <targets>) -> execute
/// ... store (result|success)
///     ... <TARGET> <path> (byte|short|int|long|float|double) <scale> -> execute
///     ... bossbar <id> (max|value) -> execute
///     ... score <targets> <objective> -> execute
/// ... (if|unless)
///     ... block <pos> <block> -> [execute]
///     ... blocks <start> <end> <destination> (all|masked) -> [execute]
///     ... data
///         ... block <sourcePos> <path> -> [execute]
///         ... entity <source> <path> -> [execute]
///         ... storage <source> <path> -> [execute]
///     ... entity <entities> -> [execute]
///     ... predicate <predicate> -> [execute]
///     ... score <target> <targetObjective>
///         ... (< | <= | = | > | >=) <source> <sourceObjective> -> [execute]
///         ... matches <range> -> [execute]
/// ... run <command>
/// ```
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Execute {
    pub subcommands: Vec<ExecuteSubCmd>,
    pub run: Option<Box<Command>>,
}

impl fmt::Display for Execute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "execute")?;
        for sub in self.subcommands.iter() {
            write!(f, " {}", sub)?;
        }
        if let Some(run) = &self.run {
            write!(f, " run {}", run)?;
        }
        Ok(())
    }
}

impl Execute {
    pub fn new() -> Self {
        Execute::default()
    }

    pub fn with_subcmd(&mut self, cmd: ExecuteSubCmd) -> &mut Self {
        self.subcommands.push(cmd);
        self
    }

    pub fn with_if(&mut self, cond: ExecuteCondition) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Condition {
            is_unless: false,
            cond,
        })
    }

    pub fn with_unless(&mut self, cond: ExecuteCondition) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Condition {
            is_unless: true,
            cond,
        })
    }

    pub fn with_as(&mut self, target: Target) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::As { target })
    }

    pub fn with_at(&mut self, target: Target) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::At { target })
    }

    pub fn with_positioned(&mut self, pos: String) -> &mut Self {
        self.with_subcmd(ExecuteSubCmd::Positioned { pos })
    }

    pub fn with_run<C: Into<Command>>(&mut self, cmd: C) -> &mut Self {
        assert!(self.run.is_none());

        self.run = Some(Box::new(cmd.into()));
        self
    }

    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        for subcmd in self.subcommands.iter() {
            merge_uses(&mut result, &subcmd.holder_uses());
        }

        if let Some(run) = &self.run {
            merge_uses(&mut result, &run.holder_uses());
        }

        result
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteSubCmd {
    // TODO: There's others lol
    Condition {
        is_unless: bool,
        cond: ExecuteCondition,
    },
    Store {
        is_success: bool,
        kind: ExecuteStoreKind,
    },
    As {
        target: Target,
    },
    At {
        target: Target,
    },
    // TODO: There's another one
    Positioned {
        pos: String,
    }
}

impl ExecuteSubCmd {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        match self {
            Self::Condition { is_unless: _, cond } => match cond {
                ExecuteCondition::Score { target, kind, .. } => {
                    let mut result = HashMap::new();

                    if let Target::Uuid(target) = target {
                        merge_use(&mut result, target, HolderUse::ReadOnly)
                    }

                    if let ExecuteCondKind::Relation {
                        source: Target::Uuid(source),
                        ..
                    } = kind
                    {
                        merge_use(&mut result, source, HolderUse::ReadOnly)
                    }

                    result
                }
                ExecuteCondition::Block { .. } => HashMap::new(),
            },
            Self::Store {
                is_success: _,
                kind,
            } => match kind {
                ExecuteStoreKind::Data { .. } => HashMap::new(),
                ExecuteStoreKind::Score { target, .. } => {
                    let mut result = HashMap::new();

                    if let Target::Uuid(target) = target {
                        merge_use(&mut result, target, HolderUse::WriteOnly);
                    }

                    result
                }
            },
            Self::Positioned { .. } |
            Self::At { .. } | Self::As { .. } => HashMap::new(),
        }
    }
}

impl fmt::Display for ExecuteSubCmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Condition { is_unless, cond } => {
                if *is_unless {
                    write!(f, "unless")?;
                } else {
                    write!(f, "if")?;
                }

                write!(f, " {}", cond)
            }
            Self::Store { is_success, kind } => {
                write!(f, "store ")?;
                if *is_success {
                    write!(f, "success ")?;
                } else {
                    write!(f, "result ")?;
                }
                write!(f, "{}", kind)
            }
            Self::As { target } => write!(f, "as {}", target),
            Self::At { target } => write!(f, "at {}", target),
            Self::Positioned { pos } => write!(f, "positioned {}", pos),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteStoreKind {
    // TODO: There's 2 other kinds
    Score {
        target: Target,
        objective: Objective,
    },
    Data {
        target: DataTarget,
        path: String,
        ty: String,
        scale: f32,
    },
}

impl fmt::Display for ExecuteStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Score { target, objective } => write!(f, "score {} {}", target, objective),
            Self::Data {
                target,
                path,
                ty,
                scale,
            } => write!(f, "{} {} {} {}", target, path, ty, scale),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteCondition {
    // TODO: There's more
    Score {
        target: Target,
        target_obj: Objective,
        kind: ExecuteCondKind,
    },
    Block {
        pos: BlockPos,
        block: String,
    },
}

impl FromStr for ExecuteCondition {
    type Err = CommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CommandParser { tail: s }.parse_execute_cond()
    }
}

impl fmt::Display for ExecuteCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecuteCondition::Score {
                target,
                target_obj,
                kind,
            } => write!(f, "score {} {} {}", target, target_obj, kind),
            ExecuteCondition::Block { pos, block } => write!(f, "block {} {}", pos, block),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecuteCondKind {
    Relation {
        relation: Relation,
        source: Target,
        source_obj: Objective,
    },
    Matches(McRange),
}

impl fmt::Display for ExecuteCondKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Relation {
                relation,
                source,
                source_obj,
            } => write!(f, "{} {} {}", relation, source, source_obj),
            Self::Matches(range) => write!(f, "matches {}", range),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Hash, Clone)]
pub enum Relation {
    LessThan,
    LessThanEq,
    Eq,
    GreaterThan,
    GreaterThanEq,
}

impl FromStr for Relation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "<" => Ok(Relation::LessThan),
            "<=" => Ok(Relation::LessThanEq),
            "=" => Ok(Relation::Eq),
            ">" => Ok(Relation::GreaterThan),
            ">=" => Ok(Relation::GreaterThanEq),
            s => Err(format!("invalid relation `{}`", s)),
        }
    }
}

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Relation::LessThan => write!(f, "<"),
            Relation::LessThanEq => write!(f, "<="),
            Relation::Eq => write!(f, "="),
            Relation::GreaterThan => write!(f, ">"),
            Relation::GreaterThanEq => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Gamerule(Gamerule),
    Kill(Kill),
    Fill(Fill),
    Summon(Summon),
    CloneCmd(CloneCmd),
    SetBlock(SetBlock),
    ObjRemove(ObjRemove),
    ObjAdd(ObjAdd),
    ScoreOp(ScoreOp),
    ScoreSet(ScoreSet),
    ScoreGet(ScoreGet),
    ScoreAdd(ScoreAdd),
    Execute(Execute),
    FuncCall(FuncCall),
    Data(Data),
    Tellraw(Box<Tellraw>),
    Teleport(Teleport),
    Comment(String),
}

impl Command {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        match self {
            Self::ScoreOp(c) => c.holder_uses(),
            Self::ScoreSet(c) => c.holder_uses(),
            Self::ScoreGet(c) => c.holder_uses(),
            Self::ScoreAdd(c) => c.holder_uses(),
            Self::Execute(c) => c.holder_uses(),
            Self::Tellraw(c) => (&**c).holder_uses(),
            Self::Data(_)
            | Self::Kill(_)
            | Self::Summon(_)
            | Self::SetBlock(_)
            | Self::FuncCall(_)
            | Self::Teleport(_)
            | Self::Fill(_)
            | Self::CloneCmd(_)
            | Self::ObjRemove(_)
            | Self::ObjAdd(_)
            | Self::Comment(_) 
            | Self::Gamerule(_) => HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HolderUse {
    ReadOnly,
    WriteOnly,
    ReadWrite,
}

struct CommandParser<'a> {
    tail: &'a str,
}

#[derive(Debug, Clone)]
pub enum CommandParseError {
    UnexpectedEof,
}

type CmdParseResult<T> = Result<T, CommandParseError>;

impl CommandParser<'_> {
    pub fn next_word(&mut self) -> CmdParseResult<&str> {
        if self.tail.is_empty() {
            return Err(CommandParseError::UnexpectedEof);
        }

        if let Some(idx) = self.tail.find(char::is_whitespace) {
            let result = self.tail[..idx].trim();
            self.tail = self.tail[idx..].trim();
            Ok(result)
        } else {
            Ok(std::mem::take(&mut self.tail))
        }
    }

    pub fn peek_word(&mut self) -> Option<&str> {
        if self.tail.is_empty() {
            None
        } else if let Some(idx) = self.tail.find(char::is_whitespace) {
            Some(self.tail[..idx].trim())
        } else {
            Some(self.tail)
        }
    }

    pub fn parse(&mut self) -> CmdParseResult<Command> {
        Ok(match self.next_word()? {
            "#" => Command::Comment(self.tail.into()),
            "scoreboard" => self.parse_scoreboard()?,
            "execute" => self.parse_execute()?,
            "function" => FuncCall {
                id: FunctionId::new(self.tail),
            }
            .into(),
            "summon" => self.parse_summon()?,
            "tellraw" => self.parse_tellraw()?,
            "data" => self.parse_data()?,
            "tp" => self.parse_teleport()?,
            "fill" => self.parse_fill()?,
            "clone" => self.parse_clone()?,
            "setblock" => self.parse_setblock()?,
            "kill" => self.parse_kill()?,
            "gamerule" => self.parse_gamerule()?,
            nw => todo!("{:?}", nw),
        })
    }

    pub fn parse_kill(&mut self) -> CmdParseResult<Command> {
        let sel = self.next_word()?.parse().expect("TODO:");

        Ok(Kill(sel).into())
    }

    pub fn parse_gamerule(&mut self) -> CmdParseResult<Command> {
        let rule = self.next_word()?.to_string();
        let value = self.next_word().ok().map(|v| v.to_string());

        Ok(Gamerule { rule, value }.into())
    }

    pub fn parse_summon(&mut self) -> CmdParseResult<Command> {
        let entity = self.next_word()?.to_string();
        let pos = self.parse_rel_pos()?;
        let data = if self.tail.is_empty() { None } else { Some(self.tail.to_string()) };

        Ok(Summon { entity, pos, data }.into())
    }

    pub fn parse_clone(&mut self) -> CmdParseResult<Command> {
        let start = self.parse_rel_pos()?;
        let end = self.parse_rel_pos()?;
        let dest = self.parse_rel_pos()?;

        Ok(CloneCmd { start, end, dest }.into())
    }

    pub fn parse_fill(&mut self) -> CmdParseResult<Command> {
        let start = self.parse_rel_pos()?;
        let end = self.parse_rel_pos()?;
        let block = self.parse_block_spec()?;

        Ok(Fill { start, end, block }.into())
    }

    pub fn parse_block_spec(&mut self) -> CmdParseResult<BlockSpec> {
        let id_end = self.tail.find(|c: char| {
            !(c.is_alphanumeric() || c == ':' || c == '_')
        }).unwrap_or(self.tail.len());

        let id = self.tail[..id_end].to_string();
        self.tail = &self.tail[id_end..];

        let state = if self.tail.starts_with('[') {
            let state_end = self.tail.find(']').expect("TODO:");

            let state = &self.tail[..=state_end];

            self.tail = &self.tail[state_end + 1..];

            state.parse::<BlockState>().expect("TODO:")
        } else {
            BlockState::new()
        };

        let start = self.tail;

        let mut final_idx = None;

        let mut depth = 0;

        for (idx, c) in self.tail.chars().enumerate() {
            if depth == 0 && c == ' ' {
                final_idx = Some(idx);
                break;
            }

            if c == '{' {
                depth += 1;
            }

            if c == '}' {
                depth -= 1;
            }
        }

        let final_idx = final_idx.unwrap_or(self.tail.len());

        // TODO: Error
        assert_eq!(depth, 0, "{:?}", start);
        
        let result = &self.tail[..final_idx];
        self.tail = &self.tail[final_idx..].trim();

        let nbt = result.to_string();

        Ok(BlockSpec {
            id,
            state,
            nbt,
        })
    }

    pub fn parse_block(&mut self) -> String {
        let start = self.tail;

        let mut final_idx = None;

        let mut depth = 0;

        for (idx, c) in self.tail.chars().enumerate() {
            if depth == 0 && c == ' ' {
                final_idx = Some(idx);
                break;
            }

            if c == '{' {
                depth += 1;
            }

            if c == '}' {
                depth -= 1;
            }
        }

        let final_idx = final_idx.unwrap_or(self.tail.len());

        assert_eq!(depth, 0, "{:?}", start);
        assert_ne!(final_idx, 0, "{:?}", start);
        
        let result = &self.tail[..final_idx];
        self.tail = &self.tail[final_idx..].trim();

        result.to_string()
    }

    pub fn parse_setblock(&mut self) -> CmdParseResult<Command> {
        let pos = self.parse_rel_pos()?;

        let block = self.parse_block_spec()?;

        let kind = self
            .next_word()
            .map(|w| w.parse().expect("TODO: "))
            .unwrap_or(SetBlockKind::Replace);

        Ok(SetBlock { pos, block, kind }.into())
    }

    pub fn parse_teleport(&mut self) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let pos = self.parse_rel_pos()?;
        Ok(Teleport { target, pos }.into())
    }

    pub fn parse_modify_source(&mut self) -> CmdParseResult<DataModifySource> {
        match self.next_word()? {
            "value" => {
                if self.tail.starts_with('"') {
                    // TODO: Escapes
                    self.tail = &self.tail[1..];

                    let end = self.tail.find('"').expect("TODO:");

                    let value = self.tail[..end].to_string();
                    self.tail = &self.tail[end + 1..];

                    Ok(DataModifySource::ValueString(value))
                } else {
                    Ok(DataModifySource::Value(self.next_word()?.parse::<i32>().expect("TODO:")))
                }
            }
            nw => todo!("{:?}", nw)
        }
    }

    pub fn parse_data(&mut self) -> CmdParseResult<Command> {
        let kind_word = self.next_word().map(|s| s.to_owned())?;

        let target = self.parse_data_target()?;

        let kind = match kind_word.as_str() {
            "get" => {
                let path = self.next_word()?.to_owned();
                let scale = self.next_word()?.parse::<f32>().unwrap();
                DataKind::Get { path, scale }
            }
            "modify" => {
                let path = self.next_word()?.to_owned();
                let modkind = self.next_word()?.parse::<DataModifyKind>().unwrap();
                let modsource = self.parse_modify_source()?;
                DataKind::Modify { path, kind: modkind, source: modsource }
            }
            nw => todo!("{:?}", nw),
        };

        Ok(Data { target, kind }.into())
    }

    pub fn parse_tellraw(&mut self) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let message = serde_json::from_str(self.tail).expect("TODO:");

        Ok(Tellraw { target, message }.into())
    }

    pub fn parse_execute(&mut self) -> CmdParseResult<Command> {
        let mut cmd = Execute::new();

        loop {
            if self.peek_word() == Some("run") || self.peek_word().is_none() {
                break;
            }

            cmd.with_subcmd(self.parse_execute_subcmd()?);
        }

        if self.next_word().ok() == Some("run") {
            cmd.with_run(self.parse()?);
        }

        Ok(cmd.into())
    }

    pub fn parse_execute_subcmd(&mut self) -> CmdParseResult<ExecuteSubCmd> {
        match self.next_word()? {
            "if" => Ok(ExecuteSubCmd::Condition {
                is_unless: false,
                cond: self.parse_execute_cond()?,
            }),
            "unless" => Ok(ExecuteSubCmd::Condition {
                is_unless: true,
                cond: self.parse_execute_cond()?,
            }),
            "at" => Ok(ExecuteSubCmd::At {
                target: self.next_word()?.parse().expect("TODO:"),
            }),
            "as" => Ok(ExecuteSubCmd::As {
                target: self.next_word()?.parse().expect("TODO:"),
            }),
            "store" => self.parse_execute_store(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_execute_store(&mut self) -> CmdParseResult<ExecuteSubCmd> {
        let is_success = match self.next_word()? {
            "result" => false,
            "success" => true,
            nw => panic!("TODO: {:?}", nw),
        };

        let kind = match self.peek_word() {
            Some("score") => {
                self.next_word().unwrap();
                let target = self.next_word()?.parse().expect("TODO:");
                let objective = self.next_word()?.to_owned();

                ExecuteStoreKind::Score { target, objective }
            }
            _ => {
                let target = self.parse_data_target()?;
                let path = self.next_word()?.to_owned();
                let ty = self.next_word()?.to_owned();
                let scale = self.next_word()?.parse().expect("TODO:");

                ExecuteStoreKind::Data {
                    target,
                    path,
                    ty,
                    scale,
                }
            }
        };

        Ok(ExecuteSubCmd::Store { is_success, kind })
    }

    pub fn parse_coord(&mut self) -> CmdParseResult<Coord> {
        let coord = self.next_word()?;
        Ok(coord.parse().unwrap_or_else(|e| panic!("TODO: {:?} {:?}", coord, e)))
    }

    pub fn parse_rel_pos(&mut self) -> CmdParseResult<RelBlockPos> {
        let x = self.parse_coord()?;
        let y = self.parse_coord()?;
        let z = self.parse_coord()?;
        Ok(RelBlockPos(x, y, z))
    }

    pub fn parse_data_target(&mut self) -> CmdParseResult<DataTarget> {
        match self.next_word()? {
            "block" => Ok(DataTarget::Block(self.parse_rel_pos()?)),
            "entity" => {
                let target = self.next_word()?.parse().expect("TODO:");

                Ok(DataTarget::Entity(target))
            }
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_execute_cond(&mut self) -> CmdParseResult<ExecuteCondition> {
        match self.next_word()? {
            "score" => {
                let target = self.next_word()?.parse().expect("TODO:");
                let target_obj = self.next_word()?.to_owned();
                let kind = match self.next_word()? {
                    "matches" => {
                        ExecuteCondKind::Matches(self.next_word()?.parse().expect("TODO:"))
                    }
                    s => {
                        let relation = s.parse().expect("TODO:");
                        let source = self.next_word()?.parse().expect("TODO:");
                        let source_obj = self.next_word()?.to_owned();
                        ExecuteCondKind::Relation {
                            relation,
                            source,
                            source_obj,
                        }
                    }
                };

                Ok(ExecuteCondition::Score {
                    target,
                    target_obj,
                    kind,
                })
            }
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_scoreboard(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "players" => self.parse_players(),
            "objectives" => self.parse_objectives(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_objectives(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "add" => {
                let obj = self.next_word()?.to_owned();
                let criteria = self.next_word()?.to_owned();
                Ok(ObjAdd { obj, criteria }.into())
            }
            "remove" => Ok(ObjRemove(self.next_word()?.to_owned()).into()),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_players(&mut self) -> CmdParseResult<Command> {
        match self.next_word()? {
            "operation" => self.parse_operation(),
            "add" => self.parse_scoreboard_add(false),
            "remove" => self.parse_scoreboard_add(true),
            "set" => self.parse_scoreboard_set(),
            "get" => self.parse_scoreboard_get(),
            nw => todo!("{:?}", nw),
        }
    }

    pub fn parse_scoreboard_get(&mut self) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let target_obj = self.next_word()?.to_owned();
        Ok(ScoreGet { target, target_obj }.into())
    }

    pub fn parse_scoreboard_set(&mut self) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let target_obj = self.next_word()?.to_owned();

        let score = self.next_word()?;
        let score = score.parse::<i32>().unwrap_or_else(|e| panic!("TODO: {:?} {}", e, score));

        Ok(ScoreSet {
            target,
            target_obj,
            score,
        }
        .into())
    }

    pub fn parse_scoreboard_add(&mut self, is_remove: bool) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let target_obj = self.next_word()?.to_owned();
        let score = self.next_word()?.parse::<i32>().unwrap();
        let score = if is_remove { -score } else { score };
        Ok(ScoreAdd {
            target,
            target_obj,
            score,
        }
        .into())
    }

    pub fn parse_operation(&mut self) -> CmdParseResult<Command> {
        let target = self.next_word()?.parse().expect("TODO:");
        let target_obj = self.next_word()?.to_owned();
        let kind = self.next_word()?.parse().expect("TODO:");
        let source = self.next_word()?.parse().expect("TODO:");
        let source_obj = self.next_word()?.to_owned();
        Ok(ScoreOp {
            target,
            target_obj,
            kind,
            source,
            source_obj,
        }
        .into())
    }
}

impl FromStr for Command {
    type Err = CommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CommandParser { tail: s }.parse()
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Gamerule(s) => s.fmt(f),
            Command::Kill(s) => s.fmt(f),
            Command::Fill(s) => s.fmt(f),
            Command::Summon(s) => s.fmt(f),
            Command::CloneCmd(s) => s.fmt(f),
            Command::SetBlock(s) => s.fmt(f),
            Command::ObjAdd(s) => s.fmt(f),
            Command::ObjRemove(s) => s.fmt(f),
            Command::ScoreOp(s) => s.fmt(f),
            Command::ScoreSet(s) => s.fmt(f),
            Command::ScoreGet(s) => s.fmt(f),
            Command::ScoreAdd(s) => s.fmt(f),
            Command::Execute(s) => s.fmt(f),
            Command::FuncCall(s) => s.fmt(f),
            Command::Data(s) => s.fmt(f),
            Command::Tellraw(s) => s.fmt(f),
            Command::Teleport(s) => s.fmt(f),
            Command::Comment(s) => {
                let mut commented = s.replace('\n', "\n# ");
                commented.insert_str(0, "# ");
                write!(f, "{}", commented)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Gamerule {
    pub rule: String,
    pub value: Option<String>,
}

impl fmt::Display for Gamerule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "gamerule {}", self.rule)?;

        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjRemove(pub Objective);

impl fmt::Display for ObjRemove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoreboard objectives remove {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjAdd {
    pub obj: Objective,
    pub criteria: String,
    // TODO: display name
}

impl fmt::Display for ObjAdd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard objectives add {} {}",
            self.obj, self.criteria
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Kill(Target);

impl fmt::Display for Kill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "kill {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fill {
    pub start: RelBlockPos,
    pub end: RelBlockPos,
    pub block: BlockSpec,
}

impl fmt::Display for Fill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fill {} {} {}", self.start, self.end, self.block)
    }
}

// TODO:
#[derive(Debug, PartialEq, Clone)]
pub struct Summon {
    pub entity: String,
    pub pos: RelBlockPos,
    pub data: Option<String>,
}

impl fmt::Display for Summon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "summon {} {}", self.entity, self.pos)?;
        if let Some(data) = self.data.as_ref() {
            write!(f, " {}", data)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CloneCmd {
    pub start: RelBlockPos,
    pub end: RelBlockPos,
    pub dest: RelBlockPos,
}

impl fmt::Display for CloneCmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "clone {} {} {}", self.start, self.end, self.dest)
    }
}

impl From<Gamerule> for Command {
    fn from(g: Gamerule) -> Self {
        Command::Gamerule(g)
    }
}

impl From<Kill> for Command {
    fn from(k: Kill) -> Self {
        Command::Kill(k)
    }
}

impl From<Teleport> for Command {
    fn from(t: Teleport) -> Self {
        Command::Teleport(t)
    }
}

impl From<Fill> for Command {
    fn from(f: Fill) -> Self {
        Command::Fill(f)
    }
}

impl From<Summon> for Command {
    fn from(c: Summon) -> Self {
        Command::Summon(c)
    }
}

impl From<CloneCmd> for Command {
    fn from(c: CloneCmd) -> Self {
        Command::CloneCmd(c)
    }
}

impl From<ObjAdd> for Command {
    fn from(c: ObjAdd) -> Self {
        Command::ObjAdd(c)
    }
}

impl From<ObjRemove> for Command {
    fn from(c: ObjRemove) -> Self {
        Command::ObjRemove(c)
    }
}

impl From<ScoreOp> for Command {
    fn from(s: ScoreOp) -> Self {
        Command::ScoreOp(s)
    }
}

impl From<ScoreSet> for Command {
    fn from(s: ScoreSet) -> Self {
        Command::ScoreSet(s)
    }
}

impl From<Execute> for Command {
    fn from(e: Execute) -> Self {
        Command::Execute(e)
    }
}

impl From<FuncCall> for Command {
    fn from(f: FuncCall) -> Self {
        Command::FuncCall(f)
    }
}

impl From<Data> for Command {
    fn from(d: Data) -> Self {
        Command::Data(d)
    }
}

impl From<ScoreGet> for Command {
    fn from(s: ScoreGet) -> Self {
        Command::ScoreGet(s)
    }
}

impl From<ScoreAdd> for Command {
    fn from(s: ScoreAdd) -> Self {
        Command::ScoreAdd(s)
    }
}

impl From<SetBlock> for Command {
    fn from(s: SetBlock) -> Self {
        Command::SetBlock(s)
    }
}

impl From<Tellraw> for Command {
    fn from(t: Tellraw) -> Self {
        Command::Tellraw(Box::new(t))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Teleport {
    pub target: Target,
    pub pos: RelBlockPos,
}

impl fmt::Display for Teleport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "tp {} {}", self.target, self.pos)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Tellraw {
    pub target: Target,
    pub message: Vec<TextComponent>,
}

impl Tellraw {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        for m in self.message.iter() {
            merge_uses(&mut result, &m.holder_uses())
        }

        result
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Coord {
    pub val: i32,
    pub is_rel: bool,
}

impl FromStr for Coord {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "~" {
            Ok(Coord { val: 0, is_rel: true })
        } else if let Some(s) = s.strip_prefix("~") {
            let val = s.parse::<i32>().map_err(|s| s.to_string())?;
            Ok(Coord { val, is_rel: true })
        } else {
            let val = s.parse::<i32>().map_err(|s| s.to_string())?;
            Ok(Coord { val, is_rel: false } )
        }
    }
}

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_rel {
            write!(f, "~")?;
        }
        write!(f, "{}", self.val)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct RelBlockPos(pub Coord, pub Coord, pub Coord);

impl RelBlockPos {
    pub fn maybe_based(&self, base: Option<(i32, i32, i32)>) -> (i32, i32, i32) {
        if let Some(b) = base {
            self.based(b)
        } else {
            assert!(!self.0.is_rel);
            assert!(!self.1.is_rel);
            assert!(!self.2.is_rel);
            (self.0.val, self.1.val, self.2.val)
        }
    }

    pub fn based(&self, base: (i32, i32, i32)) -> (i32, i32, i32) {
        let x = if self.0.is_rel {
            self.0.val + base.0
        } else {
            self.0.val
        };

        let y = if self.1.is_rel {
            self.1.val + base.1
        } else {
            self.1.val
        };

        let z = if self.2.is_rel {
            self.2.val + base.2
        } else {
            self.2.val
        };

        (x, y, z)
    }
}

impl fmt::Display for RelBlockPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.0, self.1, self.2)
    }
}

impl FromStr for RelBlockPos {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = s.split_whitespace();
        let x = s.next().ok_or_else(|| "expected x".to_string())?;
        let y = s.next().ok_or_else(|| "expected y".to_string())?;
        let z = s.next().ok_or_else(|| "expected z".to_string())?;
        if s.next().is_some() {
            return Err("unexpected trailing data".to_string())
        }

        Ok(RelBlockPos(x.parse()?, y.parse()?, z.parse()?))
    }
}


type NbtPath = String;
type BlockPos = String;
type StorageId = String;
type StringNbt = String;

impl fmt::Display for Tellraw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "tellraw {} {}",
            self.target,
            serde_json::to_string(&self.message).unwrap()
        )
    }
}

type BlockId = String;

type SNbt = String;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct BlockState(BTreeMap<String, String>);

impl BlockState {
    pub fn new() -> Self {
        BlockState(BTreeMap::new())
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|v| v.as_str())
    }
    
    pub fn items(&self) -> impl Iterator<Item=(&str, &str)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromStr for BlockState {
    type Err = String;

    fn from_str(mut s: &str) -> Result<Self, Self::Err> {
        let orig_s = s;

        if !s.starts_with('[') {
            return Err(format!("expected '[' at beginning of {}", orig_s))
        }
        s = &s[1..];

        if !s.ends_with(']') {
            return Err(format!("expected ']' at end of {}", orig_s));
        }
        s = &s[..s.len() - 1];

        let inner = s.split(',').map(|pair| {
            pair.split_once('=')
                .map(|(l, r)| (l.to_string(), r.to_string()))
                .ok_or_else(|| format!("missing '=' in {}", orig_s))
        }).collect::<Result<BTreeMap<String, String>, _>>()?;

        Ok(BlockState(inner))
    }
}

impl fmt::Display for BlockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        for (idx, (lhs, rhs)) in self.0.iter().enumerate() {
            write!(f, "{}={}", lhs, rhs)?;

            if idx != self.0.len() - 1 {
                write!(f, ",")?;
            }
        }

        write!(f, "]")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockSpec {
    pub id: BlockId,
    pub state: BlockState,
    pub nbt: SNbt,
}

impl fmt::Display for BlockSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.id, self.state, self.nbt)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetBlock {
    pub pos: RelBlockPos,
    pub block: BlockSpec,
    pub kind: SetBlockKind,
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum SetBlockKind {
    Destroy,
    Keep,
    Replace,
}

impl FromStr for SetBlockKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "destroy" => Ok(Self::Destroy),
            "keep" => Ok(Self::Keep),
            "replace" => Ok(Self::Replace),
            _ => Err(format!("invalid setblock kind `{}`", s))
        }
    }
}

impl fmt::Display for SetBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "setblock {} {} {}", self.pos, self.block, self.kind)
    }
}

impl fmt::Display for SetBlockKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SetBlockKind::Destroy => write!(f, "destroy"),
            SetBlockKind::Keep => write!(f, "keep"),
            SetBlockKind::Replace => write!(f, "replace"),
        }
    }
}

pub type Objective = String;

/* Scoreboard (players functions)

TODO: scoreboard players reset <targets> [<objectives>]
*/

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreGet {
    pub target: Target,
    pub target_obj: Objective,
}

impl ScoreGet {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        if let Target::Uuid(target) = &self.target {
            merge_use(&mut result, target, HolderUse::ReadOnly);
        };

        result
    }
}

impl fmt::Display for ScoreGet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players get {} {}",
            self.target, self.target_obj
        )
    }
}

/// `scoreboard players set <targets> <objective> <score>`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreAdd {
    pub target: Target,
    pub target_obj: Objective,
    pub score: i32,
}

impl ScoreAdd {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        if let Target::Uuid(target) = &self.target {
            merge_use(&mut result, target, HolderUse::ReadWrite);
        };

        result
    }
}

impl fmt::Display for ScoreAdd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoreboard players ")?;
        let score = if self.score < 0 {
            write!(f, "remove ")?;
            -self.score
        } else {
            write!(f, "add ")?;
            self.score
        };

        write!(f, "{} {} {}", self.target, self.target_obj, score)
    }
}

/// `scoreboard players set <targets> <objective> <score>`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreSet {
    pub target: Target,
    pub target_obj: Objective,
    pub score: i32,
}

impl ScoreSet {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        if let Target::Uuid(target) = &self.target {
            merge_use(&mut result, target, HolderUse::WriteOnly);
        }

        result
    }
}

impl fmt::Display for ScoreSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players set {} {} {}",
            self.target, self.target_obj, self.score
        )
    }
}

pub fn merge_uses<'a>(
    all: &mut HashMap<&'a ScoreHolder, HolderUse>,
    other: &HashMap<&'a ScoreHolder, HolderUse>,
) {
    for (k, v) in other.iter() {
        merge_use(all, k, *v);
    }
}

pub fn merge_use<'a>(
    all: &mut HashMap<&'a ScoreHolder, HolderUse>,
    holder: &'a ScoreHolder,
    holder_use: HolderUse,
) {
    if let Some(prev) = all.get_mut(holder) {
        if *prev != holder_use {
            *prev = HolderUse::ReadWrite;
        }
    } else {
        all.insert(holder, holder_use);
    }
}

/// `scoreboard players operation <targets> <targetObjective> <operation> <source> <sourceObjective>`
///
/// `<operation>` may be: `+=`, `-=`, `*=`, `/=`, `%=`, `=`, `<` (min), `>` (max), `><` (swap)
///
/// Both `target` and `source` may be `*`, which uses all entites tracked by (TODO: the scoreboard? that objective?)
///
/// All operations treat a null score as 0
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScoreOp {
    pub target: Target,
    pub target_obj: Objective,
    pub kind: ScoreOpKind,
    pub source: Target,
    pub source_obj: Objective,
}

impl ScoreOp {
    pub fn holder_uses(&self) -> HashMap<&ScoreHolder, HolderUse> {
        let mut result = HashMap::new();

        if let Target::Uuid(target) = &self.target {
            let target_use = if self.kind == ScoreOpKind::Assign {
                HolderUse::WriteOnly
            } else {
                HolderUse::ReadOnly
            };

            merge_use(&mut result, target, target_use);
        }

        if let Target::Uuid(source) = &self.source {
            let source_use = if self.kind == ScoreOpKind::Swap {
                HolderUse::ReadWrite
            } else {
                HolderUse::ReadOnly
            };

            merge_use(&mut result, source, source_use);
        }

        result
    }
}

impl fmt::Display for ScoreOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "scoreboard players operation {} {} {} {} {}",
            self.target, self.target_obj, self.kind, self.source, self.source_obj
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScoreOpKind {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    Assign,
    Min,
    Max,
    Swap,
}

impl FromStr for ScoreOpKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+=" => Ok(Self::AddAssign),
            "-=" => Ok(Self::SubAssign),
            "*=" => Ok(Self::MulAssign),
            "/=" => Ok(Self::DivAssign),
            "%=" => Ok(Self::ModAssign),
            "=" => Ok(Self::Assign),
            "<" => Ok(Self::Min),
            ">" => Ok(Self::Max),
            "><" => Ok(Self::Swap),
            _ => Err(s.into()),
        }
    }
}

impl fmt::Display for ScoreOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreOpKind::AddAssign => write!(f, "+="),
            ScoreOpKind::SubAssign => write!(f, "-="),
            ScoreOpKind::MulAssign => write!(f, "*="),
            ScoreOpKind::DivAssign => write!(f, "/="),
            ScoreOpKind::ModAssign => write!(f, "%="),
            ScoreOpKind::Assign => write!(f, "="),
            ScoreOpKind::Min => write!(f, "<"),
            ScoreOpKind::Max => write!(f, ">"),
            ScoreOpKind::Swap => write!(f, "><"),
        }
    }
}

/*

/data
    ... get <TARGET> [<path>] [<scale>]
    ... merge <TARGET> <nbt>
    ... modify <TARGET> <targetPath> <MODIFICATION>
        ... from <SOURCE> [<sourcePath>]
        ... value <value>
    ... remove <TARGET> <path>

<TARGET> = <SOURCE> = (block <targetPos> | entity <target> | storage <target>)
<MODIFICATION> = (append | insert <index> | merge | prepend | set)

*/

#[derive(Debug, PartialEq, Clone)]
pub struct Data {
    pub target: DataTarget,
    pub kind: DataKind,
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "data ")?;
        match &self.kind {
            DataKind::Get { .. } => write!(f, "get ")?,
            DataKind::Modify { .. } => write!(f, "modify ")?,
        }
        write!(f, "{} ", self.target)?;
        match &self.kind {
            DataKind::Get { path, scale } => write!(f, "{} {}", path, scale),
            DataKind::Modify { path, kind, source } => write!(f, "{} {} {}", path, kind, source),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataKind {
    Get {
        path: String,
        scale: f32,
    },
    Modify {
        path: String,
        kind: DataModifyKind,
        source: DataModifySource,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataModifyKind {
    // TODO: There's others
    Set,
}

impl FromStr for DataModifyKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "set" => Ok(DataModifyKind::Set),
            _ => Err(format!("invalid data modify kind {}", s))
        }
    }
}

impl fmt::Display for DataModifyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Set => write!(f, "set"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataModifySource {
    // TODO: There's another
    // TODO: This can technically be other datatypes too, I think
    Value(i32),
    ValueString(String),
}

impl fmt::Display for DataModifySource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataModifySource::Value(v) => write!(f, "value {}", v),
            DataModifySource::ValueString(v) => {
                if v.contains(char::is_control) || v.contains('\\') || v.contains('"') {
                    todo!("{:?}", v)
                }
                write!(f, "value {:?}", v)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataTarget {
    // TODO: More
    Block(RelBlockPos),
    Entity(Target),
}

impl fmt::Display for DataTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataTarget::Block(b) => write!(f, "block {}", b),
            DataTarget::Entity(e) => write!(f, "entity {}", e),
        }
    }
}
