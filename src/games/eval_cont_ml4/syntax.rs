use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalContML4BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalContML4BinOp {
    pub const fn symbol(self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Lt => "<",
        }
    }

    const fn precedence(self) -> u8 {
        match self {
            Self::Lt => 2,
            Self::Plus | Self::Minus => 3,
            Self::Times => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML4Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Nil,
    Cons {
        head: Box<EvalContML4Expr>,
        tail: Box<EvalContML4Expr>,
    },
    BinOp {
        op: EvalContML4BinOp,
        left: Box<EvalContML4Expr>,
        right: Box<EvalContML4Expr>,
    },
    If {
        condition: Box<EvalContML4Expr>,
        then_branch: Box<EvalContML4Expr>,
        else_branch: Box<EvalContML4Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalContML4Expr>,
        body: Box<EvalContML4Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<EvalContML4Expr>,
        body: Box<EvalContML4Expr>,
    },
    LetCc {
        name: String,
        body: Box<EvalContML4Expr>,
    },
    Fun {
        param: String,
        body: Box<EvalContML4Expr>,
    },
    App {
        func: Box<EvalContML4Expr>,
        arg: Box<EvalContML4Expr>,
    },
    Match {
        scrutinee: Box<EvalContML4Expr>,
        nil_case: Box<EvalContML4Expr>,
        head_name: String,
        tail_name: String,
        cons_case: Box<EvalContML4Expr>,
    },
}

impl EvalContML4Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Var(_) | Self::Nil => 6,
            Self::App { .. } => 5,
            Self::BinOp { op, .. } => op.precedence(),
            Self::Cons { .. } => 1,
            Self::If { .. }
            | Self::Let { .. }
            | Self::LetRec { .. }
            | Self::LetCc { .. }
            | Self::Fun { .. }
            | Self::Match { .. } => 0,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int(value) => write!(f, "{value}")?,
            Self::Bool(value) => write!(f, "{value}")?,
            Self::Var(name) => write!(f, "{name}")?,
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                if parent == 0
                    && (matches!(right.as_ref(), Self::If { .. })
                        || matches!(right.as_ref(), Self::Let { .. })
                        || matches!(right.as_ref(), Self::LetRec { .. })
                        || matches!(right.as_ref(), Self::LetCc { .. })
                        || matches!(right.as_ref(), Self::Fun { .. })
                        || matches!(right.as_ref(), Self::Match { .. }))
                {
                    right.fmt_with_precedence(f, 0)?;
                } else {
                    right.fmt_with_precedence(f, op.precedence())?;
                }
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if ")?;
                condition.fmt_with_precedence(f, 0)?;
                write!(f, " then ")?;
                then_branch.fmt_with_precedence(f, 0)?;
                write!(f, " else ")?;
                else_branch.fmt_with_precedence(f, 0)?;
            }
            Self::Let {
                name,
                bound_expr,
                body,
            } => {
                write!(f, "let {name} = ")?;
                bound_expr.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::LetRec {
                name,
                param,
                fun_body,
                body,
            } => {
                write!(f, "let rec {name} = fun {param} -> ")?;
                fun_body.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::LetCc { name, body } => {
                write!(f, "letcc {name} in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::Fun { param, body } => {
                write!(f, "fun {param} -> ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::App { func, arg } => {
                func.fmt_with_precedence(f, self.precedence())?;
                write!(f, " ")?;
                arg.fmt_with_precedence(f, self.precedence() + 1)?;
            }
            Self::Match {
                scrutinee,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            } => {
                write!(f, "match ")?;
                scrutinee.fmt_with_precedence(f, 0)?;
                write!(f, " with [] -> ")?;
                nil_case.fmt_with_precedence(f, 0)?;
                write!(f, " | {head_name} :: {tail_name} -> ")?;
                cons_case.fmt_with_precedence(f, 0)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalContML4Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML4ContFrame {
    EvalR {
        env: Option<EvalContML4Env>,
        op: EvalContML4BinOp,
        right: EvalContML4Expr,
    },
    Plus {
        left: i64,
    },
    Minus {
        left: i64,
    },
    Times {
        left: i64,
    },
    Lt {
        left: i64,
    },
    If {
        env: Option<EvalContML4Env>,
        then_branch: EvalContML4Expr,
        else_branch: EvalContML4Expr,
    },
    LetBody {
        env: Option<EvalContML4Env>,
        name: String,
        body: EvalContML4Expr,
    },
    EvalArg {
        env: Option<EvalContML4Env>,
        arg: EvalContML4Expr,
    },
    EvalFun {
        func: EvalContML4Value,
    },
    EvalConsR {
        env: Option<EvalContML4Env>,
        tail_expr: EvalContML4Expr,
    },
    Cons {
        head: EvalContML4Value,
    },
    Match {
        env: Option<EvalContML4Env>,
        nil_case: EvalContML4Expr,
        head_name: String,
        tail_name: String,
        cons_case: EvalContML4Expr,
    },
}

fn format_env_prefix(env: &Option<EvalContML4Env>) -> String {
    match env {
        Some(env) if env.0.is_empty() => "|- ".to_string(),
        Some(env) => format!("{env} |- "),
        None => String::new(),
    }
}

impl fmt::Display for EvalContML4ContFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalR { env, op, right } => {
                let prefix = format_env_prefix(env);
                write!(f, "{{{prefix}_ {} {right}}}", op.symbol())
            }
            Self::Plus { left } => write!(f, "{{{left} + _}}"),
            Self::Minus { left } => write!(f, "{{{left} - _}}"),
            Self::Times { left } => write!(f, "{{{left} * _}}"),
            Self::Lt { left } => write!(f, "{{{left} < _}}"),
            Self::If {
                env,
                then_branch,
                else_branch,
            } => {
                let prefix = format_env_prefix(env);
                write!(f, "{{{prefix}if _ then {then_branch} else {else_branch}}}")
            }
            Self::LetBody { env, name, body } => {
                let prefix = format_env_prefix(env);
                write!(f, "{{{prefix}let {name} = _ in {body}}}")
            }
            Self::EvalArg { env, arg } => {
                let prefix = format_env_prefix(env);
                write!(f, "{{{prefix}_ {arg}}}")
            }
            Self::EvalFun { func } => write!(f, "{{{func} _}}"),
            Self::EvalConsR { env, tail_expr } => {
                let prefix = format_env_prefix(env);
                write!(f, "{{{prefix}_ :: {tail_expr}}}")
            }
            Self::Cons { head } => write!(f, "{{{head} :: _}}"),
            Self::Match {
                env,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            } => {
                let prefix = format_env_prefix(env);
                write!(
                    f,
                    "{{{prefix}match _ with [] -> {nil_case} | {head_name} :: {tail_name} -> {cons_case}}}"
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalContML4Continuation {
    pub frames: Vec<EvalContML4ContFrame>,
    pub explicit_ret: bool,
}

impl EvalContML4Continuation {
    pub const fn hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: true,
        }
    }

    pub const fn implicit_hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: false,
        }
    }

    pub fn semantic_eq(&self, other: &Self) -> bool {
        self.frames == other.frames
    }
}

impl fmt::Display for EvalContML4Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.frames.is_empty() {
            return write!(f, "_");
        }

        for (index, frame) in self.frames.iter().enumerate() {
            if index > 0 {
                write!(f, " >> ")?;
            }
            write!(f, "{frame}")?;
        }

        if self.explicit_ret {
            write!(f, " >> _")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML4Value {
    Int(i64),
    Bool(bool),
    Closure {
        env: EvalContML4Env,
        param: String,
        body: EvalContML4Expr,
    },
    RecClosure {
        env: EvalContML4Env,
        name: String,
        param: String,
        body: EvalContML4Expr,
    },
    Continuation(EvalContML4Continuation),
    Nil,
    Cons {
        head: Box<EvalContML4Value>,
        tail: Box<EvalContML4Value>,
    },
}

impl EvalContML4Value {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Cons { .. } => 1,
            _ => 2,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int(value) => write!(f, "{value}")?,
            Self::Bool(value) => write!(f, "{value}")?,
            Self::Closure { env, param, body } => {
                write!(f, "({env})[fun {param} -> {body}]")?;
            }
            Self::RecClosure {
                env,
                name,
                param,
                body,
            } => {
                write!(f, "({env})[rec {name} = fun {param} -> {body}]")?;
            }
            Self::Continuation(continuation) => {
                write!(f, "[ {continuation} ]")?;
            }
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalContML4Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalContML4Binding {
    pub name: String,
    pub value: EvalContML4Value,
}

impl fmt::Display for EvalContML4Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalContML4Env(pub Vec<EvalContML4Binding>);

impl fmt::Display for EvalContML4Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = self
            .0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{text}")
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML4Judgment {
    EvalTo {
        env: EvalContML4Env,
        expr: EvalContML4Expr,
        continuation: EvalContML4Continuation,
        value: EvalContML4Value,
        has_continuation: bool,
    },
    ContEvalTo {
        input: EvalContML4Value,
        continuation: EvalContML4Continuation,
        value: EvalContML4Value,
    },
    PlusIs {
        left: i64,
        right: i64,
        result: i64,
    },
    MinusIs {
        left: i64,
        right: i64,
        result: i64,
    },
    TimesIs {
        left: i64,
        right: i64,
        result: i64,
    },
    LessThanIs {
        left: i64,
        right: i64,
        result: bool,
    },
}

impl fmt::Display for EvalContML4Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo {
                env,
                expr,
                continuation,
                value,
                has_continuation,
            } => {
                if *has_continuation || !continuation.frames.is_empty() || continuation.explicit_ret
                {
                    if env.0.is_empty() {
                        write!(f, "|- {expr} >> {continuation} evalto {value}")
                    } else {
                        write!(f, "{env} |- {expr} >> {continuation} evalto {value}")
                    }
                } else if env.0.is_empty() {
                    write!(f, "|- {expr} evalto {value}")
                } else {
                    write!(f, "{env} |- {expr} evalto {value}")
                }
            }
            Self::ContEvalTo {
                input,
                continuation,
                value,
            } => write!(f, "{input} => {continuation} evalto {value}"),
            Self::PlusIs {
                left,
                right,
                result,
            } => write!(f, "{left} plus {right} is {result}"),
            Self::MinusIs {
                left,
                right,
                result,
            } => write!(f, "{left} minus {right} is {result}"),
            Self::TimesIs {
                left,
                right,
                result,
            } => write!(f, "{left} times {right} is {result}"),
            Self::LessThanIs {
                left,
                right,
                result,
            } => write!(f, "{left} less than {right} is {result}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalContML4Derivation {
    pub span: SourceSpan,
    pub judgment: EvalContML4Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalContML4Derivation>,
}
