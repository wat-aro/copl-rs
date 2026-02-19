use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    TypingML4BinOp, TypingML4Binding, TypingML4Derivation, TypingML4Env, TypingML4Expr,
    TypingML4Judgment, TypingML4Type,
};

pub(super) fn prove_judgment(
    judgment: TypingML4Judgment,
) -> Result<TypingML4Derivation, CheckError> {
    let TypingML4Judgment::HasType { env, expr, ty } = &judgment;

    let mut ctx = InferContext::new();
    let infer_env = env
        .0
        .iter()
        .map(|binding| (binding.name.clone(), infer_type_from_surface(&binding.ty)))
        .collect::<Vec<_>>();
    let typed_expr = infer_expr(&infer_env, expr, &mut ctx)
        .ok_or_else(|| non_derivable_judgment_error(&judgment, None))?;

    let expected_ty = infer_type_from_surface(ty);
    if ctx.unify(typed_expr.ty.clone(), expected_ty).is_err() {
        let actual = TypingML4Judgment::HasType {
            env: env.clone(),
            expr: expr.clone(),
            ty: ctx.resolve_surface_type(&typed_expr.ty),
        };
        return Err(non_derivable_judgment_error(&judgment, Some(actual)));
    }

    build_derivation(env, &typed_expr, ty, &mut ctx)
        .ok_or_else(|| non_derivable_judgment_error(&judgment, None))
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum InferType {
    Int,
    Bool,
    List(Box<InferType>),
    Fun(Box<InferType>, Box<InferType>),
    Var(usize),
}

#[derive(Debug, Clone)]
struct TypedExpr {
    expr: TypingML4Expr,
    ty: InferType,
    kind: TypedExprKind,
}

#[derive(Debug, Clone)]
enum TypedExprKind {
    Int,
    Bool,
    Var {
        name: String,
    },
    Nil,
    Cons {
        head: Box<TypedExpr>,
        tail: Box<TypedExpr>,
    },
    BinOp {
        op: TypingML4BinOp,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
    Let {
        name: String,
        bound_expr: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },
    Fun {
        param: String,
        param_ty: InferType,
        body: Box<TypedExpr>,
    },
    App {
        func: Box<TypedExpr>,
        arg: Box<TypedExpr>,
    },
    LetRec {
        name: String,
        param: String,
        param_ty: InferType,
        ret_ty: InferType,
        fun_body: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },
    Match {
        scrutinee: Box<TypedExpr>,
        nil_case: Box<TypedExpr>,
        head_name: String,
        tail_name: String,
        cons_case: Box<TypedExpr>,
        elem_ty: InferType,
    },
}

#[derive(Debug, Default)]
struct InferContext {
    substitutions: Vec<Option<InferType>>,
}

impl InferContext {
    fn new() -> Self {
        Self::default()
    }

    fn fresh_var(&mut self) -> InferType {
        let id = self.substitutions.len();
        self.substitutions.push(None);
        InferType::Var(id)
    }

    fn unify(&mut self, left: InferType, right: InferType) -> Result<(), ()> {
        let left = self.resolve(left);
        let right = self.resolve(right);
        match (left, right) {
            (InferType::Int, InferType::Int) | (InferType::Bool, InferType::Bool) => Ok(()),
            (InferType::List(left), InferType::List(right)) => self.unify(*left, *right),
            (InferType::Fun(left_param, left_ret), InferType::Fun(right_param, right_ret)) => {
                self.unify(*left_param, *right_param)?;
                self.unify(*left_ret, *right_ret)
            }
            (InferType::Var(id), ty) | (ty, InferType::Var(id)) => self.bind_var(id, ty),
            _ => Err(()),
        }
    }

    fn bind_var(&mut self, id: usize, ty: InferType) -> Result<(), ()> {
        let ty = self.resolve(ty);
        if matches!(ty, InferType::Var(other) if other == id) {
            return Ok(());
        }
        if self.occurs(id, &ty) {
            return Err(());
        }
        self.substitutions[id] = Some(ty);
        Ok(())
    }

    fn occurs(&mut self, id: usize, ty: &InferType) -> bool {
        match self.resolve(ty.clone()) {
            InferType::Int | InferType::Bool => false,
            InferType::List(inner) => self.occurs(id, &inner),
            InferType::Fun(param, ret) => self.occurs(id, &param) || self.occurs(id, &ret),
            InferType::Var(other) => other == id,
        }
    }

    fn resolve(&mut self, ty: InferType) -> InferType {
        match ty {
            InferType::Int | InferType::Bool => ty,
            InferType::List(inner) => InferType::List(Box::new(self.resolve(*inner))),
            InferType::Fun(param, ret) => {
                InferType::Fun(Box::new(self.resolve(*param)), Box::new(self.resolve(*ret)))
            }
            InferType::Var(id) => {
                if let Some(bound) = self.substitutions[id].clone() {
                    let resolved = self.resolve(bound);
                    self.substitutions[id] = Some(resolved.clone());
                    resolved
                } else {
                    InferType::Var(id)
                }
            }
        }
    }

    fn resolve_surface_type(&mut self, ty: &InferType) -> TypingML4Type {
        match self.resolve(ty.clone()) {
            InferType::Int => TypingML4Type::Int,
            InferType::Bool => TypingML4Type::Bool,
            InferType::List(inner) => {
                TypingML4Type::List(Box::new(self.resolve_surface_type(&inner)))
            }
            InferType::Fun(param, ret) => TypingML4Type::Fun {
                param: Box::new(self.resolve_surface_type(&param)),
                ret: Box::new(self.resolve_surface_type(&ret)),
            },
            InferType::Var(_) => TypingML4Type::Int,
        }
    }
}

fn infer_type_from_surface(ty: &TypingML4Type) -> InferType {
    match ty {
        TypingML4Type::Int => InferType::Int,
        TypingML4Type::Bool => InferType::Bool,
        TypingML4Type::List(inner) => InferType::List(Box::new(infer_type_from_surface(inner))),
        TypingML4Type::Fun { param, ret } => InferType::Fun(
            Box::new(infer_type_from_surface(param)),
            Box::new(infer_type_from_surface(ret)),
        ),
    }
}

fn infer_expr(
    env: &[(String, InferType)],
    expr: &TypingML4Expr,
    ctx: &mut InferContext,
) -> Option<TypedExpr> {
    match expr {
        TypingML4Expr::Int(_) => Some(TypedExpr {
            expr: expr.clone(),
            ty: InferType::Int,
            kind: TypedExprKind::Int,
        }),
        TypingML4Expr::Bool(_) => Some(TypedExpr {
            expr: expr.clone(),
            ty: InferType::Bool,
            kind: TypedExprKind::Bool,
        }),
        TypingML4Expr::Var(name) => {
            let ty = lookup_infer_type(env, name)?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty,
                kind: TypedExprKind::Var { name: name.clone() },
            })
        }
        TypingML4Expr::Nil => {
            let elem_ty = ctx.fresh_var();
            Some(TypedExpr {
                expr: expr.clone(),
                ty: InferType::List(Box::new(elem_ty)),
                kind: TypedExprKind::Nil,
            })
        }
        TypingML4Expr::Cons { head, tail } => {
            let typed_head = infer_expr(env, head, ctx)?;
            let typed_tail = infer_expr(env, tail, ctx)?;
            ctx.unify(
                typed_tail.ty.clone(),
                InferType::List(Box::new(typed_head.ty.clone())),
            )
            .ok()?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: InferType::List(Box::new(typed_head.ty.clone())),
                kind: TypedExprKind::Cons {
                    head: Box::new(typed_head),
                    tail: Box::new(typed_tail),
                },
            })
        }
        TypingML4Expr::BinOp { op, left, right } => {
            let typed_left = infer_expr(env, left, ctx)?;
            let typed_right = infer_expr(env, right, ctx)?;
            ctx.unify(typed_left.ty.clone(), InferType::Int).ok()?;
            ctx.unify(typed_right.ty.clone(), InferType::Int).ok()?;
            let result_ty = match op {
                TypingML4BinOp::Lt => InferType::Bool,
                TypingML4BinOp::Plus | TypingML4BinOp::Minus | TypingML4BinOp::Times => {
                    InferType::Int
                }
            };
            Some(TypedExpr {
                expr: expr.clone(),
                ty: result_ty,
                kind: TypedExprKind::BinOp {
                    op: *op,
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                },
            })
        }
        TypingML4Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let typed_condition = infer_expr(env, condition, ctx)?;
            let typed_then = infer_expr(env, then_branch, ctx)?;
            let typed_else = infer_expr(env, else_branch, ctx)?;
            ctx.unify(typed_condition.ty.clone(), InferType::Bool)
                .ok()?;
            ctx.unify(typed_then.ty.clone(), typed_else.ty.clone())
                .ok()?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: typed_then.ty.clone(),
                kind: TypedExprKind::If {
                    condition: Box::new(typed_condition),
                    then_branch: Box::new(typed_then),
                    else_branch: Box::new(typed_else),
                },
            })
        }
        TypingML4Expr::Let {
            name,
            bound_expr,
            body,
        } => {
            let typed_bound = infer_expr(env, bound_expr, ctx)?;
            let extended = push_infer_binding(env, name, typed_bound.ty.clone());
            let typed_body = infer_expr(&extended, body, ctx)?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: typed_body.ty.clone(),
                kind: TypedExprKind::Let {
                    name: name.clone(),
                    bound_expr: Box::new(typed_bound),
                    body: Box::new(typed_body),
                },
            })
        }
        TypingML4Expr::Fun { param, body } => {
            let param_ty = ctx.fresh_var();
            let extended = push_infer_binding(env, param, param_ty.clone());
            let typed_body = infer_expr(&extended, body, ctx)?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: InferType::Fun(Box::new(param_ty.clone()), Box::new(typed_body.ty.clone())),
                kind: TypedExprKind::Fun {
                    param: param.clone(),
                    param_ty,
                    body: Box::new(typed_body),
                },
            })
        }
        TypingML4Expr::App { func, arg } => {
            let typed_func = infer_expr(env, func, ctx)?;
            let typed_arg = infer_expr(env, arg, ctx)?;
            let result_ty = ctx.fresh_var();
            ctx.unify(
                typed_func.ty.clone(),
                InferType::Fun(Box::new(typed_arg.ty.clone()), Box::new(result_ty.clone())),
            )
            .ok()?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: result_ty,
                kind: TypedExprKind::App {
                    func: Box::new(typed_func),
                    arg: Box::new(typed_arg),
                },
            })
        }
        TypingML4Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let param_ty = ctx.fresh_var();
            let ret_ty = ctx.fresh_var();
            let fun_ty = InferType::Fun(Box::new(param_ty.clone()), Box::new(ret_ty.clone()));
            let env_with_fun = push_infer_binding(env, name, fun_ty);
            let env_for_fun_body = push_infer_binding(&env_with_fun, param, param_ty.clone());
            let typed_fun_body = infer_expr(&env_for_fun_body, fun_body, ctx)?;
            ctx.unify(typed_fun_body.ty.clone(), ret_ty.clone()).ok()?;
            let typed_body = infer_expr(&env_with_fun, body, ctx)?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: typed_body.ty.clone(),
                kind: TypedExprKind::LetRec {
                    name: name.clone(),
                    param: param.clone(),
                    param_ty,
                    ret_ty,
                    fun_body: Box::new(typed_fun_body),
                    body: Box::new(typed_body),
                },
            })
        }
        TypingML4Expr::Match {
            scrutinee,
            nil_case,
            head_name,
            tail_name,
            cons_case,
        } => {
            let typed_scrutinee = infer_expr(env, scrutinee, ctx)?;
            let typed_nil_case = infer_expr(env, nil_case, ctx)?;
            let elem_ty = ctx.fresh_var();
            let list_ty = InferType::List(Box::new(elem_ty.clone()));
            ctx.unify(typed_scrutinee.ty.clone(), list_ty.clone())
                .ok()?;

            let env_with_head = push_infer_binding(env, head_name, elem_ty.clone());
            let env_with_tail = push_infer_binding(&env_with_head, tail_name, list_ty.clone());
            let typed_cons_case = infer_expr(&env_with_tail, cons_case, ctx)?;
            ctx.unify(typed_nil_case.ty.clone(), typed_cons_case.ty.clone())
                .ok()?;

            Some(TypedExpr {
                expr: expr.clone(),
                ty: typed_nil_case.ty.clone(),
                kind: TypedExprKind::Match {
                    scrutinee: Box::new(typed_scrutinee),
                    nil_case: Box::new(typed_nil_case),
                    head_name: head_name.clone(),
                    tail_name: tail_name.clone(),
                    cons_case: Box::new(typed_cons_case),
                    elem_ty,
                },
            })
        }
    }
}

fn build_derivation(
    env: &TypingML4Env,
    typed: &TypedExpr,
    expected_ty: &TypingML4Type,
    ctx: &mut InferContext,
) -> Option<TypingML4Derivation> {
    let rule_name = match &typed.kind {
        TypedExprKind::Int => {
            if !matches!(expected_ty, TypingML4Type::Int) {
                return None;
            }
            "T-Int"
        }
        TypedExprKind::Bool => {
            if !matches!(expected_ty, TypingML4Type::Bool) {
                return None;
            }
            "T-Bool"
        }
        TypedExprKind::Var { name } => {
            if lookup_surface_type(env, name)? != expected_ty {
                return None;
            }
            "T-Var"
        }
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let first = build_derivation(env, condition, &TypingML4Type::Bool, ctx)?;
            let second = build_derivation(env, then_branch, expected_ty, ctx)?;
            let third = build_derivation(env, else_branch, expected_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-If",
                vec![first, second, third],
            ));
        }
        TypedExprKind::BinOp { op, left, right } => {
            let (rule, result_ty) = match op {
                TypingML4BinOp::Plus => ("T-Plus", TypingML4Type::Int),
                TypingML4BinOp::Minus => ("T-Minus", TypingML4Type::Int),
                TypingML4BinOp::Times => ("T-Times", TypingML4Type::Int),
                TypingML4BinOp::Lt => ("T-Lt", TypingML4Type::Bool),
            };
            if *expected_ty != result_ty {
                return None;
            }
            let first = build_derivation(env, left, &TypingML4Type::Int, ctx)?;
            let second = build_derivation(env, right, &TypingML4Type::Int, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                rule,
                vec![first, second],
            ));
        }
        TypedExprKind::Let {
            name,
            bound_expr,
            body,
        } => {
            let bound_ty = ctx.resolve_surface_type(&bound_expr.ty);
            let first = build_derivation(env, bound_expr, &bound_ty, ctx)?;
            let extended = push_surface_binding(env, name, &bound_ty);
            let second = build_derivation(&extended, body, expected_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Let",
                vec![first, second],
            ));
        }
        TypedExprKind::Fun {
            param,
            param_ty,
            body,
        } => {
            let TypingML4Type::Fun {
                param: expected_param,
                ret: expected_ret,
            } = expected_ty
            else {
                return None;
            };

            if &ctx.resolve_surface_type(param_ty) != expected_param.as_ref() {
                return None;
            }

            let extended = push_surface_binding(env, param, expected_param);
            let body_derivation = build_derivation(&extended, body, expected_ret, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Fun",
                vec![body_derivation],
            ));
        }
        TypedExprKind::App { func, arg } => {
            let arg_ty = ctx.resolve_surface_type(&arg.ty);
            let fun_ty = TypingML4Type::Fun {
                param: Box::new(arg_ty.clone()),
                ret: Box::new(expected_ty.clone()),
            };
            let first = build_derivation(env, func, &fun_ty, ctx)?;
            let second = build_derivation(env, arg, &arg_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-App",
                vec![first, second],
            ));
        }
        TypedExprKind::LetRec {
            name,
            param,
            param_ty,
            ret_ty,
            fun_body,
            body,
        } => {
            let surface_param_ty = ctx.resolve_surface_type(param_ty);
            let surface_ret_ty = ctx.resolve_surface_type(ret_ty);
            let fun_ty = TypingML4Type::Fun {
                param: Box::new(surface_param_ty.clone()),
                ret: Box::new(surface_ret_ty.clone()),
            };
            let env_with_fun = push_surface_binding(env, name, &fun_ty);
            let env_for_fun_body = push_surface_binding(&env_with_fun, param, &surface_param_ty);
            let first = build_derivation(&env_for_fun_body, fun_body, &surface_ret_ty, ctx)?;
            let second = build_derivation(&env_with_fun, body, expected_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-LetRec",
                vec![first, second],
            ));
        }
        TypedExprKind::Nil => {
            if !matches!(expected_ty, TypingML4Type::List(_)) {
                return None;
            }
            "T-Nil"
        }
        TypedExprKind::Cons { head, tail } => {
            let TypingML4Type::List(elem_ty) = expected_ty else {
                return None;
            };
            let first = build_derivation(env, head, elem_ty, ctx)?;
            let second = build_derivation(env, tail, expected_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Cons",
                vec![first, second],
            ));
        }
        TypedExprKind::Match {
            scrutinee,
            nil_case,
            head_name,
            tail_name,
            cons_case,
            elem_ty,
        } => {
            let surface_elem_ty = ctx.resolve_surface_type(elem_ty);
            let scrutinee_ty = TypingML4Type::List(Box::new(surface_elem_ty.clone()));
            let first = build_derivation(env, scrutinee, &scrutinee_ty, ctx)?;
            let second = build_derivation(env, nil_case, expected_ty, ctx)?;
            let with_head = push_surface_binding(env, head_name, &surface_elem_ty);
            let with_tail = push_surface_binding(&with_head, tail_name, &scrutinee_ty);
            let third = build_derivation(&with_tail, cons_case, expected_ty, ctx)?;
            return Some(derivation(
                TypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Match",
                vec![first, second, third],
            ));
        }
    };

    Some(derivation(
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: typed.expr.clone(),
            ty: expected_ty.clone(),
        },
        rule_name,
        Vec::new(),
    ))
}

fn lookup_infer_type(env: &[(String, InferType)], name: &str) -> Option<InferType> {
    env.iter()
        .rev()
        .find(|(binding_name, _)| binding_name == name)
        .map(|(_, ty)| ty.clone())
}

fn lookup_surface_type<'a>(env: &'a TypingML4Env, name: &str) -> Option<&'a TypingML4Type> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.ty)
}

fn push_infer_binding(
    env: &[(String, InferType)],
    name: &str,
    ty: InferType,
) -> Vec<(String, InferType)> {
    let mut extended = env.to_vec();
    extended.push((name.to_string(), ty));
    extended
}

fn push_surface_binding(env: &TypingML4Env, name: &str, ty: &TypingML4Type) -> TypingML4Env {
    let mut bindings = env.0.clone();
    bindings.push(TypingML4Binding {
        name: name.to_string(),
        ty: ty.clone(),
    });
    TypingML4Env(bindings)
}

fn non_derivable_judgment_error(
    actual: &TypingML4Judgment,
    expected: Option<TypingML4Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in TypingML4 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in TypingML4 (actual: {actual}; fix: check environment bindings and expression/type consistency)"
        )),
    }
}

fn fix_message(actual: &TypingML4Judgment, expected: &TypingML4Judgment) -> String {
    let (
        TypingML4Judgment::HasType {
            env: actual_env,
            expr: actual_expr,
            ty: actual_ty,
        },
        TypingML4Judgment::HasType {
            env: expected_env,
            expr: expected_expr,
            ty: expected_ty,
        },
    ) = (actual, expected);

    if actual_env == expected_env && actual_expr == expected_expr {
        if actual_ty == expected_ty {
            "fix: check expression typing constraints and environment bindings".to_string()
        } else {
            format!("fix: replace conclusion type with {expected_ty}")
        }
    } else {
        "fix: check expression typing constraints and environment bindings".to_string()
    }
}

fn derivation(
    judgment: TypingML4Judgment,
    rule_name: &str,
    subderivations: Vec<TypingML4Derivation>,
) -> TypingML4Derivation {
    TypingML4Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, TypingML4Derivation};
    use crate::games::typing_ml4::syntax::{
        TypingML4Binding, TypingML4Env, TypingML4Expr, TypingML4Judgment, TypingML4Type,
    };

    #[test]
    fn proves_int_judgment_with_t_int() {
        let derivation = prove_judgment(TypingML4Judgment::HasType {
            env: TypingML4Env::default(),
            expr: TypingML4Expr::Int(3),
            ty: TypingML4Type::Int,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_var_with_nearest_binding_by_t_var() {
        let derivation = prove_judgment(TypingML4Judgment::HasType {
            env: TypingML4Env(vec![
                TypingML4Binding {
                    name: "x".to_string(),
                    ty: TypingML4Type::Bool,
                },
                TypingML4Binding {
                    name: "x".to_string(),
                    ty: TypingML4Type::Int,
                },
            ]),
            expr: TypingML4Expr::Var("x".to_string()),
            ty: TypingML4Type::Int,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Var");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_let_rec_judgment_with_t_let_rec() {
        let derivation = prove_judgment(TypingML4Judgment::HasType {
            env: TypingML4Env::default(),
            expr: TypingML4Expr::LetRec {
                name: "length".to_string(),
                param: "l".to_string(),
                fun_body: Box::new(TypingML4Expr::Match {
                    scrutinee: Box::new(TypingML4Expr::Var("l".to_string())),
                    nil_case: Box::new(TypingML4Expr::Int(0)),
                    head_name: "x".to_string(),
                    tail_name: "y".to_string(),
                    cons_case: Box::new(TypingML4Expr::Int(1)),
                }),
                body: Box::new(TypingML4Expr::Var("length".to_string())),
            },
            ty: TypingML4Type::Fun {
                param: Box::new(TypingML4Type::List(Box::new(TypingML4Type::Int))),
                ret: Box::new(TypingML4Type::Int),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-LetRec");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "T-Match");
        assert_eq!(derivation.subderivations[1].rule_name, "T-Var");
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(TypingML4Judgment::HasType {
            env: TypingML4Env::default(),
            expr: TypingML4Expr::Int(1),
            ty: TypingML4Type::Bool,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in TypingML4"));
        assert!(err
            .message()
            .contains("expected: |- 1 : int, actual: |- 1 : bool"));
        assert!(err
            .message()
            .contains("fix: replace conclusion type with int"));
    }

    #[test]
    fn rejects_ill_typed_judgment() {
        let err = prove_judgment(TypingML4Judgment::HasType {
            env: TypingML4Env::default(),
            expr: TypingML4Expr::BinOp {
                op: crate::games::typing_ml4::syntax::TypingML4BinOp::Plus,
                left: Box::new(TypingML4Expr::Bool(true)),
                right: Box::new(TypingML4Expr::Int(1)),
            },
            ty: TypingML4Type::Int,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in TypingML4"));
        assert!(err
            .message()
            .contains("fix: check environment bindings and expression/type consistency"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_102() {
        let expected =
            parse_source(include_str!("../../../copl/102.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &TypingML4Derivation, expected: &TypingML4Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
