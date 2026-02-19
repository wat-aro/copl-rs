use std::collections::{HashMap, HashSet};

use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    PolyTypingML4BinOp, PolyTypingML4Binding, PolyTypingML4Derivation, PolyTypingML4Env,
    PolyTypingML4Expr, PolyTypingML4Judgment, PolyTypingML4Type, PolyTypingML4TypeScheme,
};

pub(super) fn prove_judgment(
    judgment: PolyTypingML4Judgment,
) -> Result<PolyTypingML4Derivation, CheckError> {
    let PolyTypingML4Judgment::HasType { env, expr, ty } = &judgment;

    let mut ctx = InferContext::new(collect_reserved_type_var_names(env, ty));
    let infer_env = infer_env_from_surface(env, &mut ctx);
    let typed_expr = infer_expr(&infer_env, expr, &mut ctx)
        .ok_or_else(|| non_derivable_judgment_error(&judgment, None))?;

    let expected_ty = infer_type_from_surface(ty);
    if ctx.unify(typed_expr.ty.clone(), expected_ty).is_err() {
        let actual = PolyTypingML4Judgment::HasType {
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
    Flex(usize),
    Rigid(String),
}

#[derive(Debug, Clone)]
struct InferScheme {
    quantified: Vec<usize>,
    ty: InferType,
}

impl InferScheme {
    fn mono(ty: InferType) -> Self {
        Self {
            quantified: Vec::new(),
            ty,
        }
    }
}

#[derive(Debug, Clone)]
struct InferBinding {
    name: String,
    scheme: InferScheme,
}

#[derive(Debug, Clone)]
struct TypedExpr {
    expr: PolyTypingML4Expr,
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
        op: PolyTypingML4BinOp,
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

#[derive(Debug)]
struct InferContext {
    substitutions: Vec<Option<InferType>>,
    surface_var_names: HashMap<usize, String>,
    next_surface_var_index: usize,
    reserved_surface_var_names: HashSet<String>,
}

impl InferContext {
    fn new(reserved_surface_var_names: HashSet<String>) -> Self {
        Self {
            substitutions: Vec::new(),
            surface_var_names: HashMap::new(),
            next_surface_var_index: 0,
            reserved_surface_var_names,
        }
    }

    fn fresh_var(&mut self) -> InferType {
        InferType::Flex(self.fresh_var_id())
    }

    fn fresh_var_id(&mut self) -> usize {
        let id = self.substitutions.len();
        self.substitutions.push(None);
        id
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
            (InferType::Rigid(left), InferType::Rigid(right)) => {
                if left == right {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (InferType::Flex(id), ty) | (ty, InferType::Flex(id)) => self.bind_var(id, ty),
            _ => Err(()),
        }
    }

    fn bind_var(&mut self, id: usize, ty: InferType) -> Result<(), ()> {
        let ty = self.resolve(ty);
        if matches!(ty, InferType::Flex(other) if other == id) {
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
            InferType::Int | InferType::Bool | InferType::Rigid(_) => false,
            InferType::List(inner) => self.occurs(id, &inner),
            InferType::Fun(param, ret) => self.occurs(id, &param) || self.occurs(id, &ret),
            InferType::Flex(other) => other == id,
        }
    }

    fn resolve(&mut self, ty: InferType) -> InferType {
        match ty {
            InferType::Int | InferType::Bool | InferType::Rigid(_) => ty,
            InferType::List(inner) => InferType::List(Box::new(self.resolve(*inner))),
            InferType::Fun(param, ret) => {
                InferType::Fun(Box::new(self.resolve(*param)), Box::new(self.resolve(*ret)))
            }
            InferType::Flex(id) => {
                if let Some(bound) = self.substitutions[id].clone() {
                    let resolved = self.resolve(bound);
                    self.substitutions[id] = Some(resolved.clone());
                    resolved
                } else {
                    InferType::Flex(id)
                }
            }
        }
    }

    fn resolve_surface_type(&mut self, ty: &InferType) -> PolyTypingML4Type {
        let resolved = self.resolve(ty.clone());
        self.surface_type_from_infer(resolved)
    }

    fn surface_type_from_infer(&mut self, ty: InferType) -> PolyTypingML4Type {
        match ty {
            InferType::Int => PolyTypingML4Type::Int,
            InferType::Bool => PolyTypingML4Type::Bool,
            InferType::List(inner) => {
                PolyTypingML4Type::List(Box::new(self.surface_type_from_infer(*inner)))
            }
            InferType::Fun(param, ret) => PolyTypingML4Type::Fun {
                param: Box::new(self.surface_type_from_infer(*param)),
                ret: Box::new(self.surface_type_from_infer(*ret)),
            },
            InferType::Rigid(name) => PolyTypingML4Type::Var(name),
            InferType::Flex(id) => PolyTypingML4Type::Var(self.surface_var_name(id)),
        }
    }

    fn surface_var_name(&mut self, id: usize) -> String {
        if let Some(name) = self.surface_var_names.get(&id) {
            return name.clone();
        }
        loop {
            let candidate = alphabetical_name(self.next_surface_var_index);
            self.next_surface_var_index += 1;
            if self.reserved_surface_var_names.contains(&candidate) {
                continue;
            }
            self.reserved_surface_var_names.insert(candidate.clone());
            self.surface_var_names.insert(id, candidate.clone());
            return candidate;
        }
    }
}

fn infer_type_from_surface(ty: &PolyTypingML4Type) -> InferType {
    match ty {
        PolyTypingML4Type::Int => InferType::Int,
        PolyTypingML4Type::Bool => InferType::Bool,
        PolyTypingML4Type::Var(name) => InferType::Rigid(name.clone()),
        PolyTypingML4Type::List(inner) => InferType::List(Box::new(infer_type_from_surface(inner))),
        PolyTypingML4Type::Fun { param, ret } => InferType::Fun(
            Box::new(infer_type_from_surface(param)),
            Box::new(infer_type_from_surface(ret)),
        ),
    }
}

fn infer_env_from_surface(env: &PolyTypingML4Env, ctx: &mut InferContext) -> Vec<InferBinding> {
    env.0
        .iter()
        .map(|binding| InferBinding {
            name: binding.name.clone(),
            scheme: infer_scheme_from_surface(&binding.scheme, ctx),
        })
        .collect()
}

fn infer_scheme_from_surface(
    scheme: &PolyTypingML4TypeScheme,
    ctx: &mut InferContext,
) -> InferScheme {
    let mut quantified_map = HashMap::<String, usize>::new();
    let mut quantified = Vec::with_capacity(scheme.quantified.len());
    for name in &scheme.quantified {
        let id = ctx.fresh_var_id();
        quantified_map.insert(name.clone(), id);
        quantified.push(id);
    }
    let ty = infer_type_from_surface_with_quantified(&scheme.ty, &quantified_map);
    InferScheme { quantified, ty }
}

fn infer_type_from_surface_with_quantified(
    ty: &PolyTypingML4Type,
    quantified_map: &HashMap<String, usize>,
) -> InferType {
    match ty {
        PolyTypingML4Type::Int => InferType::Int,
        PolyTypingML4Type::Bool => InferType::Bool,
        PolyTypingML4Type::Var(name) => {
            if let Some(id) = quantified_map.get(name) {
                InferType::Flex(*id)
            } else {
                InferType::Rigid(name.clone())
            }
        }
        PolyTypingML4Type::List(inner) => InferType::List(Box::new(
            infer_type_from_surface_with_quantified(inner, quantified_map),
        )),
        PolyTypingML4Type::Fun { param, ret } => InferType::Fun(
            Box::new(infer_type_from_surface_with_quantified(
                param,
                quantified_map,
            )),
            Box::new(infer_type_from_surface_with_quantified(ret, quantified_map)),
        ),
    }
}

fn instantiate_scheme(scheme: &InferScheme, ctx: &mut InferContext) -> InferType {
    let mut replacements = HashMap::<usize, InferType>::new();
    for id in &scheme.quantified {
        replacements.insert(*id, ctx.fresh_var());
    }
    instantiate_type(scheme.ty.clone(), &replacements, ctx)
}

fn instantiate_type(
    ty: InferType,
    replacements: &HashMap<usize, InferType>,
    ctx: &mut InferContext,
) -> InferType {
    match ctx.resolve(ty) {
        InferType::Int => InferType::Int,
        InferType::Bool => InferType::Bool,
        InferType::Rigid(name) => InferType::Rigid(name),
        InferType::List(inner) => {
            InferType::List(Box::new(instantiate_type(*inner, replacements, ctx)))
        }
        InferType::Fun(param, ret) => InferType::Fun(
            Box::new(instantiate_type(*param, replacements, ctx)),
            Box::new(instantiate_type(*ret, replacements, ctx)),
        ),
        InferType::Flex(id) => replacements
            .get(&id)
            .cloned()
            .unwrap_or(InferType::Flex(id)),
    }
}

fn infer_expr(
    env: &[InferBinding],
    expr: &PolyTypingML4Expr,
    ctx: &mut InferContext,
) -> Option<TypedExpr> {
    match expr {
        PolyTypingML4Expr::Int(_) => Some(TypedExpr {
            expr: expr.clone(),
            ty: InferType::Int,
            kind: TypedExprKind::Int,
        }),
        PolyTypingML4Expr::Bool(_) => Some(TypedExpr {
            expr: expr.clone(),
            ty: InferType::Bool,
            kind: TypedExprKind::Bool,
        }),
        PolyTypingML4Expr::Var(name) => {
            let scheme = lookup_infer_scheme(env, name)?;
            let ty = instantiate_scheme(&scheme, ctx);
            Some(TypedExpr {
                expr: expr.clone(),
                ty,
                kind: TypedExprKind::Var { name: name.clone() },
            })
        }
        PolyTypingML4Expr::Nil => {
            let elem_ty = ctx.fresh_var();
            Some(TypedExpr {
                expr: expr.clone(),
                ty: InferType::List(Box::new(elem_ty)),
                kind: TypedExprKind::Nil,
            })
        }
        PolyTypingML4Expr::Cons { head, tail } => {
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
        PolyTypingML4Expr::BinOp { op, left, right } => {
            let typed_left = infer_expr(env, left, ctx)?;
            let typed_right = infer_expr(env, right, ctx)?;
            ctx.unify(typed_left.ty.clone(), InferType::Int).ok()?;
            ctx.unify(typed_right.ty.clone(), InferType::Int).ok()?;
            let result_ty = match op {
                PolyTypingML4BinOp::Lt => InferType::Bool,
                PolyTypingML4BinOp::Plus
                | PolyTypingML4BinOp::Minus
                | PolyTypingML4BinOp::Times => InferType::Int,
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
        PolyTypingML4Expr::If {
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
        PolyTypingML4Expr::Let {
            name,
            bound_expr,
            body,
        } => {
            let typed_bound = infer_expr(env, bound_expr, ctx)?;
            let generalized = generalize_infer_type(&typed_bound.ty, env, ctx);
            let extended = push_infer_binding(env, name, generalized);
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
        PolyTypingML4Expr::Fun { param, body } => {
            let param_ty = ctx.fresh_var();
            let extended = push_infer_binding(env, param, InferScheme::mono(param_ty.clone()));
            let typed_body = infer_expr(&extended, body, ctx)?;
            Some(TypedExpr {
                expr: expr.clone(),
                ty: InferType::Fun(Box::new(param_ty.clone()), Box::new(typed_body.ty.clone())),
                kind: TypedExprKind::Fun {
                    param: param.clone(),
                    body: Box::new(typed_body),
                },
            })
        }
        PolyTypingML4Expr::App { func, arg } => {
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
        PolyTypingML4Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let param_ty = ctx.fresh_var();
            let ret_ty = ctx.fresh_var();
            let fun_ty = InferType::Fun(Box::new(param_ty.clone()), Box::new(ret_ty.clone()));
            let env_with_fun = push_infer_binding(env, name, InferScheme::mono(fun_ty.clone()));
            let env_for_fun_body =
                push_infer_binding(&env_with_fun, param, InferScheme::mono(param_ty.clone()));
            let typed_fun_body = infer_expr(&env_for_fun_body, fun_body, ctx)?;
            ctx.unify(typed_fun_body.ty.clone(), ret_ty.clone()).ok()?;

            let generalized = generalize_infer_type(&fun_ty, env, ctx);
            let env_for_body = push_infer_binding(env, name, generalized);
            let typed_body = infer_expr(&env_for_body, body, ctx)?;
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
        PolyTypingML4Expr::Match {
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

            let env_with_head =
                push_infer_binding(env, head_name, InferScheme::mono(elem_ty.clone()));
            let env_with_tail = push_infer_binding(
                &env_with_head,
                tail_name,
                InferScheme::mono(list_ty.clone()),
            );
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

fn generalize_infer_type(
    ty: &InferType,
    env: &[InferBinding],
    ctx: &mut InferContext,
) -> InferScheme {
    let resolved_ty = ctx.resolve(ty.clone());
    let env_free = free_flex_vars_in_infer_env(env, ctx);
    let mut ordered = Vec::new();
    collect_flex_vars_in_order(&resolved_ty, &mut ordered);

    let mut quantified = Vec::new();
    for id in ordered {
        if env_free.contains(&id) || quantified.contains(&id) {
            continue;
        }
        quantified.push(id);
    }

    InferScheme {
        quantified,
        ty: resolved_ty,
    }
}

fn free_flex_vars_in_infer_env(env: &[InferBinding], ctx: &mut InferContext) -> HashSet<usize> {
    let mut set = HashSet::new();
    for binding in env {
        let mut scheme_set = HashSet::new();
        let resolved = ctx.resolve(binding.scheme.ty.clone());
        collect_flex_vars_to_set(&resolved, &mut scheme_set);
        for quantified in &binding.scheme.quantified {
            scheme_set.remove(quantified);
        }
        set.extend(scheme_set);
    }
    set
}

fn collect_flex_vars_in_order(ty: &InferType, out: &mut Vec<usize>) {
    match ty {
        InferType::Int | InferType::Bool | InferType::Rigid(_) => {}
        InferType::List(inner) => collect_flex_vars_in_order(inner, out),
        InferType::Fun(param, ret) => {
            collect_flex_vars_in_order(param, out);
            collect_flex_vars_in_order(ret, out);
        }
        InferType::Flex(id) => out.push(*id),
    }
}

fn collect_flex_vars_to_set(ty: &InferType, out: &mut HashSet<usize>) {
    match ty {
        InferType::Int | InferType::Bool | InferType::Rigid(_) => {}
        InferType::List(inner) => collect_flex_vars_to_set(inner, out),
        InferType::Fun(param, ret) => {
            collect_flex_vars_to_set(param, out);
            collect_flex_vars_to_set(ret, out);
        }
        InferType::Flex(id) => {
            out.insert(*id);
        }
    }
}

fn build_derivation(
    env: &PolyTypingML4Env,
    typed: &TypedExpr,
    expected_ty: &PolyTypingML4Type,
    ctx: &mut InferContext,
) -> Option<PolyTypingML4Derivation> {
    let rule_name = match &typed.kind {
        TypedExprKind::Int => {
            if !matches!(expected_ty, PolyTypingML4Type::Int) {
                return None;
            }
            "T-Int"
        }
        TypedExprKind::Bool => {
            if !matches!(expected_ty, PolyTypingML4Type::Bool) {
                return None;
            }
            "T-Bool"
        }
        TypedExprKind::Var { name } => {
            let scheme = lookup_surface_scheme(env, name)?;
            if !is_instance_of_scheme(scheme, expected_ty) {
                return None;
            }
            "T-Var"
        }
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let first = build_derivation(env, condition, &PolyTypingML4Type::Bool, ctx)?;
            let second = build_derivation(env, then_branch, expected_ty, ctx)?;
            let third = build_derivation(env, else_branch, expected_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
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
                PolyTypingML4BinOp::Plus => ("T-Plus", PolyTypingML4Type::Int),
                PolyTypingML4BinOp::Minus => ("T-Minus", PolyTypingML4Type::Int),
                PolyTypingML4BinOp::Times => ("T-Mult", PolyTypingML4Type::Int),
                PolyTypingML4BinOp::Lt => ("T-Lt", PolyTypingML4Type::Bool),
            };
            if *expected_ty != result_ty {
                return None;
            }
            let first = build_derivation(env, left, &PolyTypingML4Type::Int, ctx)?;
            let second = build_derivation(env, right, &PolyTypingML4Type::Int, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
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
            let generalized = generalize_surface_type(env, &bound_ty);
            let extended = push_surface_binding(env, name, &generalized);
            let second = build_derivation(&extended, body, expected_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Let",
                vec![first, second],
            ));
        }
        TypedExprKind::Fun { param, body } => {
            let PolyTypingML4Type::Fun {
                param: expected_param,
                ret: expected_ret,
            } = expected_ty
            else {
                return None;
            };

            let extended = push_surface_binding(
                env,
                param,
                &PolyTypingML4TypeScheme::mono(*expected_param.clone()),
            );
            let body_derivation = build_derivation(&extended, body, expected_ret, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-Abs",
                vec![body_derivation],
            ));
        }
        TypedExprKind::App { func, arg } => {
            let arg_ty = ctx.resolve_surface_type(&arg.ty);
            let fun_ty = PolyTypingML4Type::Fun {
                param: Box::new(arg_ty.clone()),
                ret: Box::new(expected_ty.clone()),
            };
            let first = build_derivation(env, func, &fun_ty, ctx)?;
            let second = build_derivation(env, arg, &arg_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
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
            let fun_ty = PolyTypingML4Type::Fun {
                param: Box::new(surface_param_ty.clone()),
                ret: Box::new(surface_ret_ty.clone()),
            };
            let rec_mono = PolyTypingML4TypeScheme::mono(fun_ty.clone());
            let env_with_fun = push_surface_binding(env, name, &rec_mono);
            let env_for_fun_body = push_surface_binding(
                &env_with_fun,
                param,
                &PolyTypingML4TypeScheme::mono(surface_param_ty.clone()),
            );
            let first = build_derivation(&env_for_fun_body, fun_body, &surface_ret_ty, ctx)?;

            let generalized = generalize_surface_type(env, &fun_ty);
            let env_for_body = push_surface_binding(env, name, &generalized);
            let second = build_derivation(&env_for_body, body, expected_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
                    env: env.clone(),
                    expr: typed.expr.clone(),
                    ty: expected_ty.clone(),
                },
                "T-LetRec",
                vec![first, second],
            ));
        }
        TypedExprKind::Nil => {
            if !matches!(expected_ty, PolyTypingML4Type::List(_)) {
                return None;
            }
            "T-Nil"
        }
        TypedExprKind::Cons { head, tail } => {
            let PolyTypingML4Type::List(elem_ty) = expected_ty else {
                return None;
            };
            let first = build_derivation(env, head, elem_ty, ctx)?;
            let second = build_derivation(env, tail, expected_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
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
            let scrutinee_ty = PolyTypingML4Type::List(Box::new(surface_elem_ty.clone()));
            let first = build_derivation(env, scrutinee, &scrutinee_ty, ctx)?;
            let second = build_derivation(env, nil_case, expected_ty, ctx)?;
            let with_head = push_surface_binding(
                env,
                head_name,
                &PolyTypingML4TypeScheme::mono(surface_elem_ty.clone()),
            );
            let with_tail = push_surface_binding(
                &with_head,
                tail_name,
                &PolyTypingML4TypeScheme::mono(scrutinee_ty.clone()),
            );
            let third = build_derivation(&with_tail, cons_case, expected_ty, ctx)?;
            return Some(derivation(
                PolyTypingML4Judgment::HasType {
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: typed.expr.clone(),
            ty: expected_ty.clone(),
        },
        rule_name,
        Vec::new(),
    ))
}

fn lookup_infer_scheme(env: &[InferBinding], name: &str) -> Option<InferScheme> {
    env.iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| binding.scheme.clone())
}

fn push_infer_binding(env: &[InferBinding], name: &str, scheme: InferScheme) -> Vec<InferBinding> {
    let mut extended = env.to_vec();
    extended.push(InferBinding {
        name: name.to_string(),
        scheme,
    });
    extended
}

fn lookup_surface_scheme<'a>(
    env: &'a PolyTypingML4Env,
    name: &str,
) -> Option<&'a PolyTypingML4TypeScheme> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.scheme)
}

fn push_surface_binding(
    env: &PolyTypingML4Env,
    name: &str,
    scheme: &PolyTypingML4TypeScheme,
) -> PolyTypingML4Env {
    let mut bindings = env.0.clone();
    bindings.push(PolyTypingML4Binding {
        name: name.to_string(),
        scheme: scheme.clone(),
    });
    PolyTypingML4Env(bindings)
}

fn generalize_surface_type(
    env: &PolyTypingML4Env,
    ty: &PolyTypingML4Type,
) -> PolyTypingML4TypeScheme {
    let env_free = free_type_vars_in_env(env);
    let mut ordered = Vec::new();
    collect_type_vars_in_order(ty, &mut ordered);

    let mut quantified = Vec::new();
    for name in ordered {
        if env_free.contains(&name) || quantified.contains(&name) {
            continue;
        }
        quantified.push(name);
    }

    PolyTypingML4TypeScheme {
        quantified,
        ty: ty.clone(),
    }
}

fn free_type_vars_in_env(env: &PolyTypingML4Env) -> HashSet<String> {
    let mut set = HashSet::new();
    for binding in &env.0 {
        let mut scheme_set = HashSet::new();
        collect_type_vars_to_set(&binding.scheme.ty, &mut scheme_set);
        for quantified in &binding.scheme.quantified {
            scheme_set.remove(quantified);
        }
        set.extend(scheme_set);
    }
    set
}

fn collect_type_vars_in_order(ty: &PolyTypingML4Type, out: &mut Vec<String>) {
    match ty {
        PolyTypingML4Type::Int | PolyTypingML4Type::Bool => {}
        PolyTypingML4Type::Var(name) => out.push(name.clone()),
        PolyTypingML4Type::List(inner) => collect_type_vars_in_order(inner, out),
        PolyTypingML4Type::Fun { param, ret } => {
            collect_type_vars_in_order(param, out);
            collect_type_vars_in_order(ret, out);
        }
    }
}

fn collect_type_vars_to_set(ty: &PolyTypingML4Type, out: &mut HashSet<String>) {
    match ty {
        PolyTypingML4Type::Int | PolyTypingML4Type::Bool => {}
        PolyTypingML4Type::Var(name) => {
            out.insert(name.clone());
        }
        PolyTypingML4Type::List(inner) => collect_type_vars_to_set(inner, out),
        PolyTypingML4Type::Fun { param, ret } => {
            collect_type_vars_to_set(param, out);
            collect_type_vars_to_set(ret, out);
        }
    }
}

fn collect_reserved_type_var_names(
    env: &PolyTypingML4Env,
    ty: &PolyTypingML4Type,
) -> HashSet<String> {
    let mut reserved = HashSet::new();
    collect_type_vars_to_set(ty, &mut reserved);
    for binding in &env.0 {
        collect_type_vars_to_set(&binding.scheme.ty, &mut reserved);
        reserved.extend(binding.scheme.quantified.iter().cloned());
    }
    reserved
}

fn is_instance_of_scheme(scheme: &PolyTypingML4TypeScheme, target: &PolyTypingML4Type) -> bool {
    let quantified = scheme.quantified.iter().cloned().collect::<HashSet<_>>();
    let mut subst = HashMap::<String, PolyTypingML4Type>::new();
    match_type_instance(&scheme.ty, target, &quantified, &mut subst)
}

fn match_type_instance(
    pattern: &PolyTypingML4Type,
    target: &PolyTypingML4Type,
    quantified: &HashSet<String>,
    subst: &mut HashMap<String, PolyTypingML4Type>,
) -> bool {
    match pattern {
        PolyTypingML4Type::Int => matches!(target, PolyTypingML4Type::Int),
        PolyTypingML4Type::Bool => matches!(target, PolyTypingML4Type::Bool),
        PolyTypingML4Type::Var(name) => {
            if quantified.contains(name) {
                if let Some(bound) = subst.get(name) {
                    bound == target
                } else {
                    subst.insert(name.clone(), target.clone());
                    true
                }
            } else {
                matches!(target, PolyTypingML4Type::Var(other) if other == name)
            }
        }
        PolyTypingML4Type::List(inner) => {
            let PolyTypingML4Type::List(target_inner) = target else {
                return false;
            };
            match_type_instance(inner, target_inner, quantified, subst)
        }
        PolyTypingML4Type::Fun { param, ret } => {
            let PolyTypingML4Type::Fun {
                param: target_param,
                ret: target_ret,
            } = target
            else {
                return false;
            };
            match_type_instance(param, target_param, quantified, subst)
                && match_type_instance(ret, target_ret, quantified, subst)
        }
    }
}

fn non_derivable_judgment_error(
    actual: &PolyTypingML4Judgment,
    expected: Option<PolyTypingML4Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in PolyTypingML4 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in PolyTypingML4 (actual: {actual}; fix: check environment bindings and expression/type consistency)"
        )),
    }
}

fn fix_message(actual: &PolyTypingML4Judgment, expected: &PolyTypingML4Judgment) -> String {
    let (
        PolyTypingML4Judgment::HasType {
            env: actual_env,
            expr: actual_expr,
            ty: actual_ty,
        },
        PolyTypingML4Judgment::HasType {
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
    judgment: PolyTypingML4Judgment,
    rule_name: &str,
    subderivations: Vec<PolyTypingML4Derivation>,
) -> PolyTypingML4Derivation {
    PolyTypingML4Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

fn alphabetical_name(mut index: usize) -> String {
    let mut letters = Vec::new();
    loop {
        letters.push((b'a' + (index % 26) as u8) as char);
        if index < 26 {
            break;
        }
        index = (index / 26) - 1;
    }
    letters.reverse();
    letters.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, PolyTypingML4Derivation};
    use crate::games::poly_typing_ml4::syntax::{
        PolyTypingML4BinOp, PolyTypingML4Binding, PolyTypingML4Env, PolyTypingML4Expr,
        PolyTypingML4Judgment, PolyTypingML4Type, PolyTypingML4TypeScheme,
    };

    #[test]
    fn proves_int_judgment_with_t_int() {
        let derivation = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env::default(),
            expr: PolyTypingML4Expr::Int(3),
            ty: PolyTypingML4Type::Int,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_var_with_nearest_binding_by_t_var() {
        let derivation = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env(vec![
                PolyTypingML4Binding {
                    name: "x".to_string(),
                    scheme: PolyTypingML4TypeScheme::mono(PolyTypingML4Type::Bool),
                },
                PolyTypingML4Binding {
                    name: "x".to_string(),
                    scheme: PolyTypingML4TypeScheme::mono(PolyTypingML4Type::Int),
                },
            ]),
            expr: PolyTypingML4Expr::Var("x".to_string()),
            ty: PolyTypingML4Type::Int,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Var");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_var_instantiation_by_t_var() {
        let derivation = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env(vec![PolyTypingML4Binding {
                name: "id".to_string(),
                scheme: PolyTypingML4TypeScheme {
                    quantified: vec!["a".to_string()],
                    ty: PolyTypingML4Type::Fun {
                        param: Box::new(PolyTypingML4Type::Var("a".to_string())),
                        ret: Box::new(PolyTypingML4Type::Var("a".to_string())),
                    },
                },
            }]),
            expr: PolyTypingML4Expr::Var("id".to_string()),
            ty: PolyTypingML4Type::Fun {
                param: Box::new(PolyTypingML4Type::Int),
                ret: Box::new(PolyTypingML4Type::Int),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Var");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_let_rec_judgment_with_t_let_rec() {
        let derivation = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env::default(),
            expr: PolyTypingML4Expr::LetRec {
                name: "id".to_string(),
                param: "x".to_string(),
                fun_body: Box::new(PolyTypingML4Expr::Var("x".to_string())),
                body: Box::new(PolyTypingML4Expr::Var("id".to_string())),
            },
            ty: PolyTypingML4Type::Fun {
                param: Box::new(PolyTypingML4Type::Var("a".to_string())),
                ret: Box::new(PolyTypingML4Type::Var("a".to_string())),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-LetRec");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "T-Var");
        assert_eq!(derivation.subderivations[1].rule_name, "T-Var");
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env::default(),
            expr: PolyTypingML4Expr::Int(1),
            ty: PolyTypingML4Type::Bool,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in PolyTypingML4"));
        assert!(err
            .message()
            .contains("expected: |- 1 : int, actual: |- 1 : bool"));
        assert!(err
            .message()
            .contains("fix: replace conclusion type with int"));
    }

    #[test]
    fn rejects_ill_typed_judgment() {
        let err = prove_judgment(PolyTypingML4Judgment::HasType {
            env: PolyTypingML4Env::default(),
            expr: PolyTypingML4Expr::BinOp {
                op: PolyTypingML4BinOp::Plus,
                left: Box::new(PolyTypingML4Expr::Bool(true)),
                right: Box::new(PolyTypingML4Expr::Int(1)),
            },
            ty: PolyTypingML4Type::Int,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in PolyTypingML4"));
        assert!(err
            .message()
            .contains("fix: check environment bindings and expression/type consistency"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_107() {
        let expected =
            parse_source(include_str!("../../../copl/107.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &PolyTypingML4Derivation, expected: &PolyTypingML4Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
