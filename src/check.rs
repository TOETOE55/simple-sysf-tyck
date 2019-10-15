use crate::ast::{Tm, TmRef, Ty, TyRef};
use crate::tcm::{TCM, TCS};

pub fn infer(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    let ty_exh_size = tcs.ty_exh.size();
    let rules = [var_rule, abs_rule, app_rule, gen_rule, inst_rule];
    for rule in rules.iter() {
        if let Ok(ty) = rule(tcs, tm) {
            return Ok(ty);
        }
        tcs.ty_exh.shrink(ty_exh_size);
    }
    Err("no match type")
}

fn var_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Var(dbi) => {
            let depth = tcs.gamma.len();
            let ty_ref = (tcs.gamma)[depth - dbi - 1];
            Ok(ty_ref)
        }
        _ => Err("expected variable"),
    }
}

fn abs_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Abs(param_ty, body) => {
            tcs.gamma.push(param_ty);
            let ret_ty = infer(tcs, body)?;
            tcs.gamma.pop();
            let arr = tcs.ty_exh.arr(param_ty, ret_ty);
            Ok(arr)
        }
        _ => Err("expected lambda expression"),
    }
}

fn app_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::App(opt, opr) => {
            let arr = infer(tcs, opt)?;
            let (param_ty, ret_ty) = match tcs.ty_exh.deref(arr) {
                Ty::Arr(param_ty, ret_ty) => (param_ty, ret_ty),
                _ => return Err("not a function"),
            };

            let arg_ty = infer(tcs, opr)?;
            if !tcs.ty_exh.eq(arg_ty, param_ty) {
                return Err("could not match as the same type");
            }

            Ok(ret_ty)
        }
        _ => Err("excepted application"),
    }
}

fn gen_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Gen(body) => {
            let ty = infer(tcs, body)?;
            let gen = tcs.ty_exh.for_all(ty);
            Ok(gen)
        }
        _ => Err("excepted generalization"),
    }
}

fn inst_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Inst(tm, spec) => {
            let tm_ty = infer(tcs, tm)?;
            let for_all = match tcs.ty_exh.deref(tm_ty) {
                Ty::ForAll(t) => t,
                _ => return Err("not a generic type"),
            };
            let res_ty = tcs.ty_exh.subst(for_all, spec, 0);
            Ok(res_ty)
        }
        _ => Err("excepted specification"),
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{TmExpr, TyExpr};
    use crate::exhibit::{TmExhibit, TyExhibit};
    use crate::tcm::TCS;

    #[test]
    fn id() {
        let mut ty_exh = TyExhibit::new();
        let mut tm_exh = TmExhibit::new();
        let gen_id_expr = TmExpr::Gen(Box::new(TmExpr::Abs(
            TyExpr::Var(0),
            Box::new(TmExpr::Var(0)),
        )));
        let gen_id = tm_exh.alloc(&gen_id_expr, &mut ty_exh);
        assert_eq!(tm_exh.quote(gen_id, &ty_exh), gen_id_expr);

        let expected_ty_expr = TyExpr::ForAll(Box::new(TyExpr::Arr(
            Box::new(TyExpr::Var(0)),
            Box::new(TyExpr::Var(0)),
        )));
        let expected_ty = ty_exh.alloc(&expected_ty_expr);
        assert_eq!(ty_exh.quote(expected_ty), expected_ty_expr);

        let mut tcs = TCS::new(tm_exh, ty_exh);
        let inferred_ty = tcs.infer(gen_id).unwrap();
        assert!(tcs.ty_exh.eq(expected_ty, inferred_ty));
    }

    #[test]
    fn r#const() {
        let mut ty_exh = TyExhibit::new();
        let mut tm_exh = TmExhibit::new();
        let gen_const_expr = TmExpr::Gen(Box::new(TmExpr::Gen(Box::new(TmExpr::Abs(
            TyExpr::Var(1),
            Box::new(TmExpr::Abs(TyExpr::Var(0), Box::new(TmExpr::Var(1)))),
        )))));
        let gen_const = tm_exh.alloc(&gen_const_expr, &mut ty_exh);
        assert_eq!(tm_exh.quote(gen_const, &ty_exh), gen_const_expr);

        let expected_ty_expr = TyExpr::ForAll(Box::new(TyExpr::ForAll(Box::new(TyExpr::Arr(
            Box::new(TyExpr::Var(1)),
            Box::new(TyExpr::Arr(
                Box::new(TyExpr::Var(0)),
                Box::new(TyExpr::Var(1)),
            )),
        )))));
        let expected_ty = ty_exh.alloc(&expected_ty_expr);
        assert_eq!(ty_exh.quote(expected_ty), expected_ty_expr);

        let mut tcs = TCS::new(tm_exh, ty_exh);
        let inferred_ty = tcs.infer(gen_const).unwrap();
        assert!(tcs.ty_exh.eq(expected_ty, inferred_ty));
    }

    #[test]
    fn one() {
        let mut ty_exh = TyExhibit::new();
        let mut tm_exh = TmExhibit::new();
        let one_expr = TmExpr::Gen(Box::new(TmExpr::Abs(
            TyExpr::Var(0),
            Box::new(TmExpr::Abs(
                TyExpr::ForAll(Box::new(TyExpr::Arr(
                    Box::new(TyExpr::Var(0)),
                    Box::new(TyExpr::Var(1)),
                ))),
                Box::new(TmExpr::App(
                    Box::new(TmExpr::Inst(Box::new(TmExpr::Var(0)), TyExpr::Var(0))),
                    Box::new(TmExpr::Var(1)),
                )),
            )),
        )));
        let one_const = tm_exh.alloc(&one_expr, &mut ty_exh);
        assert_eq!(tm_exh.quote(one_const, &ty_exh), one_expr);
        let expected_ty_expr = TyExpr::ForAll(Box::new(TyExpr::Arr(
            Box::new(TyExpr::Var(0)),
            Box::new(TyExpr::Arr(
                Box::new(TyExpr::ForAll(Box::new(TyExpr::Arr(
                    Box::new(TyExpr::Var(0)),
                    Box::new(TyExpr::Var(1)),
                )))),
                Box::new(TyExpr::Var(0)),
            )),
        )));
        let expected_ty = ty_exh.alloc(&expected_ty_expr);
        assert_eq!(ty_exh.quote(expected_ty), expected_ty_expr);

        let mut tcs = TCS::new(tm_exh, ty_exh);
        let inferred_ty = tcs.infer(one_const).unwrap();
        assert!(tcs.ty_exh.eq(expected_ty, inferred_ty));
    }
}
