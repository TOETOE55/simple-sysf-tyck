use crate::tcm::{TCS, TCM};
use crate::ast::{TmRef, TyRef, Tm, Ty};

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
        },
        _ => Err("expected variable")
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
        },
        _ => Err("expected lambda expression")
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
            if tcs.ty_exh.eq(arg_ty, param_ty) {
                return Err("could not match as the same type");
            }

            Ok(ret_ty)
        }
        _ => Err("excepted application"),
    }
}

fn gen_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Gen(_) => unimplemented!(),
        _ => Err("excepted generalization"),
    }
}

fn inst_rule(tcs: &mut TCS, tm: TmRef) -> TCM<TyRef> {
    match tcs.tm_exh.deref(tm) {
        Tm::Inst(_, _) => unimplemented!(),
        _ => Err("excepted specification"),
    }
}