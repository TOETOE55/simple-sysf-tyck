use crate::ast::{TmExpr, TyExpr};
use crate::exhibit::{TmExhibit, TyExhibit};
use crate::tcm::TCS;

mod ast;
mod check;
mod exhibit;
mod tcm;

fn main() {
    {
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

        println!(
            "{:?}: {:?}",
            tcs.tm_exh.quote(gen_id, &tcs.ty_exh),
            tcs.ty_exh.quote(inferred_ty)
        );
    }

    {
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

        println!(
            "{:?}: {:?}",
            tcs.tm_exh.quote(gen_const, &tcs.ty_exh),
            tcs.ty_exh.quote(inferred_ty)
        );
    }

    {
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

        println!(
            "{:?}: {:?}",
            tcs.tm_exh.quote(one_const, &tcs.ty_exh),
            tcs.ty_exh.quote(inferred_ty)
        );
    }
}
