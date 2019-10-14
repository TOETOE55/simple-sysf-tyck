pub type TyRef = usize;
pub type TmRef = usize;

pub type DBI = usize;

#[derive(Eq, Clone, Copy, PartialEq, Debug)]
pub enum Ty {
    Var(DBI),
    ForAll(TyRef),
    Arr(TyRef, TyRef),
}

#[derive(Eq, Clone, Copy, PartialEq, Debug)]
pub enum Tm {
    Var(DBI),
    Abs(TyRef, TmRef),
    App(TmRef, TmRef),
    Gen(TmRef),
    Inst(TmRef, TyRef),
}
