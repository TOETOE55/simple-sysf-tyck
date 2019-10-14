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

#[derive(Eq, Clone, PartialEq)]
pub enum TyExpr {
    Var(DBI),
    ForAll(Box<TyExpr>),
    Arr(Box<TyExpr>, Box<TyExpr>),
}

#[derive(Eq, Clone, PartialEq)]
pub enum TmExpr {
    Var(DBI),
    Abs(TyExpr, Box<TmExpr>),
    App(Box<TmExpr>, Box<TmExpr>),
    Gen(Box<TmExpr>),
    Inst(Box<TmExpr>, TyExpr),
}

impl std::fmt::Display for TyExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            TyExpr::Var(i) => write!(f, "{}", i),
            TyExpr::ForAll(ty) => write!(f, "(∀. {})", ty),
            TyExpr::Arr(domain, codomain) => write!(f, "({} → {})", domain, codomain),
        }
    }
}

impl std::fmt::Display for TmExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            TmExpr::Var(i) => write!(f, "{}", i),
            TmExpr::Abs(ty, body) => write!(f, "(λ {}. {})", ty, body),
            TmExpr::App(opr, opt) => write!(f, "({} {})", opr, opt),
            TmExpr::Gen(body) => write!(f, "(Λ. {})", body),
            TmExpr::Inst(tm, ty) => write!(f, "({}[{}])", tm, ty),
        }
    }
}
