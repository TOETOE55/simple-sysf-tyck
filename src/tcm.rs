use crate::exhibit::{TmExhibit, TyExhibit};
use crate::ast::TyRef;

pub type TCM<T> = Result<T, TCE>;
pub type TCE = &'static str;

pub struct TCS {
    pub tm_exh: TmExhibit,
    pub ty_exh: TyExhibit,
    pub ty_env: Vec<TyRef>,
    pub gamma: Vec<TyRef>,
}

impl TCS {
    pub fn empty(tm_exh: TmExhibit, ty_exh: TyExhibit) -> Self {
        Self { tm_exh, ty_exh, ty_env: vec![], gamma: vec![] }
    }
}