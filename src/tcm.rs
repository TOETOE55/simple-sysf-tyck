use crate::ast::{TmRef, TyRef};
use crate::check::infer;
use crate::exhibit::{TmExhibit, TyExhibit};

pub type TCM<T> = Result<T, TCE>;
pub type TCE = &'static str;

pub struct TCS {
    pub tm_exh: TmExhibit,
    pub ty_exh: TyExhibit,
    pub gamma: Vec<TyRef>,
}

impl TCS {
    pub fn new(tm_exh: TmExhibit, ty_exh: TyExhibit) -> Self {
        Self {
            tm_exh,
            ty_exh,
            gamma: vec![],
        }
    }

    pub fn infer(&mut self, tm: TmRef) -> TCM<TyRef> {
        infer(self, tm)
    }
}
