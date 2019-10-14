use crate::ast::{Tm, TmRef, Ty, TyRef, DBI};

pub struct TyExhibit {
    pub exhibit: Vec<Ty>,
}

impl TyExhibit {
    pub fn new() -> Self {
        Self { exhibit: vec![] }
    }

    pub fn var(&mut self, dbi: DBI) -> TyRef {
        self.exhibit.push(Ty::Var(dbi));
        self.exhibit.len() - 1
    }

    pub fn for_all(&mut self, ty: TyRef) -> TyRef {
        self.exhibit.push(Ty::ForAll(ty));
        self.exhibit.len() - 1
    }

    pub fn arr(&mut self, domain: TyRef, codomain: TyRef) -> TyRef {
        self.exhibit.push(Ty::Arr(domain, codomain));
        self.exhibit.len() - 1
    }

    pub fn eq(&self, a: TyRef, b: TyRef) -> bool {
        match (self.deref(a), self.deref(b)) {
            (Ty::Var(i), Ty::Var(j)) => i == j,
            (Ty::ForAll(t1), Ty::ForAll(t2)) => self.eq(t1, t2),
            (Ty::Arr(ta, tb), Ty::Arr(tc, td)) => self.eq(ta, tc) && self.eq(tb, td),
            _ => false,
        }
    }

    pub fn subst(&mut self, gen: TyRef, spec: TyRef, dbi: DBI) -> TyRef {
        match self.deref(gen) {
            Ty::Var(i) => {
                if i == dbi {
                    spec
                } else {
                    gen
                }
            }
            Ty::ForAll(ty) => {
                let new_ty = self.subst(ty, dbi + 1, spec);
                self.for_all(new_ty)
            }
            Ty::Arr(domain, codomain) => {
                let new_domain = self.subst(domain, dbi + 1, spec);
                let new_codomain = self.subst(codomain, dbi + 1, spec);
                self.arr(new_domain, new_codomain)
            }
        }
    }

    pub fn size(&self) -> usize {
        self.exhibit.len()
    }

    pub fn shrink(&mut self, size: usize) {
        self.exhibit.truncate(size)
    }

    pub fn as_ref(&self, ty: TyRef) -> &Ty {
        &(self.exhibit)[ty]
    }

    pub fn as_mut(&mut self, ty: TyRef) -> &mut Ty {
        &mut (self.exhibit)[ty]
    }

    pub fn deref(&self, ty: TyRef) -> Ty {
        (self.exhibit)[ty]
    }
}

pub struct TmExhibit {
    pub exhibit: Vec<Tm>,
}

impl TmExhibit {
    pub fn new() -> Self {
        Self { exhibit: vec![] }
    }

    pub fn var(&mut self, dbi: DBI) -> TmRef {
        self.exhibit.push(Tm::Var(dbi));
        self.exhibit.len() - 1
    }

    pub fn abs(&mut self, param_ty: TyRef, body: TmRef) -> TmRef {
        self.exhibit.push(Tm::Abs(param_ty, body));
        self.exhibit.len() - 1
    }

    pub fn app(&mut self, a: TmRef, b: TmRef) -> TmRef {
        self.exhibit.push(Tm::App(a, b));
        self.exhibit.len() - 1
    }

    pub fn gen(&mut self, body: TmRef) -> TmRef {
        self.exhibit.push(Tm::Gen(body));
        self.exhibit.len() - 1
    }

    pub fn inst(&mut self, tm: TmRef, ty: TyRef) -> TmRef {
        self.exhibit.push(Tm::Inst(tm, ty));
        self.exhibit.len() - 1
    }

    pub fn eq(&self, a: TmRef, b: TmRef, ty_exh: &TyExhibit) -> bool {
        match (self.deref(a), self.deref(b)) {
            (Tm::Var(i), Tm::Var(j)) => i == j,
            (Tm::Abs(ty1, b1), Tm::Abs(ty2, b2)) => ty_exh.eq(ty1, ty2) && self.eq(b1, b2, ty_exh),
            (Tm::App(ta, tb), Tm::App(tc, td)) => {
                self.eq(ta, tc, ty_exh) && self.eq(tb, td, ty_exh)
            }
            (Tm::Gen(b1), Tm::Gen(b2)) => self.eq(b1, b2, ty_exh),
            (Tm::Inst(tm1, ty1), Tm::Inst(tm2, ty2)) => {
                self.eq(tm1, tm2, ty_exh) && ty_exh.eq(ty1, ty2)
            }
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        self.exhibit.len()
    }

    pub fn shrink(&mut self, size: usize) {
        self.exhibit.truncate(size)
    }

    pub fn as_ref(&self, tm: TmRef) -> &Tm {
        &(self.exhibit)[tm]
    }

    pub fn as_mut(&mut self, tm: TmRef) -> &mut Tm {
        &mut (self.exhibit)[tm]
    }

    pub fn deref(&self, tm: TmRef) -> Tm {
        (self.exhibit)[tm]
    }
}
