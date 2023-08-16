//! Coverage checking algorithm for pa(tt)erns.

use std::fmt::Display;

use im_rc::HashSet;
use vulpi_syntax::{
    elaborated::{Literal, LiteralKind, Pattern, PatternArm, PatternKind},
    r#abstract::Qualified,
};

use crate::{
    r#type::{eval::Eval, TypeKind},
    Context, Env, Real, Type, Virtual,
};

#[derive(Clone)]
pub enum Pat {
    Tuple(Vec<Pat>),
    Constructor(Qualified, Vec<Pat>),
    Wildcard,
    Literal(Literal),
    Or(Box<Pat>, Box<Pat>),
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pat::Tuple(args) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Pat::Constructor(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name.name.get())
                } else {
                    write!(f, "({}", name.name.get())?;
                    for arg in args.iter() {
                        write!(f, " {}", arg)?;
                    }
                    write!(f, ")")
                }
            }
            Pat::Wildcard => write!(f, "_"),
            Pat::Literal(lit) => match &**lit {
                LiteralKind::String(s) => write!(f, "\"{}\"", s.get()),
                LiteralKind::Integer(i) => write!(f, "{}", i.get()),
                LiteralKind::Float(fe) => write!(f, "{}", fe.get()),
                LiteralKind::Char(c) => write!(f, "'{}'", c.get()),
                LiteralKind::Unit => write!(f, "()"),
            },
            Pat::Or(l, r) => {
                write!(f, "{} | {}", l, r)
            }
        }
    }
}

impl Pat {
    pub fn from_pattern(pat: &Pattern) -> Option<Pat> {
        match &**pat {
            PatternKind::Wildcard => Some(Pat::Wildcard),
            PatternKind::Variable(_) => Some(Pat::Wildcard),
            PatternKind::Literal(l) => Some(Pat::Literal(l.clone())),
            PatternKind::Application(p) => Some(Pat::Constructor(
                p.func.clone(),
                p.args
                    .iter()
                    .map(Pat::from_pattern)
                    .collect::<Option<Vec<_>>>()?,
            )),
            PatternKind::Error => None,
            PatternKind::Or(_) => todo!(),
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, Pat::Wildcard)
    }

    pub fn constructor(&self) -> Option<Qualified> {
        match self {
            Pat::Constructor(name, _) => Some(name.clone()),
            _ => None,
        }
    }
}

/// A line in the problem matrix. It's used to indicate that there's an answer to a open pattern
/// problem.
#[derive(Clone)]
pub struct Row<T>(im_rc::Vector<T>);

impl<T: Clone> Row<T> {
    /// Removes the first column of the row
    pub fn pop_front(&self) -> Self {
        let mut line = self.clone();
        line.0.pop_front();
        line
    }

    /// Checks if the line is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Adds an item to the beggining of the Row
    pub fn preppend(&self, item: T) -> Self {
        let mut line = self.clone();
        line.0.push_front(item);
        line
    }

    pub fn split(&self, place: usize) -> (Self, Self) {
        let (left, right) = self.0.clone().split_at(place);
        (Row(left), Row(right))
    }

    pub fn first(&self) -> &T {
        self.0.get(0).unwrap()
    }

    pub fn inline(&self, mut other: Vec<T>) -> Self {
        let mut copied = self.clone();
        copied.0.pop_front();
        other.extend(copied.0);
        Row(other.into())
    }
}

impl Row<Pat> {
    pub fn specialize(&self, useful: Pat) -> Vec<Row<Pat>> {
        let first = &self.0[0];
        match (useful, first) {
            (Pat::Wildcard, Pat::Tuple(args)) => vec![self.inline(wildcards(args.len()))],
            (Pat::Wildcard, Pat::Constructor(_, args)) => vec![self.inline(wildcards(args.len()))],
            (Pat::Wildcard, Pat::Wildcard) => vec![self.pop_front()],
            (Pat::Wildcard, Pat::Literal(_)) => vec![self.pop_front()],

            (Pat::Constructor(n, _), Pat::Constructor(m, args)) if n == *m => {
                vec![self.inline(args.clone())]
            }

            (Pat::Constructor(_, args), Pat::Wildcard) => vec![self.inline(wildcards(args.len()))],

            (Pat::Tuple(a), Pat::Tuple(b)) if a.len() == b.len() => {
                vec![self.inline(b.to_vec())]
            }

            (Pat::Literal(n), Pat::Literal(m)) if n == *m => vec![self.pop_front()],

            (_, _) => vec![],
        }
    }

    pub fn default_row(self) -> Vec<Row<Pat>> {
        let first = &self.0[0];
        match first {
            Pat::Wildcard => vec![self.pop_front()],
            Pat::Or(l, r) => {
                let mut l = self.preppend(*l.clone()).default_row();
                l.extend(self.preppend(*r.clone()).default_row());
                l
            }
            _ => vec![],
        }
    }

    pub fn is_wildcard(&self) -> bool {
        self.0[0].is_wildcard()
    }

    pub fn used_constructor(&self) -> Option<Qualified> {
        self.first().constructor()
    }
}

/// A matrix is a vector of rows.
#[derive(Clone)]
pub struct Matrix<T>(Vec<Row<T>>);

impl Matrix<Pat> {
    pub fn is_wildcard(&self) -> bool {
        self.0.iter().all(|x| x.is_wildcard())
    }

    pub fn used_constructors(&self) -> HashSet<Qualified> {
        self.0.iter().flat_map(|x| x.used_constructor()).collect()
    }

    pub fn specialize(self, useful: Pat) -> Matrix<Pat> {
        Matrix(
            self.0
                .into_iter()
                .flat_map(|x| x.specialize(useful.clone()))
                .collect(),
        )
    }

    pub fn default_matrix(self) -> Matrix<Pat> {
        Matrix(self.0.into_iter().flat_map(|x| x.default_row()).collect())
    }
}

fn wildcards(n: usize) -> Vec<Pat> {
    vec![Pat::Wildcard; n]
}

pub enum Witness {
    Ok,
    NonExhaustive(Row<Pat>),
}

impl Witness {
    pub fn non_exaustive(&self) -> bool {
        matches!(self, Witness::NonExhaustive(_))
    }

    pub fn expand(self, name: Option<Qualified>, size: usize) -> Self {
        let Witness::NonExhaustive(x) = self else {
            return self
        };

        let (left, right) = x.split(size);

        let data = if let Some(name) = name {
            Pat::Constructor(name, left.0.into_iter().collect())
        } else {
            Pat::Tuple(left.0.into_iter().collect())
        };

        let row = right.preppend(data);

        Self::NonExhaustive(row)
    }

    pub fn preppend(self, pat: Pat) -> Self {
        let Witness::NonExhaustive(row) = self else {
            return self
        };

        Witness::NonExhaustive(row.preppend(pat))
    }
}

pub enum Finitude<T> {
    Infinite,
    Finite(T),
}

pub enum Completeness {
    Complete(HashSet<Qualified>),
    Incomplete(Finitude<HashSet<Qualified>>),
}

impl Completeness {
    pub fn check(all: HashSet<Qualified>, used: HashSet<Qualified>) -> Self {
        let diff: HashSet<_> = all.clone().difference(used).into_iter().collect();
        if diff.is_empty() {
            Completeness::Complete(all)
        } else {
            Completeness::Incomplete(Finitude::Finite(diff))
        }
    }

    pub fn infinite() -> Self {
        Completeness::Incomplete(Finitude::Infinite)
    }
}

/// A problem is a matrix, a vector of types and a row that is guiding the specialization
#[derive(Clone)]
pub struct Problem {
    types: Row<Type<Virtual>>,
    case: Row<Pat>,
    matrix: Matrix<Pat>,
}

impl<T: Display + Clone> Display for Row<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for case in self.0.clone().into_iter() {
            write!(f, "{} ", case)?;
        }
        writeln!(f)
    }
}
impl Display for Problem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "|- ")?;
        for case in &self.case.0 {
            write!(f, "{}", case)?;
        }
        writeln!(f)?;

        for row in &self.matrix.0 {
            write!(f, "|  ")?;
            for case in &row.0 {
                write!(f, "{}", case)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Problem {
    pub fn exhaustiveness(arms: &[PatternArm<Type<Real>>], types: Vec<Type<Virtual>>) -> Self {
        let map = arms
            .iter()
            .flat_map(|x| {
                x.patterns
                    .iter()
                    .map(Pat::from_pattern)
                    .collect::<Option<Vec<_>>>()
                    .map(|x| Row(x.into()))
            })
            .collect::<Vec<_>>();

        Self {
            types: Row(types[0..map.get(0).map(|x| x.0.len()).unwrap_or_default()].into()),
            case: Row(wildcards(arms[0].patterns.len()).into()),
            matrix: Matrix(map),
        }
    }

    /// Checks if the pattern matrix is empty (0x0)
    pub fn is_empty(&self) -> bool {
        self.matrix.0.is_empty()
    }

    /// Checks if the pattern matrix contains a line that has no columns (nx0)
    pub fn is_exhaustive(&self) -> bool {
        self.matrix.0.iter().any(|x| x.0.is_empty())
    }

    pub fn specialize(
        self,
        ctx: &mut Context,
        env: Env,
        types: Vec<Type<Virtual>>,
        case_pats: Vec<Pat>,
        case: Pat,
    ) -> Witness {
        let specialized = Self {
            types: self.types.inline(types),
            case: self.case.inline(case_pats),
            matrix: self.matrix.specialize(case),
        };

        specialized.exaustive(ctx, env)
    }

    pub fn default_matrix(self) -> Self {
        Self {
            types: self.types.pop_front(),
            case: self.case.pop_front(),
            matrix: self.matrix.default_matrix(),
        }
    }

    pub fn specialize_wildcard(self, ctx: &mut Context, env: Env) -> Witness {
        self.default_matrix()
            .exaustive(ctx, env)
            .preppend(Pat::Wildcard)
    }

    pub fn specialize_cons(
        self,
        ctx: &mut Context,
        env: Env,
        name: Qualified,
        case_pats: Vec<Pat>,
        args: Vec<Type<Virtual>>,
    ) -> Witness {
        let (signature, _) = ctx.modules.constructor(&name);
        let signature = ctx.instantiate_with_args(&signature.eval(&env), args);

        let spine = signature.arrow_spine();

        let case = Pat::Constructor(name, case_pats.clone());

        self.specialize(ctx, env, spine, case_pats, case)
    }

    pub fn exaustive(self, ctx: &mut Context, env: Env) -> Witness {
        if self.is_empty() {
            Witness::NonExhaustive(self.case)
        } else if self.is_exhaustive() {
            Witness::Ok
        } else {
            self.match_exhaustiveness(ctx, env)
        }
    }

    pub fn is_complete_signature(&self, ctx: &mut Context, type_name: Qualified) -> Completeness {
        let signature = ctx.modules.typ(&type_name);

        if let crate::module::Def::Enum(constructors) = signature.def {
            Completeness::check(
                constructors.into_iter().collect(),
                self.matrix.used_constructors(),
            )
        } else {
            Completeness::infinite()
        }
    }

    pub fn synthetize(&self, ctx: &mut Context, name: Qualified) -> Pat {
        let (_, args) = ctx.modules.constructor(&name);
        Pat::Constructor(name.clone(), wildcards(args))
    }

    pub fn exhaustiveness_wildcard(
        self,
        ctx: &mut Context,
        env: Env,
        type_name: Qualified,
        type_spine: Vec<Type<Virtual>>,
    ) -> Witness {
        if self.matrix.is_wildcard() {
            self.specialize_wildcard(ctx, env)
        } else {
            match self.is_complete_signature(ctx, type_name.clone()) {
                Completeness::Complete(_) => self.split(ctx, env, type_name, type_spine),
                Completeness::Incomplete(Finitude::Finite(cons)) => {
                    let name = cons.into_iter().collect::<Vec<_>>()[0].clone();
                    // let (ty, size) = ctx.modules.constructor(&name);
                    let pat = self.synthetize(ctx, name);
                    // let args = ctx.instantiate_all(&env, &ty.eval(&env)).arrow_spine();
                    // let witness = self.specialize_cons(ctx, env, name, wildcards(size), args);

                    let witness = self.default_matrix().exaustive(ctx, env);

                    witness.preppend(pat)
                }
                Completeness::Incomplete(Finitude::Infinite) => {
                    let witness = self.specialize_wildcard(ctx, env);
                    witness.preppend(Pat::Wildcard)
                }
            }
        }
    }

    pub fn split(
        self,
        ctx: &mut Context,
        env: Env,
        type_name: Qualified,
        type_spine: Vec<Type<Virtual>>,
    ) -> Witness {
        let typ = ctx.modules.typ(&type_name);

        if let crate::module::Def::Enum(constructors) = typ.def {
            for constructor in constructors {
                let (_, size) = ctx.modules.constructor(&constructor);

                let witness = self.clone().specialize_cons(
                    ctx,
                    env.clone(),
                    constructor.clone(),
                    wildcards(size),
                    type_spine.clone(),
                );

                if witness.non_exaustive() {
                    return witness.expand(Some(constructor), size);
                }
            }

            Witness::Ok
        } else {
            unreachable!("This should never happen")
        }
    }

    pub fn exhaustiveness_tuple(
        self,
        ctx: &mut Context,
        env: Env,
        spine: Vec<Type<Virtual>>,
    ) -> Witness {
        let size = spine.len();
        let case_pats = wildcards(size);
        let witness = self.specialize(ctx, env, spine, case_pats.clone(), Pat::Tuple(case_pats));

        if witness.non_exaustive() {
            witness.expand(None, size)
        } else {
            Witness::Ok
        }
    }

    pub fn match_exhaustiveness(self, ctx: &mut Context, env: Env) -> Witness {
        let case = self.case.first();
        let current = self.types.first();

        match (case, current.deref().as_ref()) {
            (Pat::Wildcard, TypeKind::Application(_, _))
            | (Pat::Wildcard, TypeKind::Variable(_)) => {
                let (head, spine) = current.application_spine();
                match head.as_ref() {
                    TypeKind::Variable(name) => {
                        self.exhaustiveness_wildcard(ctx, env, name.clone(), spine)
                    }
                    _ => self.specialize_wildcard(ctx, env),
                }
            }

            (Pat::Wildcard, TypeKind::Tuple(spine)) => {
                let spine = spine.to_vec();
                self.exhaustiveness_tuple(ctx, env, spine)
            }

            (Pat::Wildcard, _) => self.specialize_wildcard(ctx, env),

            (Pat::Constructor(n, pats), TypeKind::Application(_, _)) => {
                let args = current.application_spine().1;
                let name = n.clone();
                let pats = pats.clone();
                self.specialize_cons(ctx, env, name, pats, args)
            }
            (Pat::Tuple(m), TypeKind::Tuple(n)) => {
                let case_pats = m.clone();
                let types = n.clone();
                let size = n.len();

                self.specialize(ctx, env, types, case_pats, Pat::Tuple(wildcards(size)))
            }

            (Pat::Literal(n), _) => {
                let literal_kind = n.clone();
                self.specialize(ctx, env, vec![], vec![], Pat::Literal(literal_kind))
            }

            _ => unreachable!("This should never happen"),
        }
    }
}
