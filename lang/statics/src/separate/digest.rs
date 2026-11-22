//! Deterministic cryptographic fingerprints for checked compilation-unit interfaces.

use super::AcceptedUnitInterface;
use super::rel::*;
use crate::check::{CopatternStep, CopatternStepShape, Switch};
use crate::syntax::*;
use crate::validate::CoveragePattern;
use sha3::{Digest, Sha3_256};
use zydeco_foundation::dense::DenseId;
use zydeco_surface::arena::ArenaId;

/// Span-free, key-space-free digest identifying a unit's exported semantics and its dependency interfaces.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InterfaceDigest(pub(crate) [u8; 32]);

struct HashWriter(Sha3_256);

impl HashWriter {
    fn new(tag: u8) -> Self {
        let mut h = Sha3_256::new();
        h.update([tag]);
        Self(h)
    }

    fn finish(self) -> InterfaceDigest {
        InterfaceDigest(self.0.finalize().into())
    }

    fn u8(&mut self, value: u8) {
        self.0.update([value]);
    }

    fn bool(&mut self, value: bool) {
        self.u8(u8::from(value));
    }

    fn u32(&mut self, value: u32) {
        self.0.update(value.to_le_bytes());
    }

    fn usize(&mut self, value: usize) {
        self.0.update((value as u64).to_le_bytes());
    }

    fn i128(&mut self, value: i128) {
        self.0.update(value.to_le_bytes());
    }

    fn f64(&mut self, value: f64) {
        self.0.update(value.to_bits().to_le_bytes());
    }

    fn bytes(&mut self, data: &[u8]) {
        self.usize(data.len());
        self.0.update(data);
    }

    fn str(&mut self, value: &str) {
        self.bytes(value.as_bytes());
    }

    fn var(&mut self, name: &VarName) {
        self.str(name.as_str());
    }

    fn field(&mut self, name: &FieldName) {
        self.str(name.as_str());
    }

    fn ctor(&mut self, name: &CtorName) {
        self.str(name.as_str());
    }

    fn dtor(&mut self, name: &DtorName) {
        self.str(name.as_str());
    }

    fn slice<T>(&mut self, items: &[T], mut f: impl FnMut(&mut Self, &T)) {
        self.usize(items.len());
        for item in items {
            f(self, item);
        }
    }

    fn option<T>(&mut self, opt: Option<&T>, f: impl FnOnce(&mut Self, &T)) {
        match opt {
            | None => self.u8(0),
            | Some(value) => {
                self.u8(1);
                f(self, value);
            }
        }
    }

    fn rel_id<Id: DenseId>(&mut self, id: RelId<Id>) {
        match id {
            | RelId::Intrinsic(raw) => {
                self.u8(0);
                self.u32(raw.raw_u32());
            }
            | RelId::DepInterface { dep, offset } => {
                self.u8(1);
                self.u32(dep);
                self.u32(offset);
            }
            | RelId::DepBody { dep, offset } => {
                self.u8(2);
                self.u32(dep);
                self.u32(offset);
            }
            | RelId::DepStub { dep, slot } => {
                self.u8(3);
                self.u32(dep);
                self.u32(slot);
            }
            | RelId::LocalInterface(offset) => {
                self.u8(4);
                self.u32(offset);
            }
            | RelId::LocalBody(offset) => {
                self.u8(5);
                self.u32(offset);
            }
        }
    }

    fn rel_def(&mut self, def: RelDefId) {
        match def {
            | RelDefId::DepInterface { dep, offset } => {
                self.u8(0);
                self.u32(dep);
                self.u32(offset);
            }
            | RelDefId::LocalInterface(offset) => {
                self.u8(1);
                self.u32(offset);
            }
            | RelDefId::Raw(raw) => {
                self.u8(2);
                self.u32(raw.raw().into_u32());
            }
        }
    }

    fn rel_ann(&mut self, ann: RelAnnId) {
        match ann {
            | RelAnnId::Set => self.u8(0),
            | RelAnnId::Kind(k) => {
                self.u8(1);
                self.rel_id(k);
            }
            | RelAnnId::Type(t) => {
                self.u8(2);
                self.rel_id(t);
            }
        }
    }

    fn rel_term_id(&mut self, term: RelTermId) {
        match term {
            | RelTermId::Kind(k) => {
                self.u8(0);
                self.rel_id(k);
            }
            | RelTermId::Type(t) => {
                self.u8(1);
                self.rel_id(t);
            }
            | RelTermId::Value(v) => {
                self.u8(2);
                self.rel_id(v);
            }
            | RelTermId::Compu(c) => {
                self.u8(3);
                self.rel_id(c);
            }
        }
    }

    fn rel_pat_id(&mut self, pat: RelPatId) {
        match pat {
            | RelPatId::Kind(k) => {
                self.u8(0);
                self.rel_id(k);
            }
            | RelPatId::Type(t) => {
                self.u8(1);
                self.rel_id(t);
            }
            | RelPatId::Value(v) => {
                self.u8(2);
                self.rel_id(v);
            }
        }
    }

    fn rel_term_ann(&mut self, ann: RelTermAnnId) {
        match ann {
            | RelTermAnnId::Hole(f) => {
                self.u8(0);
                self.rel_id(f);
            }
            | RelTermAnnId::Kind(k) => {
                self.u8(1);
                self.rel_id(k);
            }
            | RelTermAnnId::Type(t, k) => {
                self.u8(2);
                self.rel_id(t);
                self.rel_id(k);
            }
            | RelTermAnnId::Value(v, t) => {
                self.u8(3);
                self.rel_id(v);
                self.rel_id(t);
            }
            | RelTermAnnId::Compu(c, t) => {
                self.u8(4);
                self.rel_id(c);
                self.rel_id(t);
            }
            | RelTermAnnId::CompuStub(slot, t) => {
                self.u8(5);
                self.u32(slot);
                self.rel_id(t);
            }
        }
    }

    fn literal(&mut self, lit: &Literal) {
        match lit {
            | Literal::Integer(int) => {
                self.u8(0);
                match int {
                    | IntegerLiteral::Int(v) => {
                        self.u8(0);
                        self.i128((*v).into());
                    }
                    | IntegerLiteral::Int8(v) => {
                        self.u8(1);
                        self.u8(*v as u8);
                    }
                    | IntegerLiteral::Int16(v) => {
                        self.u8(2);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::Int32(v) => {
                        self.u8(3);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::Int64(v) => {
                        self.u8(4);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::UInt8(v) => {
                        self.u8(5);
                        self.u8(*v);
                    }
                    | IntegerLiteral::UInt16(v) => {
                        self.u8(6);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::UInt32(v) => {
                        self.u8(7);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::UInt64(v) => {
                        self.u8(8);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::UInt(v) => {
                        self.u8(9);
                        self.0.update(v.to_le_bytes());
                    }
                    | IntegerLiteral::Unresolved(v) => {
                        self.u8(10);
                        self.i128(*v);
                    }
                }
            }
            | Literal::Float(float) => {
                self.u8(1);
                match float {
                    | FloatLiteral::Float32(v) => {
                        self.u8(0);
                        self.0.update(v.to_le_bytes());
                    }
                    | FloatLiteral::Float64(v) => {
                        self.u8(1);
                        self.0.update(v.to_le_bytes());
                    }
                }
            }
            | Literal::Char(c) => {
                self.u8(2);
                self.u32(*c as u32);
            }
            | Literal::String(s) => {
                self.u8(3);
                self.str(s.as_str());
            }
        }
    }

    fn package_witness_projection(&mut self, proj: &PackageWitnessProjection) {
        match proj {
            | PackageWitnessProjection::Ignore => self.u8(0),
            | PackageWitnessProjection::Package { abstracts } => {
                self.u8(1);
                self.usize(*abstracts);
            }
            | PackageWitnessProjection::Product(items) => {
                self.u8(2);
                self.slice(items, Self::package_witness_projection);
            }
            | PackageWitnessProjection::Unpack { prefix, abstracts, body } => {
                self.u8(3);
                self.usize(*prefix);
                self.usize(*abstracts);
                self.package_witness_projection(body);
            }
        }
    }

    fn builtin_role(&mut self, role: BuiltinRole) {
        match role {
            | BuiltinRole::Type(t) => {
                self.u8(0);
                self.str(t.source_name());
            }
            | BuiltinRole::Value(v) => {
                self.u8(1);
                self.str(&v.source_name());
            }
        }
    }

    fn foreign_target(&mut self, target: &ForeignTarget) {
        self.u8(target.abi as u8);
        self.str(target.library.as_str());
        self.str(target.symbol.as_str());
    }

    fn unit_value_type(&mut self, ty: &UnitValueType) {
        match ty {
            | UnitValueType::Unit => self.u8(0),
            | UnitValueType::Integer(i) => {
                self.u8(1);
                self.u8(*i as u8);
            }
            | UnitValueType::Float(f) => {
                self.u8(2);
                self.u8(*f as u8);
            }
            | UnitValueType::Char => self.u8(3),
            | UnitValueType::String => self.u8(4),
            | UnitValueType::Named { name, value } => {
                self.u8(5);
                self.str(name);
                self.unit_value_type(value);
            }
            | UnitValueType::Product(items) => {
                self.u8(6);
                self.slice(items, Self::unit_value_type);
            }
            | UnitValueType::Thunk(stack) => {
                self.u8(7);
                self.unit_stack_type(stack);
            }
        }
    }

    fn unit_stack_type(&mut self, ty: &UnitStackType) {
        match ty {
            | UnitStackType::Arrow(a, b) => {
                self.u8(0);
                self.unit_value_type(a);
                self.unit_stack_type(b);
            }
            | UnitStackType::Return(v) => {
                self.u8(1);
                self.unit_value_type(v);
            }
        }
    }

    fn checked_import(&mut self, import: &CheckedImport) {
        match import {
            | CheckedImport::C(c) => {
                self.u8(0);
                self.foreign_target(&c.target);
                self.slice(c.signature.parameters(), |w, param| match param {
                    | ForeignParameter::Address => w.u8(0),
                    | ForeignParameter::Integer(i) => {
                        w.u8(1);
                        w.u8(*i as u8);
                    }
                });
                match c.signature.result() {
                    | ForeignResult::Unit => self.u8(0),
                    | ForeignResult::Integer(i) => {
                        self.u8(1);
                        self.u8(i as u8);
                    }
                }
            }
            | CheckedImport::Zydeco(z) => {
                self.u8(1);
                self.foreign_target(&z.target);
                self.unit_value_type(&z.exports);
            }
        }
    }

    fn kind(&mut self, kind: &RelKind) {
        match kind {
            | RelKind::VType => self.u8(0),
            | RelKind::CType => self.u8(1),
            | RelKind::Arrow(a, b) => {
                self.u8(2);
                self.rel_id(*a);
                self.rel_id(*b);
            }
            | RelKind::Label(name, inner) => {
                self.u8(3);
                self.field(name);
                self.rel_id(*inner);
            }
        }
    }

    fn type_node(&mut self, ty: &RelType) {
        match ty {
            | RelType::Var(d) => {
                self.u8(0);
                self.rel_def(*d);
            }
            | RelType::Abst(a) => {
                self.u8(1);
                self.rel_id(*a);
            }
            | RelType::Abs { binder, body } => {
                self.u8(2);
                self.rel_id(binder.pattern);
                self.rel_id(binder.witness);
                self.rel_id(*body);
            }
            | RelType::App(f, a) => {
                self.u8(3);
                self.rel_id(*f);
                self.rel_id(*a);
            }
            | RelType::Named(n, t) => {
                self.u8(4);
                self.field(n);
                self.rel_id(*t);
            }
            | RelType::Label(n, t) => {
                self.u8(5);
                self.field(n);
                self.rel_id(*t);
            }
            | RelType::Proj(t, n) => {
                self.u8(6);
                self.rel_id(*t);
                self.field(n);
            }
            | RelType::Thk => self.u8(7),
            | RelType::Ret => self.u8(8),
            | RelType::Unit => self.u8(9),
            | RelType::Opaque => self.u8(10),
            | RelType::Primitive(p) => {
                self.u8(11);
                self.str(p.intrinsic_name());
            }
            | RelType::OS => self.u8(12),
            | RelType::ValPi(pi) => {
                self.u8(13);
                match &pi.binder {
                    | RelValPiBinder::Type(b) => {
                        self.u8(0);
                        self.rel_id(b.pattern);
                        self.rel_id(b.witness);
                    }
                    | RelValPiBinder::Value(p) => {
                        self.u8(1);
                        self.rel_id(p.domain);
                        self.option(p.witnesses.as_ref(), |w, ws| w.slice(ws, |w, a| w.rel_id(*a)));
                        self.package_witness_projection(&p.witness_projection);
                    }
                }
                self.rel_id(pi.codomain);
            }
            | RelType::Arrow(a, b) => {
                self.u8(14);
                self.rel_id(*a);
                self.rel_id(*b);
            }
            | RelType::Forall(b, t) => {
                self.u8(15);
                self.rel_id(b.pattern);
                self.rel_id(b.witness);
                self.rel_id(*t);
            }
            | RelType::PackPi(pi) => {
                self.u8(16);
                self.rel_id(pi.domain);
                self.slice(&pi.witnesses, |w, a| w.rel_id(*a));
                self.rel_id(pi.codomain);
            }
            | RelType::Prod(items) => {
                self.u8(17);
                self.slice(items, |w, t| w.rel_id(*t));
            }
            | RelType::Exists { binder, mode, body } => {
                self.u8(18);
                self.rel_id(binder.pattern);
                self.rel_id(binder.witness);
                match mode {
                    | RelExistsMode::Abstract => self.u8(0),
                    | RelExistsMode::Manifest(d) => {
                        self.u8(1);
                        self.rel_id(*d);
                    }
                }
                self.rel_id(*body);
            }
            | RelType::ManifestKind { binder, definition, body } => {
                self.u8(19);
                self.rel_id(*binder);
                self.rel_id(*definition);
                self.rel_id(*body);
            }
            | RelType::Data(d) => {
                self.u8(20);
                self.rel_id(*d);
            }
            | RelType::CoData(c) => {
                self.u8(21);
                self.rel_id(*c);
            }
        }
    }

    fn value_pattern(&mut self, pat: &RelValuePattern) {
        match pat {
            | RelValuePattern::Hole => self.u8(0),
            | RelValuePattern::Var(d) => {
                self.u8(1);
                self.rel_def(*d);
            }
            | RelValuePattern::Named(f, p) => {
                self.u8(2);
                self.field(f);
                self.rel_id(*p);
            }
            | RelValuePattern::Ctor(c, p) => {
                self.u8(3);
                self.ctor(c);
                self.rel_id(*p);
            }
            | RelValuePattern::Lit(l) => {
                self.u8(4);
                self.literal(l);
            }
            | RelValuePattern::Alias(ps) => {
                self.u8(5);
                self.slice(ps, |w, p| w.rel_id(*p));
            }
            | RelValuePattern::Triv => self.u8(6),
            | RelValuePattern::VCons(ps) => {
                self.u8(7);
                self.slice(ps, |w, p| w.rel_id(*p));
            }
            | RelValuePattern::SCons(sp, p) => {
                self.u8(8);
                self.slice(sp, |w, item| match item {
                    | RelStaticPatId::Kind(k) => {
                        w.u8(0);
                        w.rel_id(*k);
                    }
                    | RelStaticPatId::Type(t) => {
                        w.u8(1);
                        w.rel_id(*t);
                    }
                });
                self.rel_id(*p);
            }
            | RelValuePattern::View { function, pattern } => {
                self.u8(9);
                self.rel_id(*function);
                self.rel_id(*pattern);
            }
        }
    }

    fn value(&mut self, val: &RelValue) {
        match val {
            | RelValue::Hole => self.u8(0),
            | RelValue::Var(d) => {
                self.u8(1);
                self.rel_def(*d);
            }
            | RelValue::Named(f, v) => {
                self.u8(2);
                self.field(f);
                self.rel_id(*v);
            }
            | RelValue::Let { binder, bindee, tail } => {
                self.u8(3);
                self.rel_id(*binder);
                self.rel_id(*bindee);
                self.rel_id(*tail);
            }
            | RelValue::ValAbs(b, v) => {
                self.u8(4);
                match b {
                    | RelValBinder::Type(t) => {
                        self.u8(0);
                        self.rel_id(*t);
                    }
                    | RelValBinder::Value(p) => {
                        self.u8(1);
                        self.rel_id(*p);
                    }
                }
                self.rel_id(*v);
            }
            | RelValue::ValApp(f, a) => {
                self.u8(5);
                self.rel_id(*f);
                match a {
                    | RelValArgument::Type(t) => {
                        self.u8(0);
                        self.rel_id(*t);
                    }
                    | RelValArgument::Value(v) => {
                        self.u8(1);
                        self.rel_id(*v);
                    }
                }
            }
            | RelValue::Match { scrut, arms } => {
                self.u8(6);
                self.rel_id(*scrut);
                self.slice(arms, |w, (p, v)| {
                    w.rel_id(*p);
                    w.rel_id(*v);
                });
            }
            | RelValue::IntOp { operation, operands } => {
                self.u8(7);
                self.u8(*operation as u8);
                self.rel_id(operands[0]);
                self.rel_id(operands[1]);
            }
            | RelValue::Thunk(c) => {
                self.u8(8);
                self.rel_id(*c);
            }
            | RelValue::ThunkStub(slot) => {
                self.u8(9);
                self.u32(*slot);
            }
            | RelValue::Ctor(c, v) => {
                self.u8(10);
                self.ctor(c);
                self.rel_id(*v);
            }
            | RelValue::Triv => self.u8(11),
            | RelValue::VCons(vs) => {
                self.u8(12);
                self.slice(vs, |w, v| w.rel_id(*v));
            }
            | RelValue::SCons(st, v) => {
                self.u8(13);
                self.slice(st, |w, item| match item {
                    | RelStaticTermId::Kind(k) => {
                        w.u8(0);
                        w.rel_id(*k);
                    }
                    | RelStaticTermId::Type(t) => {
                        w.u8(1);
                        w.rel_id(*t);
                    }
                });
                self.rel_id(*v);
            }
            | RelValue::Proj(v, field) => {
                self.u8(14);
                self.rel_id(*v);
                self.field(&field.name);
                self.slice(&field.products, |w, (prod, pos)| {
                    w.rel_id(*prod);
                    w.usize(*pos);
                });
            }
            | RelValue::Lit(l) => {
                self.u8(15);
                self.literal(l);
            }
        }
    }

    fn coverage_pattern(&mut self, pat: &CoveragePattern) {
        match pat {
            | CoveragePattern::Wildcard => self.u8(0),
            | CoveragePattern::Constructor(c, inner) => {
                self.u8(1);
                self.ctor(c);
                self.coverage_pattern(inner);
            }
            | CoveragePattern::Unit => self.u8(2),
            | CoveragePattern::Product(items) => {
                self.u8(3);
                self.slice(items, Self::coverage_pattern);
            }
            | CoveragePattern::Named(f, inner) => {
                self.u8(4);
                self.field(f);
                self.coverage_pattern(inner);
            }
            | CoveragePattern::Package(inner) => {
                self.u8(5);
                self.coverage_pattern(inner);
            }
        }
    }

    fn error_entry(&mut self, entry: &RelErrorEntry) {
        let tag = std::mem::discriminant(&entry.error);
        self.str(&format!("{tag:?}"));
        self.error_payload(&entry.error);
        self.usize(entry.stack.len());
        for task in &entry.stack {
            match task {
                | RelTyckTask::Pat(p, sw) => {
                    self.u8(0);
                    self.u32(p.raw().into_u32());
                    self.switch_tag(sw);
                }
                | RelTyckTask::Term(t, sw) => {
                    self.u8(1);
                    self.u32(t.raw().into_u32());
                    self.switch_tag(sw);
                }
                | RelTyckTask::Lub(a, b) => {
                    self.u8(2);
                    self.rel_ann(*a);
                    self.rel_ann(*b);
                }
                | RelTyckTask::SignatureGen(a) => {
                    self.u8(3);
                    self.rel_ann(*a);
                }
                | RelTyckTask::StructureGen(a) => {
                    self.u8(4);
                    self.rel_ann(*a);
                }
                | RelTyckTask::MonadicLiftPat(p) => {
                    self.u8(5);
                    self.rel_pat_id(*p);
                }
                | RelTyckTask::MonadicLiftTerm(t) => {
                    self.u8(6);
                    self.rel_term_id(*t);
                }
            }
        }
    }

    fn switch_tag(&mut self, sw: &Switch<RelAnnId>) {
        match sw {
            | Switch::Syn => self.u8(0),
            | Switch::Ana(ann) => {
                self.u8(1);
                self.rel_ann(*ann);
            }
        }
    }

    fn error_payload(&mut self, err: &RelTyckError) {
        match err {
            | RelTyckError::MissingSolution(fills)
            | RelTyckError::UnconstrainedInference(fills) => {
                self.u8(1);
                self.slice(fills, |w, f| w.rel_id(*f));
            }
            | RelTyckError::OccursCheck(f) => {
                self.u8(2);
                self.rel_id(*f);
            }
            | RelTyckError::MissingStructure(t)
            | RelTyckError::NonExhaustiveCopattern { expected: t } => {
                self.u8(3);
                self.rel_id(*t);
            }
            | RelTyckError::TypeMismatch { expected, found } => {
                self.u8(4);
                self.rel_id(*expected);
                self.rel_id(*found);
            }
            | RelTyckError::TypeExpected { expected, found } => {
                self.u8(5);
                self.str(expected);
                self.rel_id(*found);
            }
            | RelTyckError::NamedLabelMismatch { expected, found } => {
                self.u8(6);
                self.field(expected);
                self.field(found);
            }
            | RelTyckError::MissingNamedTypeField { field, found }
            | RelTyckError::AmbiguousNamedTypeField { field, found } => {
                self.u8(7);
                self.field(field);
                self.rel_id(*found);
            }
            | RelTyckError::MissingNamedField { field, found }
            | RelTyckError::DuplicateNamedField { field, found }
            | RelTyckError::SealedNamedField { field, found }
            | RelTyckError::StaticNamedField { field, found } => {
                self.u8(8);
                self.field(field);
                self.rel_id(*found);
            }
            | RelTyckError::UnknownDataConstructor(c) => {
                self.u8(9);
                self.ctor(c);
            }
            | RelTyckError::UnknownCoDataDestructor(d) => {
                self.u8(10);
                self.dtor(d);
            }
            | RelTyckError::Coverage(c) => {
                self.u8(11);
                match c {
                    | RelCoverageError::NonExhaustiveMatch { term, missing, truncated } => {
                        self.rel_term_id(*term);
                        self.bool(*truncated);
                        self.slice(missing, Self::coverage_pattern);
                    }
                    | RelCoverageError::NonExhaustiveCopatternMatch {
                        computation,
                        missing,
                        truncated,
                    } => {
                        self.rel_id(*computation);
                        self.bool(*truncated);
                        self.slice(missing, Self::coverage_pattern);
                    }
                    | RelCoverageError::NonExhaustiveCoMatch { computation, missing }
                    | RelCoverageError::DuplicateCoMatchArms {
                        computation,
                        duplicates: missing,
                    } => {
                        self.rel_id(*computation);
                        self.slice(missing, |w, d| w.dtor(d));
                    }
                }
            }
            | RelTyckError::CopatternStepMismatch { expected, found } => {
                self.u8(12);
                self.u8(match expected {
                    | CopatternStepShape::Pattern => 0,
                    | CopatternStepShape::Destructor => 1,
                    | CopatternStepShape::Body => 2,
                });
                match found {
                    | CopatternStep::Pattern => self.u8(0),
                    | CopatternStep::Destructor(d) => {
                        self.u8(1);
                        self.dtor(d);
                    }
                    | CopatternStep::End => self.u8(2),
                }
            }
            | RelTyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                self.u8(13);
                self.i128(*value);
                self.u8(*integer_type as u8);
            }
            | RelTyckError::FloatLiteralOutOfRange { value, float_type } => {
                self.u8(14);
                self.f64(*value);
                self.u8(*float_type as u8);
            }
            | RelTyckError::NotInlinable(d) => {
                self.u8(15);
                self.rel_def(*d);
            }
            | RelTyckError::NotInlinableSeal(a) => {
                self.u8(16);
                self.rel_id(*a);
            }
            | _ => self.u8(0),
        }
    }
}

pub(crate) fn compute_accepted_digest(
    interface: &AcceptedUnitInterface, dep_digests: &[InterfaceDigest],
) -> InterfaceDigest {
    let mut w = HashWriter::new(1);
    w.slice(dep_digests, |w, d| w.0.update(d.0));
    w.rel_term_ann(interface.root);
    w.slice(&interface.local_defs, HashWriter::var);
    w.slice(&interface.kinds, HashWriter::kind);
    w.slice(&interface.kpats, |w, p| match p {
        | RelKindPattern::Hole => w.u8(0),
        | RelKindPattern::Var(d) => {
            w.u8(1);
            w.rel_def(*d);
        }
    });
    w.slice(&interface.tpats, |w, (p, k)| {
        match p {
            | RelTypePattern::Hole => w.u8(0),
            | RelTypePattern::Var(d) => {
                w.u8(1);
                w.rel_def(*d);
            }
            | RelTypePattern::Named(f, inner) => {
                w.u8(2);
                w.field(f);
                w.rel_id(*inner);
            }
        }
        w.rel_id(*k);
    });
    w.slice(&interface.types, |w, (ty, kd)| {
        w.type_node(ty);
        w.rel_id(*kd);
    });
    w.slice(&interface.vpats, |w, (p, t)| {
        w.value_pattern(p);
        w.rel_id(*t);
    });
    w.slice(&interface.values, |w, (v, t)| {
        w.value(v);
        w.rel_id(*t);
    });
    w.slice(&interface.thunk_types, |w, t| w.rel_id(*t));
    w.slice(&interface.absts, |w, a| {
        w.rel_id(a.kind);
        w.option(a.seal.as_ref(), |w, t| w.rel_id(*t));
        w.option(a.hint.as_ref(), |w, d| w.rel_def(*d));
        w.bool(a.existential_skolem);
        w.option(a.builtin_role.as_ref(), |w, r| w.builtin_role(*r));
    });
    w.slice(&interface.datas, |w, d| {
        w.slice(&d.arms, |w, (ctor, t)| {
            w.ctor(ctor);
            w.rel_id(*t);
        });
    });
    w.slice(&interface.codatas, |w, c| {
        w.slice(&c.arms, |w, (dtor, t)| {
            w.dtor(dtor);
            w.rel_id(*t);
        });
    });
    w.slice(&interface.value_builtin_roles, |w, (ty, role)| {
        w.u32(*ty);
        w.builtin_role(BuiltinRole::Value(*role));
    });
    w.slice(&interface.foreign_imports, |w, (val, import)| {
        w.u32(*val);
        w.checked_import(import);
    });
    w.slice(&interface.data_hints, |w, (val, data)| {
        w.u32(*val);
        w.rel_id(*data);
    });
    w.slice(&interface.data_pat_hints, |w, (pat, data)| {
        w.u32(*pat);
        w.rel_id(*data);
    });
    w.slice(&interface.package_pattern_opened_arity, |w, (pat, arity)| {
        w.u32(*pat);
        w.usize(*arity);
    });
    w.slice(&interface.def_facts, |w, (def, fact)| {
        w.u32(*def);
        w.option(fact.annotation.as_ref(), |w, a| w.rel_ann(*a));
        w.option(fact.value_alias.as_ref(), |w, v| w.rel_id(*v));
        w.option(fact.package_alias.as_ref(), |w, ws| {
            w.slice(ws, |w, st| match st {
                | RelStaticTermId::Kind(k) => {
                    w.u8(0);
                    w.rel_id(*k);
                }
                | RelStaticTermId::Type(t) => {
                    w.u8(1);
                    w.rel_id(*t);
                }
            });
        });
        w.option(fact.type_definition.as_ref(), |w, t| w.rel_id(*t));
        w.option(fact.inlinable.as_ref(), |w, v| w.rel_id(*v));
        w.bool(fact.global);
    });
    w.slice(&interface.global_terms, |w, t| w.rel_term_id(*t));
    w.finish()
}

pub(crate) fn compute_rejected_digest(
    errors: &[RelErrorEntry], dep_digests: &[InterfaceDigest],
) -> InterfaceDigest {
    let mut w = HashWriter::new(2);
    w.slice(dep_digests, |w, d| w.0.update(d.0));
    w.slice(errors, HashWriter::error_entry);
    w.finish()
}
