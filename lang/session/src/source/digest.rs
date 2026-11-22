//! Span-free cryptographic fingerprint of parsed textual syntax and metadata.

use crate::source::SourceWarning;
use sha3::{Digest, Sha3_256};
use zydeco_foundation::arena::ArenaId;
use zydeco_foundation::span::Sp;
use zydeco_surface::metadata::{
    DiscoveryRule, DiscoveryRuleKind, ExportSelector, LibraryRole, PackageRelation,
    PackageRelationKind, PackageRole, SourceReference,
};
use zydeco_surface::textual::syntax as t;
use zydeco_surface::textual::{
    DocumentationSite, ImportSite, ImportTarget, LiteralSite, PackageSite,
};
use zydeco_syntax::Meta;

/// Structural hash of one parsed source unit excluding span byte offsets and `--` comments.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SyntaxDigest(pub(crate) [u8; 32]);

struct HashWriter(Sha3_256);

impl HashWriter {
    fn new() -> Self {
        Self(Sha3_256::new())
    }

    fn finish(self) -> SyntaxDigest {
        SyntaxDigest(self.0.finalize().into())
    }

    fn u8(&mut self, value: u8) {
        self.0.update([value]);
    }

    fn u32(&mut self, value: u32) {
        self.0.update(value.to_le_bytes());
    }

    fn u64(&mut self, value: u64) {
        self.0.update(value.to_le_bytes());
    }

    fn i64(&mut self, value: i64) {
        self.0.update(value.to_le_bytes());
    }

    fn i128(&mut self, value: i128) {
        self.0.update(value.to_le_bytes());
    }

    fn usize(&mut self, value: usize) {
        self.0.update((value as u64).to_le_bytes());
    }

    fn bytes(&mut self, data: &[u8]) {
        self.usize(data.len());
        self.0.update(data);
    }

    fn str(&mut self, value: &str) {
        self.bytes(value.as_bytes());
    }

    fn var(&mut self, name: &t::VarName) {
        self.str(name.as_str());
    }

    fn field(&mut self, name: &t::FieldName) {
        self.str(name.as_str());
    }

    fn ctor(&mut self, name: &t::CtorName) {
        self.str(name.as_str());
    }

    fn dtor(&mut self, name: &t::DtorName) {
        self.str(name.as_str());
    }

    fn option<T>(&mut self, opt: Option<&T>, f: impl FnOnce(&mut Self, &T)) {
        match opt {
            | None => self.u8(0),
            | Some(val) => {
                self.u8(1);
                f(self, val);
            }
        }
    }

    fn slice<T>(&mut self, items: &[T], mut f: impl FnMut(&mut Self, &T)) {
        self.usize(items.len());
        for item in items {
            f(self, item);
        }
    }
}

pub(crate) struct SyntaxDigestInput<'a> {
    pub spans: &'a t::SpanArena,
    pub arena: &'a t::TextArena,
    pub unit: t::SourceUnit,
    pub documentation: &'a [DocumentationSite],
    pub warnings: &'a [SourceWarning],
    pub import_sites: &'a [ImportSite],
    pub literals: &'a [LiteralSite],
    pub package_sites: &'a [PackageSite],
    pub discovery: &'a [Sp<DiscoveryRule>],
}

pub(crate) fn compute_syntax_digest(input: SyntaxDigestInput<'_>) -> SyntaxDigest {
    let SyntaxDigestInput {
        spans,
        arena,
        unit,
        documentation,
        warnings,
        import_sites,
        literals,
        package_sites,
        discovery,
    } = input;
    let mut w = HashWriter::new();
    w.option(spans.category_tags().as_ref(), |w, tags| w.bytes(tags));
    w.term(arena, unit.root);
    w.slice(documentation, |w, site| {
        w.u32(site.term.raw().into_u32());
        w.u32(site.payload.raw().into_u32());
        w.slice(&site.directive.meta.arguments, HashWriter::meta_val);
        w.option(site.directive.comment.as_ref(), |w, c| w.str(c.text.as_ref()));
    });
    w.slice(warnings, |w, warn| match warn {
        | SourceWarning::UnattachedText(_) => w.u8(0),
    });
    w.slice(import_sites, |w, site| {
        w.u32(site.term.raw().into_u32());
        w.import_target(&site.directive.target);
    });
    w.slice(literals, |w, site| {
        w.u32(site.term.raw().into_u32());
        w.u32(site.payload.raw().into_u32());
        w.str(site.directive.text.text.as_ref());
    });
    w.slice(package_sites, |w, site| w.package_site(site));
    w.slice(discovery, |w, rule| {
        match rule.inner.kind {
            | DiscoveryRuleKind::Include => w.u8(0),
            | DiscoveryRuleKind::Exclude => w.u8(1),
        }
        w.str(&rule.inner.pattern.to_string());
    });
    w.finish()
}

impl HashWriter {
    fn term(&mut self, arena: &t::TextArena, id: t::TermId) {
        zydeco_foundation::stack::maybe_grow(|| self.term_inner(arena, id));
    }

    fn term_inner(&mut self, arena: &t::TextArena, id: t::TermId) {
        self.u32(id.raw().into_u32());
        match &arena.terms[&id] {
            | t::Term::Meta(t::MetaTerm(meta, body)) => {
                self.u8(0);
                self.meta_node(arena, *meta);
                self.term(arena, *body);
            }
            | t::Term::SourceBoundary(t::SourceBoundary(inner)) => {
                self.u8(1);
                self.term(arena, *inner);
            }
            | t::Term::SignatureBoundary(t::SignatureBoundary(inner)) => {
                self.u8(2);
                self.term(arena, *inner);
            }
            | t::Term::Ann(t::Ann { tm, ty }) => {
                self.u8(3);
                self.term(arena, *tm);
                self.term(arena, *ty);
            }
            | t::Term::Hole(_) => self.u8(4),
            | t::Term::Var(name) => {
                self.u8(5);
                self.var(name);
            }
            | t::Term::Named(t::Named(f, inner)) => {
                self.u8(6);
                self.field(f);
                self.term(arena, *inner);
            }
            | t::Term::Label(t::Label(f, inner)) => {
                self.u8(7);
                self.field(f);
                self.term(arena, *inner);
            }
            | t::Term::Paren(t::Paren(items)) => {
                self.u8(8);
                self.slice(items, |w, child| w.term(arena, *child));
            }
            | t::Term::Abs(t::Abs(cp, body)) => {
                self.u8(9);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::ValAbs(t::Abs(cp, body)) => {
                self.u8(10);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::App(t::Appli(items)) => {
                self.u8(11);
                self.slice(items, |w, child| w.term(arena, *child));
            }
            | t::Term::Fix(t::Fix(p, body)) => {
                self.u8(12);
                self.pat(arena, *p);
                self.term(arena, *body);
            }
            | t::Term::Pi(t::Pi(cp, body)) => {
                self.u8(13);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::ValPi(t::ValPi(cp, body)) => {
                self.u8(14);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::Forall(t::Forall(cp, body)) => {
                self.u8(15);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::Arrow(t::Arrow(a, b)) => {
                self.u8(16);
                self.term(arena, *a);
                self.term(arena, *b);
            }
            | t::Term::Sigma(t::Sigma(cp, body)) => {
                self.u8(17);
                self.copat(arena, *cp);
                self.term(arena, *body);
            }
            | t::Term::Exists(ex) => {
                self.u8(18);
                self.slice(&ex.parameters, |w, param| w.existential_param(arena, param));
                self.term(arena, ex.body);
            }
            | t::Term::Pack(pack) => {
                self.u8(19);
                self.slice(&pack.parameters, |w, param| {
                    w.existential_param(arena, &param.parameter);
                    w.option(param.evidence.as_ref(), |w, ev| w.term(arena, *ev));
                });
                self.term(arena, pack.body);
            }
            | t::Term::Prod(t::Prod(items)) => {
                self.u8(20);
                self.slice(items, |w, child| w.term(arena, *child));
            }
            | other => self.term_tail(arena, other),
        }
    }

    fn term_tail(&mut self, arena: &t::TextArena, term: &t::Term) {
        match term {
            | t::Term::Thunk(t::Thunk(body)) => {
                self.u8(21);
                self.term(arena, *body);
            }
            | t::Term::Force(t::Force(body)) => {
                self.u8(22);
                self.term(arena, *body);
            }
            | t::Term::Ret(t::Return(body)) => {
                self.u8(23);
                self.term(arena, *body);
            }
            | t::Term::Do(t::Bind { binder, bindee, tail }) => {
                self.u8(24);
                self.pat(arena, *binder);
                self.term(arena, *bindee);
                self.term(arena, *tail);
            }
            | t::Term::Let(t::GenLet { binding, tail }) => {
                self.u8(25);
                self.binding(arena, binding);
                self.term(arena, *tail);
            }
            | t::Term::Param(t::Param { flavor, binder, placement, tail }) => {
                self.u8(26);
                self.u8(match flavor {
                    | t::ParameterFlavor::Plain => 0,
                    | t::ParameterFlavor::Value => 1,
                });
                self.pat(arena, *binder);
                self.placement(*placement);
                self.term(arena, *tail);
            }
            | t::Term::Pipeline(t::Pipeline { direction, subject, function }) => {
                self.u8(27);
                self.u8(match direction {
                    | t::PipelineDirection::Forward => 0,
                    | t::PipelineDirection::Backward => 1,
                });
                self.term(arena, *subject);
                self.term(arena, *function);
            }
            | t::Term::ContextBind(t::ContextBind { mode, binding, placement, tail }) => {
                self.u8(28);
                self.u8(match mode {
                    | t::DefinitionMode::Transparent => 0,
                    | t::DefinitionMode::Nominal => 1,
                });
                self.binding(arena, binding);
                self.placement(*placement);
                self.term(arena, *tail);
            }
            | t::Term::Block(t::Block(body)) => {
                self.u8(29);
                self.term(arena, *body);
            }
            | t::Term::Data(t::Data { arms }) => {
                self.u8(30);
                self.slice(arms, |w, arm| {
                    w.ctor(&arm.name);
                    w.term(arena, arm.param);
                });
            }
            | t::Term::CoData(t::CoData { arms }) => {
                self.u8(31);
                self.slice(arms, |w, arm| {
                    w.dtor(&arm.name);
                    w.option(arm.params.as_ref(), |w, cp| w.copat(arena, *cp));
                    w.term(arena, arm.out);
                });
            }
            | t::Term::Ctor(t::Ctor(name, body)) => {
                self.u8(32);
                self.ctor(name);
                self.term(arena, *body);
            }
            | t::Term::Match(t::Match { scrut, arms }) => {
                self.u8(33);
                self.term(arena, *scrut);
                self.slice(arms, |w, arm| {
                    w.pat(arena, arm.binder);
                    w.term(arena, arm.tail);
                });
            }
            | t::Term::CoMatch(t::CoMatchParam { arms }) => {
                self.u8(34);
                self.slice(arms, |w, arm| {
                    w.copat(arena, arm.params);
                    w.term(arena, arm.tail);
                });
            }
            | t::Term::Dtor(t::Dtor(head, name)) => {
                self.u8(35);
                self.term(arena, *head);
                self.dtor(name);
            }
            | t::Term::Proj(t::Proj(head, name)) => {
                self.u8(36);
                self.term(arena, *head);
                self.field(name);
            }
            | t::Term::Lit(lit) => {
                self.u8(37);
                self.literal(lit);
            }
            | _ => unreachable!("covered in term_inner"),
        }
    }

    fn pat(&mut self, arena: &t::TextArena, id: t::PatId) {
        self.u32(id.raw().into_u32());
        match &arena.pats[&id] {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                self.u8(0);
                self.pat(arena, *tm);
                self.term(arena, *ty);
            }
            | t::Pattern::Manifest(m) => {
                self.u8(1);
                self.pat(arena, m.binder);
                self.term(arena, m.definition);
            }
            | t::Pattern::Hole(_) => self.u8(2),
            | t::Pattern::Var(def) => {
                self.u8(3);
                self.u32(def.raw().into_u32());
                self.var(&arena.defs[def]);
            }
            | t::Pattern::Named(t::Named(f, inner)) => {
                self.u8(4);
                self.field(f);
                self.pat(arena, *inner);
            }
            | t::Pattern::Ctor(t::Ctor(c, inner)) => {
                self.u8(5);
                self.ctor(c);
                self.pat(arena, *inner);
            }
            | t::Pattern::Lit(lit) => {
                self.u8(6);
                self.literal(lit);
            }
            | t::Pattern::Project(t::ProjectionPattern(f, inner)) => {
                self.u8(7);
                self.field(f);
                self.pat(arena, *inner);
            }
            | t::Pattern::View(view) => {
                self.u8(8);
                self.term(arena, view.function);
                self.pat(arena, view.pattern);
            }
            | t::Pattern::Alias(t::Alias(t::ConsN(ps, last))) => {
                self.u8(9);
                self.slice(ps, |w, p| w.pat(arena, *p));
                self.pat(arena, *last);
            }
            | t::Pattern::Paren(t::Paren(ps)) => {
                self.u8(10);
                self.slice(ps, |w, p| w.pat(arena, *p));
            }
        }
    }

    fn copat(&mut self, arena: &t::TextArena, id: t::CoPatId) {
        self.u32(id.raw().into_u32());
        match &arena.copats[&id] {
            | t::CoPattern::Pat(p) => {
                self.u8(0);
                self.pat(arena, *p);
            }
            | t::CoPattern::Dtor(d) => {
                self.u8(1);
                self.dtor(d);
            }
            | t::CoPattern::App(t::Appli(items)) => {
                self.u8(2);
                self.slice(items, |w, cp| w.copat(arena, *cp));
            }
        }
    }

    fn meta_node(&mut self, arena: &t::TextArena, id: t::MetaId) {
        self.u32(id.raw().into_u32());
        match &arena.metas[&id] {
            | t::MetaNode::Ident(s) => {
                self.u8(0);
                self.str(s);
            }
            | t::MetaNode::String(s) => {
                self.u8(1);
                self.str(s);
            }
            | t::MetaNode::Integer(v) => {
                self.u8(2);
                self.i64(*v);
            }
            | t::MetaNode::Apply { callee, args } => {
                self.u8(3);
                self.str(callee);
                self.slice(args, |w, arg| w.meta_node(arena, *arg));
            }
        }
    }

    fn existential_param(&mut self, arena: &t::TextArena, param: &t::ExistentialParameter) {
        self.slice(&param.annotations, |w, sp| w.meta_node(arena, sp.inner));
        self.pat(arena, param.binder);
    }

    fn binding(&mut self, arena: &t::TextArena, b: &t::GenBind<t::TermId>) {
        self.u8(match b.flavor {
            | t::BindingFlavor::Plain => 0,
            | t::BindingFlavor::Value => 1,
            | t::BindingFlavor::Computation => 2,
            | t::BindingFlavor::Recursive => 3,
        });
        self.pat(arena, b.binder);
        self.option(b.params.as_ref(), |w, cp| w.copat(arena, *cp));
        self.option(b.ty.as_ref(), |w, ty| w.term(arena, *ty));
        self.term(arena, b.bindee);
    }

    fn placement(&mut self, p: t::Placement) {
        self.u8(match p {
            | t::Placement::In => 0,
            | t::Placement::That => 1,
        });
    }

    fn literal(&mut self, lit: &t::Literal) {
        match lit {
            | t::Literal::Integer(int) => {
                self.u8(0);
                match int {
                    | t::IntegerLiteral::Int(v) => {
                        self.u8(0);
                        self.i128((*v).into());
                    }
                    | t::IntegerLiteral::Int8(v) => {
                        self.u8(1);
                        self.u8(*v as u8);
                    }
                    | t::IntegerLiteral::Int16(v) => {
                        self.u8(2);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::Int32(v) => {
                        self.u8(3);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::Int64(v) => {
                        self.u8(4);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::UInt8(v) => {
                        self.u8(5);
                        self.u8(*v);
                    }
                    | t::IntegerLiteral::UInt16(v) => {
                        self.u8(6);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::UInt32(v) => {
                        self.u8(7);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::UInt64(v) => {
                        self.u8(8);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::UInt(v) => {
                        self.u8(9);
                        self.0.update(v.to_le_bytes());
                    }
                    | t::IntegerLiteral::Unresolved(v) => {
                        self.u8(10);
                        self.i128(*v);
                    }
                }
            }
            | t::Literal::Float(float) => {
                self.u8(1);
                match float {
                    | t::FloatLiteral::Float32(v) => {
                        self.u8(0);
                        self.u32(*v);
                    }
                    | t::FloatLiteral::Float64(v) => {
                        self.u8(1);
                        self.u64(*v);
                    }
                }
            }
            | t::Literal::Char(c) => {
                self.u8(2);
                self.u32(*c as u32);
            }
            | t::Literal::String(s) => {
                self.u8(3);
                self.str(s.as_str());
            }
        }
    }

    fn meta_val(&mut self, meta: &Meta) {
        match meta {
            | Meta::Ident(s) => {
                self.u8(0);
                self.str(s);
            }
            | Meta::String(s) => {
                self.u8(1);
                self.str(s);
            }
            | Meta::Integer(v) => {
                self.u8(2);
                self.i64(*v);
            }
            | Meta::Apply { callee, args } => {
                self.u8(3);
                self.str(callee);
                self.slice(args, HashWriter::meta_val);
            }
        }
    }

    fn import_target(&mut self, target: &ImportTarget) {
        match target {
            | ImportTarget::Source(r) => {
                self.u8(0);
                self.source_ref(r);
            }
            | ImportTarget::Input(n) => {
                self.u8(1);
                self.u64(n.get());
            }
        }
    }

    fn source_ref(&mut self, reference: &SourceReference) {
        match reference {
            | SourceReference::Path(p) => {
                self.u8(0);
                self.str(&p.to_string_lossy());
            }
            | SourceReference::Package(p) => {
                self.u8(1);
                self.str(&p.to_string());
            }
        }
    }

    fn package_site(&mut self, site: &PackageSite) {
        self.u32(site.annotation.raw().into_u32());
        self.u32(site.term.raw().into_u32());
        self.option(site.name.as_ref(), |w, name| w.str(&name.to_string()));
        match &site.role {
            | PackageRole::Library(role) => {
                self.u8(0);
                match role {
                    | LibraryRole::Source => self.u8(0),
                    | LibraryRole::Zydeco => self.u8(1),
                    | LibraryRole::Compiled(c) => {
                        self.u8(2);
                        self.u8(c.abi as u8);
                        self.slice(&c.exports, |w, exp| {
                            match &exp.selector {
                                | ExportSelector::Root => w.u8(0),
                                | ExportSelector::Field(f) => {
                                    w.u8(1);
                                    w.str(&f.to_string());
                                }
                            }
                            w.str(exp.symbol.as_str());
                        });
                    }
                }
            }
            | PackageRole::Binary => self.u8(1),
            | PackageRole::Test => self.u8(2),
        }
        self.slice(&site.relations, |w, sp| w.package_relation(&sp.inner));
    }

    fn package_relation(&mut self, rel: &PackageRelation) {
        match &rel.kind {
            | PackageRelationKind::Test => self.u8(0),
            | PackageRelationKind::TestOf => self.u8(1),
            | PackageRelationKind::Custom(name) => {
                self.u8(2);
                self.str(&name.to_string());
            }
        }
        self.source_ref(&rel.target);
    }
}
