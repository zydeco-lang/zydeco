//! Key-space-independent and delta-offset relative static syntax for separate unit artifacts.

use crate::arena::StaticsArena;
use crate::builtin::BuiltinValueClassifier;
use crate::check::{CopatternStep, CopatternStepShape, Switch};
use crate::surface_syntax as su;
use crate::syntax::*;
use crate::validate::CoveragePattern;

/// Dense arena cursor watermark separating preloaded dependencies from unit-local allocations.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct ArenaOffsets {
    pub kinds: usize,
    pub kpats: usize,
    pub tpats: usize,
    pub types: usize,
    pub vpats: usize,
    pub values: usize,
    pub compus: usize,
    pub absts: usize,
    pub datas: usize,
    pub codatas: usize,
    pub fills: usize,
}

impl ArenaOffsets {
    /// Snapshot the current lengths of every dense arena inside `statics`.
    pub(crate) fn current(statics: &StaticsArena) -> Self {
        Self {
            kinds: statics.kinds_pre.len(),
            kpats: statics.kpats.len(),
            tpats: statics.tpats.len(),
            types: statics.types_pre.len(),
            vpats: statics.vpats.len(),
            values: statics.values.len(),
            compus: statics.compus.len(),
            absts: statics.absts.len(),
            datas: statics.datas.len(),
            codatas: statics.codatas.len(),
            fills: statics.fills.len(),
        }
    }
}

/// Relocatable handle to a typed node allocated in an intrinsic table, dependency, or local unit.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum RelId<Id> {
    Intrinsic(Id),
    DepInterface { dep: u32, offset: u32 },
    DepBody { dep: u32, offset: u32 },
    DepStub { dep: u32, slot: u32 },
    LocalInterface(u32),
    LocalBody(u32),
}

/// Relocatable handle to a source or synthesized variable definition.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum RelDefId {
    DepInterface { dep: u32, offset: u32 },
    LocalInterface(u32),
    Raw(su::DefId),
}

/// Relative hole-fillable node inside a compilation unit body.
#[derive(Clone, Debug)]
pub(crate) enum RelFillable<T> {
    Fill(RelId<FillId>),
    Done(T),
}

#[derive(Clone, Debug)]
pub(crate) enum RelKind {
    VType,
    CType,
    Arrow(RelId<KindId>, RelId<KindId>),
    Label(FieldName, RelId<KindId>),
}

#[derive(Clone, Debug)]
pub(crate) enum RelKindPattern {
    Hole,
    Var(RelDefId),
}

#[derive(Clone, Debug)]
pub(crate) enum RelTypePattern {
    Hole,
    Var(RelDefId),
    Named(FieldName, RelId<TPatId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct RelTypeBinder {
    pub pattern: RelId<TPatId>,
    pub witness: RelId<AbstId>,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelExistsMode {
    Abstract,
    Manifest(RelId<TypeId>),
}

#[derive(Clone, Debug)]
pub(crate) struct RelValueParameter {
    pub domain: RelId<TypeId>,
    pub witnesses: Option<Vec<RelId<AbstId>>>,
    pub witness_projection: PackageWitnessProjection,
}

#[derive(Clone, Debug)]
pub(crate) enum RelValPiBinder {
    Type(RelTypeBinder),
    Value(RelValueParameter),
}

#[derive(Clone, Debug)]
pub(crate) struct RelValPi {
    pub binder: RelValPiBinder,
    pub codomain: RelId<TypeId>,
}

#[derive(Clone, Debug)]
pub(crate) struct RelPackPi {
    pub domain: RelId<TypeId>,
    pub witnesses: Vec<RelId<AbstId>>,
    pub codomain: RelId<TypeId>,
}

#[derive(Clone, Debug)]
pub(crate) enum RelType {
    Var(RelDefId),
    Abst(RelId<AbstId>),
    Abs { binder: RelTypeBinder, body: RelId<TypeId> },
    App(RelId<TypeId>, RelId<TypeId>),
    Named(FieldName, RelId<TypeId>),
    Label(FieldName, RelId<TypeId>),
    Proj(RelId<TypeId>, FieldName),
    Thk,
    Ret,
    Unit,
    Opaque,
    Primitive(PrimitiveType),
    OS,
    ValPi(Box<RelValPi>),
    Arrow(RelId<TypeId>, RelId<TypeId>),
    Forall(RelTypeBinder, RelId<TypeId>),
    PackPi(Box<RelPackPi>),
    Prod(Vec<RelId<TypeId>>),
    Exists { binder: RelTypeBinder, mode: RelExistsMode, body: RelId<TypeId> },
    ManifestKind { binder: RelId<KPatId>, definition: RelId<KindId>, body: RelId<TypeId> },
    Data(RelId<DataId>),
    CoData(RelId<CoDataId>),
}

#[derive(Clone, Debug)]
pub(crate) struct RelData {
    pub arms: Vec<(CtorName, RelId<TypeId>)>,
}

#[derive(Clone, Debug)]
pub(crate) struct RelCoData {
    pub arms: Vec<(DtorName, RelId<TypeId>)>,
}

#[derive(Clone, Debug)]
pub(crate) enum RelStaticPatId {
    Kind(RelId<KPatId>),
    Type(RelId<TPatId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelStaticTermId {
    Kind(RelId<KindId>),
    Type(RelId<TypeId>),
}

#[derive(Clone, Debug)]
pub(crate) enum RelValuePattern {
    Hole,
    Var(RelDefId),
    Named(FieldName, RelId<VPatId>),
    Ctor(CtorName, RelId<VPatId>),
    Lit(Literal),
    Alias(Vec<RelId<VPatId>>),
    Triv,
    VCons(Vec<RelId<VPatId>>),
    SCons(Vec<RelStaticPatId>, RelId<VPatId>),
    View { function: RelId<ValueId>, pattern: RelId<VPatId> },
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelValBinder {
    Type(RelId<TPatId>),
    Value(RelId<VPatId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelValArgument {
    Type(RelId<TypeId>),
    Value(RelId<ValueId>),
}

#[derive(Clone, Debug)]
pub(crate) struct RelResolvedField {
    pub name: FieldName,
    pub products: Vec<(RelId<TypeId>, usize)>,
}

#[derive(Clone, Debug)]
pub(crate) enum RelValue {
    Hole,
    Var(RelDefId),
    Named(FieldName, RelId<ValueId>),
    Let { binder: RelId<VPatId>, bindee: RelId<ValueId>, tail: RelId<ValueId> },
    ValAbs(RelValBinder, RelId<ValueId>),
    ValApp(RelId<ValueId>, RelValArgument),
    Match { scrut: RelId<ValueId>, arms: Vec<(RelId<VPatId>, RelId<ValueId>)> },
    IntOp { operation: ValueIntOp, operands: [RelId<ValueId>; 2] },
    IntrinsicOp(IntrinsicOp),
    Thunk(RelId<CompuId>),
    ThunkStub(u32),
    Ctor(CtorName, RelId<ValueId>),
    Triv,
    VCons(Vec<RelId<ValueId>>),
    SCons(Vec<RelStaticTermId>, RelId<ValueId>),
    Seal(RelId<ValueId>),
    Proj(RelId<ValueId>, RelResolvedField),
    Lit(Literal),
}

#[derive(Clone, Debug)]
pub(crate) enum RelComputation {
    Hole,
    VAbs(RelId<VPatId>, RelId<CompuId>),
    VApp(RelId<CompuId>, RelId<ValueId>),
    TAbs(RelId<TPatId>, RelId<CompuId>),
    TApp(RelId<CompuId>, RelId<TypeId>),
    Fix(RelId<VPatId>, RelId<CompuId>),
    Force(RelId<ValueId>),
    Ret(RelId<ValueId>),
    Do { binder: RelId<VPatId>, bindee: RelId<CompuId>, tail: RelId<CompuId> },
    Let { binder: RelId<VPatId>, bindee: RelId<ValueId>, tail: RelId<CompuId> },
    Match { scrut: RelId<ValueId>, arms: Vec<(RelId<VPatId>, RelId<CompuId>)> },
    CoMatch(Vec<(DtorName, RelId<CompuId>)>),
    Dtor(RelId<CompuId>, DtorName),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelAnnId {
    Set,
    Kind(RelId<KindId>),
    Type(RelId<TypeId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelTermAnnId {
    Hole(RelId<FillId>),
    Kind(RelId<KindId>),
    Type(RelId<TypeId>, RelId<KindId>),
    Value(RelId<ValueId>, RelId<TypeId>),
    Compu(RelId<CompuId>, RelId<TypeId>),
    CompuStub(u32, RelId<TypeId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelTermId {
    Kind(RelId<KindId>),
    Type(RelId<TypeId>),
    Value(RelId<ValueId>),
    Compu(RelId<CompuId>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum RelPatId {
    Kind(RelId<KPatId>),
    Type(RelId<TPatId>),
    Value(RelId<VPatId>),
}

#[derive(Clone, Debug)]
pub(crate) struct RelNominalFamily {
    pub constructor: RelId<TypeId>,
    pub dependencies: Vec<RelId<AbstId>>,
    pub method: DtorName,
}

#[derive(Clone, Debug)]
pub(crate) enum RelObservation {
    HoleSolution { site: InferenceSite, solution: Option<RelAnnId> },
    Debug { metadata: Meta, result: RelTermAnnId },
}

#[derive(Clone, Debug)]
pub(crate) enum RelCoverageError {
    NonExhaustiveMatch {
        term: RelTermId,
        missing: Vec<CoveragePattern>,
        truncated: bool,
    },
    NonExhaustiveCopatternMatch {
        computation: RelId<CompuId>,
        missing: Vec<CoveragePattern>,
        truncated: bool,
    },
    NonExhaustiveCoMatch {
        computation: RelId<CompuId>,
        missing: Vec<DtorName>,
    },
    DuplicateCoMatchArms {
        computation: RelId<CompuId>,
        duplicates: Vec<DtorName>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum RelUnitClassifierError {
    Initializer,
    Unsupported { classifier: RelId<TypeId> },
    Interface(UnitInterfaceError),
}

#[derive(Clone, Debug)]
pub(crate) enum RelForeignClassifierError {
    Abi,
    Unit(RelUnitClassifierError),
    UnsupportedExportParameter { index: usize, classifier: RelId<TypeId> },
    ExpectedThunk { classifier: RelId<TypeId> },
    UnsupportedParameter { index: usize, classifier: RelId<TypeId> },
    ExpectedReturn { classifier: RelId<TypeId> },
    UnsupportedResult { classifier: RelId<TypeId> },
    Signature(ForeignSignatureError),
}

#[derive(Clone, Debug)]
pub(crate) enum RelDuplicateBuiltinRole {
    Type { role: BuiltinTypeRole, witnesses: Vec<RelId<AbstId>> },
    Value { role: BuiltinValueRole, entries: Vec<RelId<TypeId>> },
}

#[derive(Clone, Debug)]
pub(crate) enum RelBuiltinClassifierError {
    Mismatch { role: BuiltinValueRole, entry: RelId<TypeId>, expected: BuiltinValueClassifier },
}

#[derive(Clone, Debug)]
pub(crate) struct RelBuiltinSignatureError {
    pub duplicates: Vec<RelDuplicateBuiltinRole>,
    pub classifiers: Vec<RelBuiltinClassifierError>,
}

#[derive(Clone, Debug)]
pub(crate) enum RelStaticEliminationError {
    UnresolvedApplication { function: RelId<ValueId> },
    UnresolvedMatch { value: RelId<ValueId> },
    UnresolvedInteger { value: RelId<ValueId> },
    RuntimeValue { value: RelId<ValueId>, ty: RelId<TypeId> },
    RuntimeComputation { computation: RelId<CompuId>, ty: RelId<TypeId> },
    ReductionLimit { function: RelId<ValueId> },
}

#[derive(Clone, Debug)]
pub(crate) enum RelTyckTask {
    Pat(su::PatId, Switch<RelAnnId>),
    Term(su::TermId, Switch<RelAnnId>),
    Lub(RelAnnId, RelAnnId),
    SignatureGen(RelAnnId),
    StructureGen(RelAnnId),
    MonadicLiftPat(RelPatId),
    MonadicLiftTerm(RelTermId),
}

#[derive(Clone, Debug)]
pub(crate) enum RelTyckError {
    MissingAnnotation,
    MissingSeal,
    InvalidBindingCycle(su::PatId),
    MissingSolution(Vec<RelId<FillId>>),
    UnconstrainedInference(Vec<RelId<FillId>>),
    OccursCheck(RelId<FillId>),
    MissingStructure(RelId<TypeId>),
    SortMismatch,
    SignatureNotType,
    TypeOfKind,
    KindMismatch,
    TypeMismatch { expected: RelId<TypeId>, found: RelId<TypeId> },
    TypeExpected { expected: String, found: RelId<TypeId> },
    NamedLabelMismatch { expected: FieldName, found: FieldName },
    MissingNamedTypeField { field: FieldName, found: RelId<KindId> },
    AmbiguousNamedTypeField { field: FieldName, found: RelId<KindId> },
    MissingNamedField { field: FieldName, found: RelId<TypeId> },
    DuplicateNamedField { field: FieldName, found: RelId<TypeId> },
    SealedNamedField { field: FieldName, found: RelId<TypeId> },
    StaticNamedField { field: FieldName, found: RelId<TypeId> },
    PatternAliasRequiresValue,
    RefutablePatternAlias,
    RefutableBinding(su::PatId),
    RefutableFieldProjectionPattern,
    UnknownDataConstructor(CtorName),
    UnknownCoDataDestructor(DtorName),
    CopatternStepMismatch { expected: CopatternStepShape, found: CopatternStep },
    OverlappingCopatternClauses,
    MultiplePackPiCopatternClauses,
    NonExhaustiveCopattern { expected: RelId<TypeId> },
    Coverage(RelCoverageError),
    StaticElimination(RelStaticEliminationError),
    PackageWitnessesUnavailable { package: RelId<ValueId> },
    PackageWitnessArityMismatch { expected: usize, found: usize },
    EscapingExistential { witnesses: Vec<RelId<AbstId>>, result: RelId<TypeId> },
    EscapingDeclaration { declarations: Vec<RelId<AbstId>>, result: RelId<TypeId> },
    InvalidBuiltinAttachment { role: BuiltinRole, expected: &'static str },
    InvalidBuiltinSignature(RelBuiltinSignatureError),
    ConflictingBuiltinRole { existing: BuiltinRole, found: BuiltinRole },
    InvalidForeignAttachment,
    InvalidForeignClassifier(RelForeignClassifierError),
    ConflictingForeignImport { existing: ForeignTarget, found: ForeignTarget },
    MissingBuiltinTypeRole { role: BuiltinTypeRole },
    AmbiguousBuiltinTypeRole { role: BuiltinTypeRole, witnesses: Vec<RelId<AbstId>> },
    IntegerLiteralOutOfRange { value: i128, integer_type: IntegerType },
    FloatLiteralOutOfRange { value: f64, float_type: FloatType },
    Expressivity(&'static str),
    NotInlinable(RelDefId),
    NotInlinableSeal(RelId<AbstId>),
}

#[derive(Clone, Debug)]
pub(crate) struct RelErrorEntry {
    pub error: RelTyckError,
    pub stack: Vec<RelTyckTask>,
}
