use super::*;
use crate::representation::RepresentationStrategy;
use std::fmt::Write as _;
use zydeco_stackir::arena::Construct as _;
use zydeco_statics::arena::StaticsScope;

struct Fixture {
    arena: sk::SpsLowArena,
    definitions: IdAllocator<StaticsScope>,
}

impl Default for Fixture {
    fn default() -> Self {
        Self { arena: sk::SpsLowArena::default(), definitions: IdAllocator::new() }
    }
}

impl Fixture {
    fn definition(&mut self, name: &str) -> sk::DefId {
        let id = self.definitions.alloc();
        self.arena.admin.defs.insert_new(id, name.into());
        id
    }

    fn terminal(&mut self) -> sk::CompuId {
        let stack = sk::Bullet.build(&mut self.arena, None);
        sk::SHole(stack).build(&mut self.arena, None)
    }

    fn integer(&mut self, value: i64) -> sk::ValueId {
        sk::Literal::Integer(sk::IntegerLiteral::Int64(value)).build(&mut self.arena, None)
    }

    fn block(&mut self, name: &str, body: sk::CompuId) -> sk::ValueId {
        let label = self.definition(name);
        let environment = sk::Hole.build(&mut self.arena, None);
        let entry = sk::EntryParameters::Closure { environment };
        sk::Block { label, entry, body }.build(&mut self.arena, None)
    }

    fn branches() -> SpsLowProgram {
        let mut fixture = Self::default();
        let arms = ["First", "Second"]
            .into_iter()
            .enumerate()
            .map(|(idx, name)| {
                let binder = sk::Hole.build(&mut fixture.arena, None);
                let binder = sk::Ctor(sk::CtorIdx { idx, name: format!("+{name}").into() }, binder)
                    .build(&mut fixture.arena, None);
                let tail = fixture.terminal();
                let block = fixture.block(name, tail);
                let ignored = sk::Hole.build(&mut fixture.arena, None);
                let tail = fixture.terminal();
                let tail = sk::LetValue { binder: ignored, bindee: block, tail }
                    .build(&mut fixture.arena, None);
                sk::Matcher { binder, tail }
            })
            .collect();
        let payload = fixture.integer(7);
        let scrut = sk::Ctor(sk::CtorIdx { idx: 0, name: "+First".into() }, payload)
            .build(&mut fixture.arena, None);
        let tail = sk::SCoprodMatch { scrut, arms }.build(&mut fixture.arena, None);
        let bindee = sk::Bullet.build(&mut fixture.arena, None);
        let root =
            sk::LetStack { binder: sk::Bullet, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn continuation() -> SpsLowProgram {
        let mut fixture = Self::default();
        let source = fixture.definition("captured");
        let binding = fixture.definition("restored");
        let label = fixture.definition("resume");
        let result = sk::Hole.build(&mut fixture.arena, None);
        let capture = binding.build(&mut fixture.arena, None);
        let environment = sk::VCons::new(vec![capture], sk::ProductLayout { arity: 1 })
            .build(&mut fixture.arena, None);
        let value: sk::ValueId = binding.build(&mut fixture.arena, None);
        let stack = sk::Bullet.build(&mut fixture.arena, None);
        let stack = sk::Cons(value, stack).build(&mut fixture.arena, None);
        let body = sk::SHole(stack).build(&mut fixture.arena, None);
        let entry = sk::EntryParameters::Continuation { result, environment };
        let code = sk::Block { label, entry, body }.build(&mut fixture.arena, None);
        let capture: sk::ValueId = source.build(&mut fixture.arena, None);
        let environment: sk::ValueId =
            sk::VCons::new(vec![capture], sk::ProductLayout { arity: 1 })
                .build(&mut fixture.arena, None);
        let ambient = sk::Bullet.build(&mut fixture.arena, None);
        let residual = sk::Cons(environment, ambient).build(&mut fixture.arena, None);
        let package = sk::ContinuationPackage { code, residual }.build(&mut fixture.arena, None);
        fixture.arena.inner.continuations.insert_new(
            package,
            sk::ContinuationEntry {
                result,
                body,
                captures: vec![sk::CaptureBinding { source, binding }],
            },
        );
        let code = sk::Hole.build(&mut fixture.arena, None);
        let body = fixture.terminal();
        let tail = sk::OpenContinuation { package, code, body }.build(&mut fixture.arena, None);
        let binder = source.build(&mut fixture.arena, None);
        let bindee = fixture.integer(42);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn lower(
        program: &SpsLowProgram, native: bool, policy: RepresentationStrategy,
    ) -> AssemblyBuild {
        let spans = SpanArena::default();
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut lowerer = Lowerer::with_policy(&spans, &scoped, &statics, program, &policy);
        lowerer.native_frames = native;
        lowerer.run()
    }

    /// Keep allocation slots and publication order, erasing only per-run key spaces.
    fn snapshot(build: &AssemblyBuild, input: &SpsLowProgram) -> String {
        let arena = &build.arena;
        let mut names = Vec::new();
        names.extend(
            arena
                .programs
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("p{}", id.raw().into_u32()))),
        );
        names.extend(
            arena
                .variables
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("v{}", id.raw().into_u32()))),
        );
        names.extend(
            arena
                .symbols
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("s{}", id.raw().into_u32()))),
        );
        let mut text = format!("root {:?}\npublished {:?}\n", build.root, arena.publication_order);
        let mut programs = arena.programs.iter().collect::<Vec<_>>();
        programs.sort_by_key(|(id, _)| *id);
        for (id, program) in programs {
            let mut deps = arena.deps.query(id);
            deps.sort_unstable();
            writeln!(text, "{id:?}: {program:?}; context {:?}; deps {deps:?}", arena.contexts[id])
                .unwrap();
        }
        let mut variables = arena.variables.iter().collect::<Vec<_>>();
        variables.sort_by_key(|(id, _)| *id);
        for (id, name) in variables {
            writeln!(text, "{id:?}: {}", name.plain()).unwrap();
        }
        let mut symbols = arena.symbols.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(id, _)| *id);
        for (id, symbol) in symbols {
            writeln!(text, "{id:?}: {symbol:?}").unwrap();
        }
        let mut defs = arena
            .defs
            .iter()
            .map(|(id, def)| (input.arena().admin.defs[id].plain().to_owned(), format!("{def:?}")))
            .collect::<Vec<_>>();
        defs.sort_unstable();
        writeln!(text, "defs {defs:?}").unwrap();
        let mut labels = arena.labels.iter().collect::<Vec<_>>();
        labels.sort_by_key(|(id, _)| **id);
        writeln!(
            text,
            "labels {labels:?}\nframes {:?}\nexterns {:?}",
            arena.frame_entries, arena.externs
        )
        .unwrap();
        for (id, name) in names {
            text = text.replace(&id, &name);
        }
        text
    }
}

#[test]
fn allocation_and_publication_baseline() {
    let mut baseline = String::new();
    for (name, program) in
        [("branches", Fixture::branches()), ("continuation", Fixture::continuation())]
    {
        for native in [false, true] {
            let build = Fixture::lower(&program, native, RepresentationStrategy::Boxed);
            writeln!(baseline, "{name}, native={native}").unwrap();
            baseline.push_str(&Fixture::snapshot(&build, &program));
        }
    }
    assert_eq!(baseline, include_str!("baseline.txt"));
}
