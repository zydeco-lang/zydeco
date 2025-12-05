use super::syntax::*;

pub struct AssemblyArena {
    /// All programs are attached with a ProgId.
    pub programs: ArenaSparse<ProgId, Program>,
    /// All variables are named.
    pub variables: ArenaSparse<VarId, VarName>,

    /// Programs are (optionally) labeled.
    pub labels: ArenaAssoc<ProgId, Label>,
    /// Programs are annotated with the corresponding context.
    pub contexts: ArenaAssoc<ProgId, Context>,

    /// The whole object has an optional entry point.
    pub entry: ArenaAssoc<ProgId, ()>,
}

impl AssemblyArena {
    pub fn new(alloc: ArcGlobalAlloc) -> Self {
        Self {
            programs: ArenaSparse::new(alloc.alloc()),
            variables: ArenaSparse::new(alloc.alloc()),
            labels: ArenaAssoc::new(),
            contexts: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
        }
    }
}
impl AsMut<AssemblyArena> for AssemblyArena {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        self
    }
}

pub trait AssemblyArenaLike {
    /// Allocate a program that is named, i.e. has a meaningful label
    fn prog_named(&mut self, prog: impl Into<Program>, ctx: Context, label: Label) -> ProgId;
    /// Allocate a program that is anonymous, i.e. has no meaningful label
    fn prog_anon(&mut self, prog: impl Into<Program>, ctx: Context) -> ProgId;
    /// Allocate a fix point program, e.g. a recursive function.
    fn prog_fix_point(
        &mut self, prog: impl FnOnce(&mut Self, ProgId) -> Program, ctx: Context, label: Label,
    ) -> ProgId;
    /// Allocate an instruction.
    fn instr(
        &mut self, instr: impl Into<Instruction>, ctx: Context,
        kont: impl FnOnce(Context, &mut Self) -> ProgId,
    ) -> ProgId;
}

impl<T> AssemblyArenaLike for T
where
    T: AsMut<AssemblyArena>,
{
    fn prog_named(&mut self, prog: impl Into<Program>, ctx: Context, label: Label) -> ProgId {
        let this = &mut *self.as_mut();
        let id = this.programs.alloc(prog.into());
        this.contexts.insert(id, ctx);
        this.labels.insert(id, label);
        id
    }
    fn prog_anon(&mut self, prog: impl Into<Program>, ctx: Context) -> ProgId {
        let this = &mut *self.as_mut();
        let id = this.programs.alloc(prog.into());
        this.contexts.insert(id, ctx);
        id
    }
    fn prog_fix_point(
        &mut self, prog: impl FnOnce(&mut Self, ProgId) -> Program, ctx: Context, label: Label,
    ) -> ProgId {
        let this = &mut *self.as_mut();
        let prog = prog;
        let id = this.programs.alloc(Program::Panic(Panic));
        let prog = prog(self, id);
        let this = &mut *self.as_mut();
        this.labels.insert(id, label);
        this.contexts[&id] = ctx;
        this.programs[&id] = prog;
        id
    }
    fn instr(
        &mut self, instr: impl Into<Instruction>, ctx: Context,
        kont: impl FnOnce(Context, &mut Self) -> ProgId,
    ) -> ProgId {
        let next = kont(ctx.clone(), self);
        let this = &mut *self.as_mut();
        let id = this.prog_anon(Program::Instruction(instr.into(), next), ctx);
        id
    }
}
