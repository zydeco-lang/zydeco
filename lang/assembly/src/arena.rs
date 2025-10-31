use super::syntax::*;

pub struct Object {
    /// All programs are attached with a ProgId.
    pub programs: ArenaSparse<ProgId, Program>,
    /// All variables are named.
    pub variables: ArenaSparse<VarId, VarName>,

    /// Programs are (optionally) labeled.
    pub labels: ArenaAssoc<ProgId, Label>,
    /// Programs are annotated with the corresponding context.
    pub contexts: ArenaAssoc<ProgId, Context>,

    /// The whole object has an optional entry point.
    pub entry: Option<ProgId>,
}

impl Object {
    pub fn new(alloc: ArcGlobalAlloc) -> Self {
        Self {
            programs: ArenaSparse::new(alloc.alloc()),
            variables: ArenaSparse::new(alloc.alloc()),
            labels: ArenaAssoc::new(),
            contexts: ArenaAssoc::new(),
            entry: None,
        }
    }
    /// Allocate a program that is named, i.e. has a meaningful label
    pub fn prog_named(&mut self, prog: Program, ctx: Context, label: Label) -> ProgId {
        let id = self.programs.alloc(prog);
        self.contexts.insert(id, ctx);
        self.labels.insert(id, label);
        id
    }
    /// Allocate a program that is anonymous, i.e. has no meaningful label
    pub fn prog_anonymous(&mut self, prog: Program, ctx: Context) -> ProgId {
        let id = self.programs.alloc(prog);
        self.contexts.insert(id, ctx);
        id
    }
    pub fn prog_fix_point(
        &mut self, prog: impl FnOnce(&mut Self, ProgId) -> Program, ctx: Context, label: Label,
    ) -> ProgId {
        let id = self.programs.alloc(Program::Panic(Panic));
        self.labels.insert(id, label);
        self.contexts[&id] = ctx;
        let prog = prog(self, id);
        self.programs[&id] = prog;
        id
    }
}
