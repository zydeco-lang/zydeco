pub mod syntax;

use zydeco_statics::{arena::*, fmt::Formatter, syntax::*};
use syntax::*;

pub fn run(scoped: ScopedArena, statics: StaticsArena) -> String {
    let mut seq = Vec::new();
    let mut scc = scoped.top.clone();
    loop {
        let groups = scc.top();
        if groups.is_empty() {
            break;
        }
        for group in groups {
            seq.extend(group.iter().cloned());
            scc.release(group);
        }
    }

    // remove anything after the first exec
    let exec_idx = seq.iter().position(|decl| {
        let Some(decl) = statics.decls.get(decl) else {
            return false;
        };
        matches!(decl, Declaration::Exec(_))
    });
    seq.truncate(exec_idx.map(|idx| idx + 1).unwrap_or(seq.len()));

    seq.iter()
        .filter_map(|decl| {
            use Declaration as Decl;
            let mut buf = String::new();
            let Some(decl) = statics.decls.get(decl) else {
                return None;
            };
            match decl {
                | Decl::TAliasBody(TAliasBody { binder, bindee: _ }) => binder
                    .pretty(&Formatter::new(&scoped, &statics))
                    .render_fmt(80, &mut buf)
                    .unwrap(),
                | Decl::VAliasBody(VAliasBody { binder, bindee: _ }) => binder
                    .pretty(&Formatter::new(&scoped, &statics))
                    .render_fmt(80, &mut buf)
                    .unwrap(),
                | Decl::VAliasHead(VAliasHead { binder, ty: _ }) => binder
                    .pretty(&Formatter::new(&scoped, &statics))
                    .render_fmt(80, &mut buf)
                    .unwrap(),
                | Decl::Exec(Exec(_)) => buf += "exec",
            }
            Some(buf)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn gen_decls(
    scoped: ScopedArena, statics: StaticsArena, mut decl_iter: impl Iterator<Item = DeclId>,
    mut ext: Vec<String>, mut env: im::HashMap<DefId, usize>, mut res: Vec<Instr>,
) -> Vec<Instr> {
    let Some(decl) = decl_iter.next() else { return res };
    'out: loop {
        let Some(decl) = statics.decls.get(&decl) else { break 'out };
        use Declaration as Decl;
        match decl {
            | Decl::TAliasBody(_) => break 'out,
            | Decl::VAliasBody(VAliasBody { binder, bindee }) => {
                
            }
            | Decl::VAliasHead(VAliasHead { binder, .. }) => {
                use ValuePattern as VPat;
                let VPat::Var(var) = statics.vpat(binder) else {
                    unreachable!()
                };
                let name = scoped.defs[&var].as_str();
                ext.push(name.to_string());
            }
            | Decl::Exec(exec) => todo!(),
        }
        break 'out;
    }
    gen_decls(scoped, statics, decl_iter, ext, env, res)
}

pub fn gen_vpat(scoped: ScopedArena, statics: StaticsArena, vpat: VPatId, mut env: im::HashMap<DefId, usize>, mut res: Vec<Instr>) -> Vec<Instr> {
    res
}

pub fn gen_value(scoped: ScopedArena, statics: StaticsArena, value: ValueId, mut env: im::HashMap<DefId, usize>, mut res: Vec<Instr>) -> Vec<Instr> {
    res
}
