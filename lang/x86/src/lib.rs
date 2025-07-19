//! RSP stores the control stack
//! R10 stores the environment stack

pub mod syntax;

// use syntax::*;
// use zydeco_statics::{arena::*, syntax::*};

// pub fn run(scoped: ScopedArena, statics: StaticsArena) -> String {
//     let mut seq = Vec::new();
//     let mut scc = scoped.top.clone();
//     loop {
//         let groups = scc.top();
//         if groups.is_empty() {
//             break;
//         }
//         for group in groups {
//             seq.extend(group.iter().cloned());
//             scc.release(group);
//         }
//     }

//     // remove anything after the first exec
//     let exec_idx = seq.iter().position(|decl| {
//         let Some(decl) = statics.decls.get(decl) else {
//             return false;
//         };
//         matches!(decl, Declaration::Exec(_))
//     });
//     seq.truncate(exec_idx.map(|idx| idx + 1).unwrap_or(seq.len()));

//     let mut res = Vec::new();
//     let mut ext = Vec::new();
//     gen_decls(&scoped, &statics, seq.into_iter(), &mut ext, Env::new(), &mut res);
//     std::iter::empty()
//         .chain([Instr::Section(".text".to_string()), Instr::Global("zydeco_start".to_string())])
//         .chain(ext.into_iter().map(|ext| Instr::Extern(ext)))
//         .chain([Instr::Label("zydeco_start".to_string())])
//         .chain(res.into_iter())
//         .map(|instr| instr.to_string())
//         .collect::<Vec<_>>()
//         .join("\n")
// }

// #[derive(Clone)]
// pub struct Env {
//     map: im::HashMap<DefId, ValueStore>,
//     next: usize,
// }

// impl Env {
//     fn new() -> Self {
//         Self { map: im::HashMap::new(), next: 0 }
//     }
//     fn get(&self, def: &DefId) -> Option<ValueStore> {
//         self.map.get(def).cloned()
//     }
// }

// #[derive(Clone)]
// pub enum ValueStore {
//     Inf,
//     Int(Loc64),
//     Thunk { env: Loc64, code: Loc64 },
//     Triv,
//     Cons(Box<ValueStore>, Box<ValueStore>),
// }

// // Todo: Arg64?
// #[derive(Clone)]
// pub enum Loc64 {
//     Env(usize),
//     Reg(Reg),
//     Label(String),
//     Signed(i64),
// }

// impl Loc64 {
//     fn to_arg64(self) -> Arg64 {
//         match self {
//             | Loc64::Env(idx) => Arg64::Mem(MemRef { reg: Reg::R10, offset: 8 * idx as i32 }),
//             | Loc64::Reg(reg) => Arg64::Reg(reg),
//             | Loc64::Label(label) => Arg64::Label(label),
//             | Loc64::Signed(n) => Arg64::Signed(n),
//         }
//     }
// }

// pub fn gen_decls(
//     scoped: &ScopedArena, statics: &StaticsArena, mut decl_iter: impl Iterator<Item = DeclId>,
//     ext: &mut Vec<String>, mut env: Env, res: &mut Vec<Instr>,
// ) {
//     let Some(decl) = decl_iter.next() else { return };
//     'out: loop {
//         let Some(decl) = statics.decls.get(&decl) else { break 'out };
//         use Declaration as Decl;
//         match decl {
//             | Decl::TAliasBody(_) => break 'out,
//             | Decl::VAliasBody(VAliasBody { binder, bindee }) => {
//                 let bindee = gen_value(scoped, statics, *bindee, env.clone(), res);
//                 env = gen_vpat_assign(scoped, statics, *binder, bindee, env.clone(), res);
//                 break 'out;
//             }
//             | Decl::VAliasHead(VAliasHead { binder, .. }) => {
//                 use ValuePattern as VPat;
//                 let VPat::Var(var) = statics.vpat(binder) else { unreachable!() };
//                 let name = scoped.defs[&var].as_str();
//                 ext.push(name.to_string());
//                 // Fixme: external labels should not be thunked
//                 env.map.insert(
//                     var,
//                     ValueStore::Thunk {
//                         env: Loc64::Signed(0),
//                         code: Loc64::Label(name.to_string()),
//                     },
//                 );
//                 break 'out;
//             }
//             | Decl::Exec(Exec(exec)) => {
//                 gen_compu(scoped, statics, *exec, env, res);
//                 return;
//             }
//         }
//     }
//     gen_decls(scoped, statics, decl_iter, ext, env, res)
// }

// // pub fn type_size(scoped: &ScopedArena, statics: &StaticsArena, ty: TypeId) -> ValueStore {
// //     let Fillable::Done(ty) = statics.r#type(&ty) else { unreachable!() };
// //     match ty {
// //         | Type::Var(def_id) => todo!(),
// //         | Type::Abst(abst_id) => todo!(),
// //         | Type::Abs(abs) => todo!(),
// //         | Type::App(app) => todo!(),
// //         | Type::Thk(thk_ty) => todo!(),
// //         | Type::Ret(ret_ty) => todo!(),
// //         | Type::Unit(unit_ty) => todo!(),
// //         | Type::Int(int_ty) => todo!(),
// //         | Type::Char(char_ty) => todo!(),
// //         | Type::String(string_ty) => todo!(),
// //         | Type::OS(osty) => todo!(),
// //         | Type::Arrow(arrow) => todo!(),
// //         | Type::Forall(forall) => todo!(),
// //         | Type::Prod(prod) => todo!(),
// //         | Type::Exists(exists) => todo!(),
// //         | Type::Data(data_id) => todo!(),
// //         | Type::CoData(co_data_id) => todo!(),
// //     }
// // }

// pub fn gen_vpat_assign(
//     scoped: &ScopedArena, statics: &StaticsArena, vpat: VPatId, value: ValueStore, mut env: Env,
//     res: &mut Vec<Instr>,
// ) -> Env {
//     use ValuePattern as VPat;
//     match statics.vpat(&vpat) {
//         | VPat::Hole(Hole) => {
//             let ValueStore::Inf = value else { unreachable!() };
//             env
//         }
//         | VPat::Var(def) => {
//             let name = scoped.defs[&def].as_str();
//             res.push(Instr::Comment(format!("[write] variable {}", name)));
//             env.map.insert(def, value);
//             env
//         }
//         | VPat::Ctor(ctor) => todo!(),
//         | VPat::Triv(triv) => todo!(),
//         | VPat::VCons(cons) => todo!(),
//         | VPat::TCons(cons) => todo!(),
//     }
// }

// pub fn gen_value(
//     scoped: &ScopedArena, statics: &StaticsArena, value: ValueId, mut env: Env,
//     res: &mut Vec<Instr>,
// ) -> ValueStore {
//     match statics.value(&value) {
//         | Value::Hole(Hole) => {
//             res.push(Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string())));
//             ValueStore::Inf
//         }
//         | Value::Var(def) => {
//             let name = scoped.defs[&def].as_str();
//             res.push(Instr::Comment(format!("[read] variable {}", name)));
//             let Some(store) = env.get(&def) else { unreachable!("{:?}", def) };
//             store
//         }
//         | Value::Thunk(Thunk(body)) => {
//             let kont_label = format!("kont_{}", value.concise_inner());
//             let body_label = format!("body_{}", value.concise_inner());
//             // copy environment to heap and store result in Rax
//             env_to_heap(&env, res);
//             // save environment pointer to environment stack
//             {
//                 let offset = 8 * env.next as i32;
//                 res.push(Instr::Mov(MovArgs::ToMem(
//                     MemRef { reg: Reg::R10, offset },
//                     Reg32::Reg(Reg::Rax),
//                 )));
//                 env.next += 1;
//             }
//             res.push(Instr::Jmp(JmpArgs::Label(kont_label.clone())));
//             // gen body
//             res.push(Instr::Label(body_label.clone()));
//             gen_compu(scoped, statics, body, env, res);
//             res.push(Instr::Label(kont_label));
//             ValueStore::Thunk { env: Loc64::Reg(Reg::Rax), code: Loc64::Label(body_label) }
//         }
//         | Value::Ctor(_ctor) => todo!(),
//         | Value::Triv(Triv) => ValueStore::Triv,
//         | Value::VCons(Cons(a, b)) => {
//             let a = gen_value(scoped, statics, a, env.clone(), res);
//             let b = gen_value(scoped, statics, b, env.clone(), res);
//             ValueStore::Cons(Box::new(a), Box::new(b))
//         }
//         | Value::TCons(Cons(_, body)) => gen_value(scoped, statics, body, env, res),
//         | Value::Lit(lit) => match lit {
//             | Literal::Int(n) => ValueStore::Int(Loc64::Signed(n)),
//             | Literal::String(_) => todo!(),
//             | Literal::Char(_) => todo!(),
//         },
//     }
// }

// pub fn gen_compu(
//     scoped: &ScopedArena, statics: &StaticsArena, compu: CompuId, mut env: Env,
//     res: &mut Vec<Instr>,
// ) {
//     use Computation as Compu;
//     match statics.compu(&compu) {
//         | Compu::Hole(Hole) => {
//             res.extend([Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string()))]);
//         }
//         | Compu::VAbs(Abs(param, body)) => {
//             // Todo: assuming param is a variable
//             let ValuePattern::Var(var) = statics.vpat(&param) else { unreachable!() };
//             // Todo: assuming param is word-sized
//             // pop param from control stack to environment
//             let offset = 8 * env.next as i32;
//             res.push(Instr::Pop(Loc::Mem(MemRef { reg: Reg::R10, offset })));
//             env.map.insert(var, ValueStore::Int(Loc64::Env(env.next)));
//             env.next += 1;
//             // gen body
//             gen_compu(scoped, statics, body, env, res);
//         }
//         | Compu::VApp(App(body, arg)) => {
//             let arg = gen_value(scoped, statics, arg, env.clone(), res);
//             // push arg to control stack
//             fn push(arg: ValueStore, res: &mut Vec<Instr>) {
//                 match arg {
//                     | ValueStore::Inf => {}
//                     | ValueStore::Int(loc) => {
//                         // move to rax and push
//                         res.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, loc.to_arg64())));
//                         res.push(Instr::Push(Arg32::Reg(Reg::Rax)));
//                     }
//                     | ValueStore::Thunk { env, code } => todo!(),
//                     | ValueStore::Triv => {}
//                     | ValueStore::Cons(a, b) => {
//                         push(*a, res);
//                         push(*b, res);
//                     }
//                 }
//             }
//             push(arg, res);
//             gen_compu(scoped, statics, body, env, res);
//         }
//         | Compu::TAbs(Abs(_, body)) => {
//             gen_compu(scoped, statics, body, env, res);
//         }
//         | Compu::TApp(App(body, _)) => {
//             gen_compu(scoped, statics, body, env, res);
//         }
//         | Compu::Fix(fix) => todo!(),
//         | Compu::Force(Force(body)) => {
//             let thunk = gen_value(scoped, statics, body, env, res);
//             let ValueStore::Thunk { env, code } = thunk else { unreachable!() };
//             // recover environment
//             res.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, env.to_arg64())));
//             env_recover(compu, res);
//             // jump to code
//             res.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, code.to_arg64())));
//             res.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
//         }
//         | Compu::Ret(Return(arg)) => {
//             let arg = gen_value(scoped, statics, arg, env.clone(), res);
//             // pop the kontinuation and the environment
//             res.push(Instr::Pop(Loc::Reg(Reg::Rax)));
//         }
//         | Compu::Do(bind) => todo!(),
//         | Compu::Let(_) => todo!(),
//         | Compu::Match(_) => todo!(),
//         | Compu::CoMatch(co_match) => todo!(),
//         | Compu::Dtor(dtor) => todo!(),
//     }
// }

// /// Copy the environment located in [R10] to the heap.
// /// The heap is located in [R11]; allocate enough space by incrementing R11.
// /// Result stored in Rax.
// fn env_to_heap(env: &Env, res: &mut Vec<Instr>) {
//     res.push(Instr::Comment("[copy] environment".to_string()));
//     // calculate space needed by looking at env.next
//     let space = env.next;
//     // save size to [R11]
//     res.push(Instr::Mov(MovArgs::ToMem(
//         MemRef { reg: Reg::R11, offset: 0 },
//         Reg32::Imm(space as i32),
//     )));
//     // copy environment
//     for i in 0..space {
//         // using Rax as temporary
//         res.push(Instr::Mov(MovArgs::ToReg(
//             Reg::Rax,
//             Arg64::Mem(MemRef { reg: Reg::R10, offset: i as i32 }),
//         )));
//         res.push(Instr::Mov(MovArgs::ToMem(
//             MemRef { reg: Reg::R11, offset: (i + 1) as i32 },
//             Reg32::Reg(Reg::Rax),
//         )));
//     }
//     // assign result to Rax
//     res.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R11))));
//     // allocate space
//     res.push(Instr::Add(BinArgs::ToReg(Reg::R11, Arg32::Signed(8 * space as i32))));
// }

// /// Suppose the environment pointer is in Rax.
// /// Recover the environment pointer to R10.
// fn env_recover(site: CompuId, res: &mut Vec<Instr>) {
//     // using Rdi, R8 and R9 as temporary
//     // save size to R9
//     res.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));
//     // overwrite [R10] to be [R11 + 8]
//     let start = format!("env_recover_loop_{}", site.concise_inner());
//     let kont = format!("env_recover_kont_{}", site.concise_inner());
//     res.extend([
//         Instr::Label(start.clone()),
//         // if size is 0, jump to label
//         Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0))),
//         Instr::JCC(ConditionCode::E, JmpArgs::Label(kont.clone())),
//         // otherwise,
//         // R8 = 8 * size + Rax,
//         Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::R9))),
//         Instr::IMul(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))),
//         Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Reg(Reg::Rax))),
//         // R8 = [R8],
//         Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))),
//         // size -= 1,
//         Instr::Sub(BinArgs::ToReg(Reg::R9, Arg32::Signed(1))),
//         // Rdi = 8 * size + R10,
//         Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::R9))),
//         Instr::IMul(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(8))),
//         Instr::Add(BinArgs::ToReg(Reg::Rdi, Arg32::Reg(Reg::R10))),
//         // and [Rdi] = R8
//         Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rdi, offset: 0 }, Reg32::Reg(Reg::R8))),
//         Instr::Jmp(JmpArgs::Label(start)),
//         Instr::Label(kont),
//     ])
// }
