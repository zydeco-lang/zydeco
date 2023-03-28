use web_sys::HtmlTextAreaElement;
use yew::prelude::*;

const EXAMPLE: &str = "
let f = {
  fn (x: Int) -> ret x
};
! f 3
";

#[function_component(ZydecoUI)]
fn ui() -> Html {
    let cur_buf: UseStateHandle<String> = use_state(|| String::from(EXAMPLE));
    let display_text = use_state(|| String::from(""));
    let text_update = {
        let cur_buf_hdl = cur_buf.clone();
        let display_hdl = display_text.clone();
        Callback::from(move |evt: InputEvent| {
            if let Some(text_area) =
                evt.target_dyn_into::<HtmlTextAreaElement>()
            {
                cur_buf_hdl.set(text_area.value());
                display_hdl.set(String::from(""))
            }
        })
    };

    let run = {
        let cur_buf_hdl: UseStateHandle<String> = cur_buf;
        let display_hdl = display_text.clone();
        Callback::from(move |_: MouseEvent| match run(&cur_buf_hdl) {
            Ok(s) => display_hdl.set(s),
            Err(e) => display_hdl.set(format!("Error: {}", e)),
        })
    };

    let cur_text: &str = display_text.as_ref();
    html! {
        <>
            <h1>{"Zydeco Interpreter"}</h1>
            <textarea oninput = {text_update}></textarea>
            <button onclick = {run}>{"run"}</button>
            <p>{ cur_text }</p>
        </>
    }
}

#[allow(unused)]
fn run(input: &str) -> Result<String, String> {
    // let p = zydeco::parse_prog(input)?;
    // let mut ctx = Ctx::new();
    // let std_decls = declarations::std_decls().expect("std library failure");
    // declarations::inject_ctx(&mut ctx, &std_decls)
    //     .expect("std library failure");
    // let b = zydeco::typecheck_computation(&p.comp, &ctx)?;
    // if b.ctor != TCtor::Ret {
    //     return Err(format!("Your computation had type {}, but the Web interpreter only support computations of type Ret(a)", b));
    // }
    // let mut env = Env::new();
    // builtins::link_builtin(&mut env);
    // linker::link(&mut env, &std_decls);
    // linker::link(&mut env, &p.decls);
    // let v = zydeco::eval_returning_computation(*p.comp, env)?;

    // Ok(format!("{} : {}", v, b.args[0]))
    todo!();
    #[allow(unreachable_code)]
    Ok(String::from(input))
}

fn main() {
    yew::start_app::<ZydecoUI>();
}
