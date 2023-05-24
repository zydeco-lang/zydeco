use std::path::PathBuf;
use web_sys::HtmlTextAreaElement;
use yew::prelude::*;
use zydeco_lang::{dynamics::syntax as ds, prelude::*, zydeco::ZydecoFile};

const EXAMPLE: &str = "
let f = {
  fn (x: Int) -> ret x
};
! f 3
";

#[function_component]
fn ZydecoUI() -> Html {
    let cur_buf: UseStateHandle<String> = use_state(|| String::from(EXAMPLE));
    let display_text = use_state(|| String::from(""));
    let text_update = {
        let cur_buf_hdl = cur_buf.clone();
        let display_hdl = display_text.clone();
        Callback::from(move |evt: InputEvent| {
            if let Some(text_area) = evt.target_dyn_into::<HtmlTextAreaElement>() {
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

fn run(input: &str) -> Result<String, String> {
    let p = ZydecoFile::parse_src(input, PathBuf::new())?;
    let p = ZydecoFile::elab(p)?;
    ZydecoFile::tyck(p.clone())?;
    let p = ZydecoFile::link(p.inner)?;
    let p = ZydecoFile::eval_os(p, &[]);
    let s = match p.entry {
        ds::ProgKont::Ret(v) => v.fmt(),
        ds::ProgKont::ExitCode(i) => format!("exit code: {}", i),
    };
    Ok(s)
}

fn main() {
    yew::Renderer::<ZydecoUI>::new().render();
}
