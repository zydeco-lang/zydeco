use web_sys::HtmlTextAreaElement;
use yew::prelude::*;
use zydeco_lang::parse::syntax::TCompute;
use zydeco_lang::zydeco;

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

fn run(input: &str) -> Result<String, String> {
    let p = zydeco::parse_prog(input)?;
    let b = zydeco::typecheck_computation(&p.comp)?;
    let a = match b {
        TCompute::Ret(a, _) => a,
        _ => return Err(format!("Your computation had type {}, but the Web interpreter only support computations of type Ret(a)", b))
    };
    let v = zydeco::eval_returning_computation(*p.comp)?;

    Ok(format!("{} : {}", v, a))
}

fn main() {
    yew::start_app::<ZydecoUI>();
}
