use web_sys::HtmlTextAreaElement;
use yew::prelude::*;

use zydeco_lang::{utils::fmt::FmtDefault, Zydeco};

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
        Callback::from(move |_: MouseEvent| {
            match (Zydeco { title: String::from("input"), verbose: false })
                .run(&cur_buf_hdl)
            {
                Ok((_b, v)) => {
                    // Note: to display type of `v`, use `_b.fmt()`
                    display_hdl.set(format!("{}", v.fmt()))
                }
                Err(()) => display_hdl.set(String::from("Error!")),
            }
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

fn main() {
    yew::start_app::<ZydecoUI>();
}
