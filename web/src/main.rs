use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::HtmlTextAreaElement;

const EXAMPLE: &str = "
let f = {
  fn (x: Int64) -> ret x
} in
! f 3
";

struct ZydecoUi;

impl ZydecoUi {
    fn mount() -> Result<(), JsValue> {
        let document = web_sys::window()
            .and_then(|window| window.document())
            .ok_or_else(|| JsValue::from_str("browser document is unavailable"))?;
        let body =
            document.body().ok_or_else(|| JsValue::from_str("browser document has no body"))?;

        let heading = document.create_element("h1")?;
        heading.set_text_content(Some("Zydeco Interpreter"));
        body.append_child(&heading)?;

        let textarea = document.create_element("textarea")?.dyn_into::<HtmlTextAreaElement>()?;
        textarea.set_value(EXAMPLE);
        body.append_child(&textarea)?;

        let button = document.create_element("button")?;
        button.set_text_content(Some("run"));
        body.append_child(&button)?;

        let output = document.create_element("p")?;
        body.append_child(&output)?;

        let on_click = Closure::<dyn FnMut(web_sys::MouseEvent)>::new(move |_| {
            let message =
                Self::run(&textarea.value()).unwrap_or_else(|error| format!("Error: {error}"));
            output.set_text_content(Some(&message));
        });
        button.add_event_listener_with_callback("click", on_click.as_ref().unchecked_ref())?;
        on_click.forget();
        Ok(())
    }

    fn run(_input: &str) -> Result<String, String> {
        // let p = ZydecoFile::parse_src(input, std::path::PathBuf::new())?;
        // let p = ZydecoFile::elab(p)?;
        // let ctx = ZydecoFile::tyck(p.clone())?;
        // let p = ZydecoFile::lift(p, ctx.clone())?;
        // let p = ZydecoFile::link(p.inner)?;
        // let p = ZydecoFile::eval_os(p, &[]);
        // let s = match p.entry {
        //     | ds::ProgKont::Ret(v) => v.fmt(),
        //     | ds::ProgKont::ExitCode(i) => format!("exit code: {}", i),
        // };
        Ok(String::new())
    }
}

fn main() {
    ZydecoUi::mount().expect("failed to mount the Zydeco web interface");
}
