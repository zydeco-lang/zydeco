export function documentationMarkup(nonce: string): string {
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Zydeco Documentation</title>
<style>
body { max-width: 64rem; padding: 1rem 1.5rem; color: var(--vscode-editor-foreground); background: var(--vscode-editor-background); font-family: var(--vscode-font-family); line-height: 1.55; }
nav { display: flex; gap: .5rem; align-items: center; position: sticky; top: 0; padding: .6rem 0; background: var(--vscode-editor-background); }
button, select { border: 1px solid var(--vscode-button-border, transparent); border-radius: 3px; padding: .3rem .7rem; color: var(--vscode-button-secondaryForeground); background: var(--vscode-button-secondaryBackground); cursor: pointer; }
button:disabled { opacity: .5; cursor: default; } button:focus-visible, select:focus-visible, a:focus-visible { outline: 2px solid var(--vscode-focusBorder); }
h1 { font-size: 1.65rem; font-weight: 600; margin-bottom: .6rem; } h2 { font-size: 1.2rem; } h3 { font-size: 1rem; }
pre { overflow: auto; padding: .85rem; background: var(--vscode-textCodeBlock-background); border-radius: 4px; }
code, pre { font-family: var(--vscode-editor-font-family); font-size: var(--vscode-editor-font-size); }
#signature { white-space: pre-wrap; } a { color: var(--vscode-textLink-foreground); }
section, article { border-top: 1px solid var(--vscode-panel-border); margin-top: 1.2rem; padding-top: .8rem; }
.muted, small { color: var(--vscode-descriptionForeground); } .actions { display: flex; gap: .5rem; }
output { display: block; white-space: pre-wrap; margin: .7rem 0; } table { border-collapse: collapse; } td, th { padding: .3rem .6rem; border: 1px solid var(--vscode-panel-border); }
[hidden] { display: none !important; }
</style></head><body>
<nav aria-label="Documentation navigation"><button id="back" disabled>Back</button><button id="pin">Pin</button><button id="source" disabled>Source</button><span id="mode" class="muted">Following cursor</span></nav>
<p id="status" role="status">Place the cursor on a documented expression.</p>
<main id="content" hidden><h1 id="title"></h1>
<label id="signature-label">Type <select id="signature-kind"><option value="use">At cursor</option><option value="declared">Documented declaration</option></select></label>
<pre id="signature"></pre><div id="sections"></div><section id="examples" hidden><h2>Examples</h2><p class="muted">Checks use complete examples and current project sources. Open a scratch file to edit and inspect one.</p><div id="example-list"></div></section></main>
<script nonce="${nonce}">
const host = acquireVsCodeApi();
const byId = id => document.getElementById(id);
let generation = 0;
let view;
const send = (action, detail = {}) => host.postMessage({ action, generation, ...detail });
const signature = () => {
  const declared = byId('signature-kind').value === 'declared';
  byId('signature').textContent = (declared ? view?.declaredSignature : view?.signature) || 'Type unavailable';
};
byId('signature-kind').addEventListener('change', signature);
for (const action of ['back', 'pin', 'source']) { byId(action).addEventListener('click', () => send(action)); }
document.addEventListener('click', event => {
  const anchor = event.target.closest('a');
  if (anchor) { event.preventDefault(); send('link', { href: anchor.getAttribute('href') }); }
});
window.addEventListener('message', event => {
  const data = event.data;
  generation = data.generation;
  byId('pin').textContent = data.pinned ? 'Unpin' : 'Pin';
  byId('mode').textContent = data.pinned ? 'Pinned to source' : 'Following cursor';
  byId('back').disabled = !data.canBack;
  if (data.kind === 'status') {
    view = undefined;
    byId('status').textContent = data.message;
    byId('status').hidden = false;
    byId('content').hidden = true;
    byId('source').disabled = true;
  } else if (data.kind === 'view') {
    view = data.view;
    byId('status').hidden = true;
    byId('content').hidden = false;
    byId('title').textContent = view.title;
    byId('source').disabled = !view.origin;
    byId('signature-kind').hidden = !view.declaredSignature || view.declaredSignature === view.signature;
    byId('signature-kind').value = view.signature ? 'use' : 'declared';
    signature();
    byId('sections').replaceChildren();
    for (const section of view.sections) {
      const element = document.createElement('section');
      const prose = document.createElement('div');
      // Cajun renders sanitized Markdown: author HTML is text and links are filtered.
      prose.innerHTML = section.html;
      const origin = document.createElement('a');
      origin.textContent = 'Source of this explanation';
      origin.href = section.origin.uri + '#L' + (section.origin.range.start.line + 1) + ',' + (section.origin.range.start.character + 1);
      element.append(prose, origin);
      byId('sections').append(element);
    }
    byId('examples').hidden = view.examples.length === 0;
    byId('example-list').replaceChildren();
    for (const example of view.examples) {
      const article = document.createElement('article');
      const title = document.createElement('h3');
      title.textContent = example.mode;
      const code = document.createElement('pre');
      code.textContent = example.code;
      const actions = document.createElement('div'); actions.className = 'actions';
      const check = document.createElement('button'); check.textContent = 'Check'; check.id = 'check-' + example.id;
      check.addEventListener('click', () => send('check', { id: example.id }));
      const scratch = document.createElement('button'); scratch.textContent = 'Open scratch'; scratch.disabled = example.scratch === null;
      scratch.addEventListener('click', () => send('scratch', { id: example.id }));
      actions.append(check, scratch);
      const output = document.createElement('output'); output.id = 'result-' + example.id; output.setAttribute('aria-live', 'polite');
      article.append(title, code, actions, output); byId('example-list').append(article);
    }
  } else if (data.kind === 'checking' || data.kind === 'checked') {
    const output = byId('result-' + data.example);
    if (output) {
      output.textContent = data.kind === 'checking' ? 'Checking…' : data.message;
      byId('check-' + data.example).disabled = data.kind === 'checking';
    }
  }
});
send('ready');
</script></body></html>`;
}
