import { randomBytes } from "node:crypto";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import * as path from "node:path";
import {
  CancellationTokenSource, commands, Disposable, env, ExtensionContext, Position,
  Range, TextDocument, TextEditor, Uri, ViewColumn, WebviewPanel, window, workspace,
} from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { DocumentationAnchor } from "./documentation-target";
import { documentationMarkup } from "./documentation-webview";

interface Point { line: number; character: number }
interface SourceRange { start: Point; end: Point }
interface Location { uri: string; range: SourceRange }
interface Target { textDocument: { uri: string }; position: Point }
interface Example { id: number; code: string; mode: string; scratch: string | null }
interface DocumentationView {
  revision: number;
  title: string;
  signature: string | null;
  declaredSignature: string | null;
  range: SourceRange;
  origin: Location | null;
  sections: { origin: Location; markdown: string; html: string }[];
  examples: Example[];
}
interface Verification {
  status: "Passed" | "Failed" | "TimedOut" | { WorkerFailure: string };
  diagnostics: { code: string | null; message: string; path: string | null }[];
}
interface Pin { document: TextDocument; anchor: DocumentationAnchor | undefined }

export class DocumentationPanel implements Disposable {
  private panel?: WebviewPanel;
  private view?: DocumentationView;
  private target?: Target;
  private pin?: Pin;
  private history: Target[] = [];
  private generation = 0;
  private pending?: CancellationTokenSource;
  private timer?: ReturnType<typeof setTimeout>;
  private listeners: Disposable[] = [];
  private supported: boolean;

  constructor(private readonly client: LanguageClient, context: ExtensionContext) {
    this.supported = client.initializeResult?.capabilities.experimental?.zydecoDocumentation?.version === 1;
    this.listeners.push(
      commands.registerCommand("zydeco.showDocumentation", () => this.show()),
      window.onDidChangeTextEditorSelection(event => this.follow(event.textEditor)),
      window.onDidChangeActiveTextEditor(editor => this.follow(editor)),
      workspace.onDidChangeTextDocument(event => {
        if (!this.panel || event.document.languageId !== "zydeco") { return; }
        this.history = this.history.filter(target => target.textDocument.uri !== event.document.uri.toString());
        if (this.pin?.document.uri.toString() === event.document.uri.toString()) {
          if (!this.pin.anchor) { return; }
          const anchor = this.pin.anchor.after(event.contentChanges);
          if (!anchor) {
            this.pin = { document: event.document, anchor: undefined };
            this.invalidate("The pinned occurrence changed. Unpin to follow the cursor again.");
            return;
          }
          this.pin = { document: event.document, anchor };
          this.target = this.at(event.document, event.document.positionAt(anchor.cursor));
        }
        this.schedule();
      }),
      workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration("cajun.hover")) { this.schedule(); }
      }),
    );
    context.subscriptions.push(this);
  }

  refresh(uri?: Uri): void {
    if (uri) {
      this.history = this.history.filter(target => target.textDocument.uri !== uri.toString());
      if (this.pin?.document.uri.toString() === uri.toString() && this.pin.document.isClosed) {
        this.pin = { document: this.pin.document, anchor: undefined };
        this.invalidate("The pinned source changed on disk. Unpin to select it again.");
        return;
      }
    }
    this.schedule();
  }

  dispose(): void {
    this.cancel();
    this.panel?.dispose();
    this.listeners.forEach(listener => listener.dispose());
  }

  private show(): void {
    if (!this.supported) {
      void window.showInformationMessage("This Cajun server does not support the documentation panel. Update Cajun to use it.");
      return;
    }
    if (!this.panel) {
      this.panel = window.createWebviewPanel("zydeco.documentation", "Zydeco Documentation", {
        viewColumn: ViewColumn.Beside, preserveFocus: true,
      }, { enableScripts: true, localResourceRoots: [] });
      this.panel.webview.html = documentationMarkup(randomBytes(18).toString("base64"));
      this.panel.webview.onDidReceiveMessage(message => { void this.message(message); });
      this.panel.onDidDispose(() => {
        this.cancel();
        this.panel = undefined;
        this.view = undefined;
        this.pin = undefined;
        this.target = undefined;
        this.history = [];
      });
    } else {
      this.panel.reveal(ViewColumn.Beside, true);
    }
    this.follow(window.activeTextEditor);
  }

  private at(document: TextDocument, position: Position): Target {
    return { textDocument: { uri: document.uri.toString() }, position: { line: position.line, character: position.character } };
  }

  private follow(editor?: TextEditor): void {
    if (!this.panel || this.pin || !editor) { return; }
    if (editor.document.languageId !== "zydeco" || editor.document.uri.scheme !== "file") {
      this.invalidate("Open a Zydeco source file to follow its documentation.");
      return;
    }
    this.target = this.at(editor.document, editor.selection.active);
    this.schedule();
  }

  private cancel(): void {
    clearTimeout(this.timer);
    this.pending?.cancel();
    this.pending?.dispose();
    this.pending = undefined;
    this.generation += 1;
  }

  private invalidate(message: string): void {
    this.cancel();
    this.view = undefined;
    this.target = undefined;
    this.post({ kind: "status", message });
  }

  private schedule(): void {
    if (!this.panel || !this.target) { return; }
    this.cancel();
    this.view = undefined;
    this.post({ kind: "status", message: "Loading documentation…" });
    this.timer = setTimeout(() => { void this.load(); }, 180);
  }

  private async load(): Promise<void> {
    const target = this.target;
    if (!this.panel || !target) { return; }
    const generation = this.generation;
    const pending = new CancellationTokenSource();
    this.pending = pending;
    try {
      const view = await this.client.sendRequest<DocumentationView | null>("zydeco/documentation", target, pending.token);
      if (generation !== this.generation) { return; }
      this.view = view ?? undefined;
      if (view) {
        this.post({ kind: "view", view });
      } else {
        this.post({ kind: "status", message: "Documentation is unavailable at this position in the current source." });
      }
    } catch (error) {
      if (generation === this.generation) {
        this.post({ kind: "status", message: `Documentation request failed: ${String(error)}` });
      }
    } finally {
      if (this.pending === pending) { this.pending = undefined; }
      pending.dispose();
    }
  }

  private post(message: object): void {
    void this.panel?.webview.postMessage({ ...message, generation: this.generation, pinned: !!this.pin, canBack: this.history.length > 0 });
  }

  private async message(message: unknown): Promise<void> {
    if (typeof message !== "object" || !message) { return; }
    const data = message as Record<string, unknown>;
    if (data.action === "ready") { this.schedule(); return; }
    if (data.generation !== this.generation) { return; }
    try {
      switch (data.action) {
        case "pin":
          if (this.pin) {
            this.pin = undefined;
            this.follow(window.activeTextEditor);
          } else if (this.target && this.view) {
            const target = this.target;
            const view = this.view;
            const document = await workspace.openTextDocument(Uri.parse(target.textDocument.uri));
            if (target !== this.target || view !== this.view) { return; }
            const range = this.range(view.range);
            const cursor = document.offsetAt(new Position(target.position.line, target.position.character));
            this.pin = { document, anchor: new DocumentationAnchor(document.offsetAt(range.start), document.offsetAt(range.end), cursor) };
            this.post({ kind: "controls" });
          }
          return;
        case "back": {
          const previous = this.history.pop();
          if (previous) { await this.navigate(previous); }
          return;
        }
        case "source":
          if (this.view?.origin) { await this.openSource(this.view.origin); }
          return;
        case "link":
          if (typeof data.href === "string") { await this.openLink(data.href); }
          return;
        case "check":
        case "scratch": {
          const example = this.view?.examples.find(example => example.id === data.id);
          if (!example || !this.view || !this.target) { return; }
          if (data.action === "scratch" && example.scratch !== null) {
            const directory = await mkdtemp(path.join(tmpdir(), "zydeco-doc-"));
            const filename = path.join(directory, "example.zydeco");
            await writeFile(filename, example.scratch, "utf8");
            await window.showTextDocument(await workspace.openTextDocument(filename), { preview: false });
          } else if (data.action === "check") {
            await this.check(example.id);
          }
          return;
        }
      }
    } catch (error) {
      void window.showErrorMessage(`Zydeco documentation: ${String(error)}`);
    }
  }

  private async check(example: number): Promise<void> {
    const view = this.view;
    const target = this.target;
    if (!view || !target) { return; }
    const generation = this.generation;
    this.post({ kind: "checking", example });
    let result: Verification | null;
    try {
      result = await this.client.sendRequest<Verification | null>("zydeco/checkDocumentationExample", { revision: view.revision, target, example });
    } catch (error) {
      if (generation === this.generation) { this.post({ kind: "checked", example, message: `Check failed: ${String(error)}` }); }
      return;
    }
    if (generation !== this.generation) { return; }
    if (!result) { this.schedule(); return; }
    const status = typeof result.status === "string" ? result.status : `Worker failed: ${result.status.WorkerFailure}`;
    const details = result.diagnostics.map(diagnostic => [diagnostic.code, diagnostic.message, diagnostic.path].filter(Boolean).join(" · "));
    this.post({ kind: "checked", example, message: [status, ...details].join("\n") });
  }

  private range(range: SourceRange): Range {
    return new Range(range.start.line, range.start.character, range.end.line, range.end.character);
  }

  private async openSource(location: Location): Promise<void> {
    const uri = Uri.parse(location.uri);
    if (uri.scheme !== "file") { return; }
    const start = location.range.start;
    const selection = new Range(start.line, start.character, start.line, start.character);
    await window.showTextDocument(await workspace.openTextDocument(uri), { selection, preview: true, viewColumn: ViewColumn.One });
  }

  private async openLink(href: string): Promise<void> {
    if (href.startsWith("#")) { return; }
    let uri = Uri.parse(href);
    if (!uri.scheme && this.view?.origin) {
      uri = Uri.joinPath(Uri.parse(this.view.origin.uri), "..", href);
    }
    if (uri.scheme === "file") {
      const match = /^L(\d+),(\d+)$/.exec(uri.fragment);
      const position = match ? { line: Math.max(0, Number(match[1]) - 1), character: Math.max(0, Number(match[2]) - 1) } : { line: 0, character: 0 };
      const target = { textDocument: { uri: uri.with({ fragment: "" }).toString() }, position };
      if (this.target) { this.history.push(this.target); }
      await this.navigate(target);
    } else if (["https", "http", "mailto"].includes(uri.scheme)) {
      await env.openExternal(uri);
    }
  }

  private async navigate(target: Target): Promise<void> {
    this.pin = undefined;
    await this.openSource({ uri: target.textDocument.uri, range: { start: target.position, end: target.position } });
    this.target = target;
    this.schedule();
  }
}
