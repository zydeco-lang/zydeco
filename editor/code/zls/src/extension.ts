import { type ExtensionContext, window } from "vscode";
import * as path from "path";

import { LanguageClient, LanguageClientOptions, type ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined = undefined;

export function activate(context: ExtensionContext): Promise<void> {
    return startClient(context).catch((e) => {
        // window.showErrorMessage(`Failed to activate zls: ${e}`);
        throw e;
    });
}

function startClient(context: ExtensionContext): Promise<void> {
    // const serverCommand = getServer();
    // const run = {
    //     command: serverCommand,
    //     options: { env: Object.assign({}, process.env, { RUST_BACKTRACE: "1" }) },
    // };
    // const serverOptions: ServerOptions = {
    //     run,
    //     debug: run,
    // };

    // const clientOptions: LanguageClientOptions = {
    //     documentSelector: [{ scheme: "file", language: "zydeco" }],
    // };

    // client = new LanguageClient("zls", "Zydeco Language Server", serverOptions, clientOptions);

    // return client.start();

    // do nothing
    return Promise.resolve();
}

function getServer(): string {
    const windows = process.platform == "win32";
    const suffix = windows ? ".exe" : "";
    const binaryName = "zls" + suffix;

    const bundledPath = path.resolve(__dirname, binaryName);

    return bundledPath;
}
