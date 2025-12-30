/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
  languages,
  workspace,
  EventEmitter,
  ExtensionContext,
  window,
  InlayHintsProvider,
  TextDocument,
  CancellationToken,
  Range,
  InlayHint,
  TextDocumentChangeEvent,
  ProviderResult,
  commands,
  WorkspaceEdit,
  TextEdit,
  Selection,
  Uri,
  ProgressLocation,
} from "vscode";

import {
  Disposable,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

import { exec, spawn } from 'child_process';

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  const traceOutputChannel = window.createOutputChannel("Zydeco Language Server Trace");

  const config = workspace.getConfiguration("zydeco-language-server");
  const configuredPath = config.get<string>("server.path");
  const command = configuredPath || process.env.SERVER_PATH || "cajun";

  if (!(await isLanguageServerInstalled(command))) {
    try {
      await installLanguageServer();
    } catch (error) {
      window.showErrorMessage(error.message);
      return;
    }
  }

  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "zydeco" }],
    // synchronize: {
    //   // Notify the server about file changes to '.clientrc files contained in the workspace
    //   fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    // },
    traceOutputChannel,
  };

  // Create the language client and start the client.
  client = new LanguageClient("zydeco-language-server", "Zydeco Language Server", serverOptions, clientOptions);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

function isLanguageServerInstalled(command): Promise<boolean> {
  return new Promise((resolve) => {
    exec(`command -v ${command}`, (error) => {
      // If the command is not found, the error code is 127
      resolve(!error);
    });
  });
}

function installLanguageServer(): Promise<void> {
  return new Promise((resolve, reject) => {
    window.showInformationMessage(
      "No zydeco installation found. Install it via cargo?",
      "Yes", "No"
    ).then((answer) => {
      if (answer !== "Yes") {
        return reject(new Error("Missing zydeco binary \\(シ)/"));
      }
      window.withProgress(
        {
          location: ProgressLocation.Notification,
          title: 'Installing zydeco from cargo (ノ*°▽°*)',
          cancellable: false
        },
        async (progress, _) => {
          await new Promise((resolve: (_: void) => void) => {
            const install = spawn("cargo", [
              "install", "--git",
              "https://github.com/zydeco-lang/zydeco.git"
            ]);
            install.on("exit", (code) => {
              if (code === 0) {
                window.showInformationMessage("Successfully installed zydeco and cajun (❁´◡`❁)");
                resolve();
              } else {
                reject(new Error("Failed to install zydeco and cajun (×_×)"));
              }
            });
          })
        }
      ).then(() => { resolve() })
    });
  });
}
