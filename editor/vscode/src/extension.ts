import { constants } from "node:fs";
import { access } from "node:fs/promises";
import * as path from "node:path";
import { spawn } from "node:child_process";
import {
  ExtensionContext,
  ProgressLocation,
  window,
  workspace,
} from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

const SERVER_NAME = "cajun";
const INSTALL_REPOSITORY = "https://github.com/zydeco-lang/zydeco.git";

let client: LanguageClient | undefined;

function formatterInitializationOptions(): Record<string, unknown> {
  const config = workspace.getConfiguration("cajun");
  return {
    format: {
      lineWidth: config.get<number>("format.lineWidth", 100),
      layoutIntentions: config.get<string>("format.layoutIntentions", "preserve"),
    },
  };
}

export async function activate(context: ExtensionContext): Promise<void> {
  const trace = window.createOutputChannel("Cajun LSP Trace");
  context.subscriptions.push(trace);

  const command = await CajunExecutable.resolve();
  if (!command) {
    return;
  }

  const executable: Executable = {
    command,
    options: { env: { ...process.env } },
  };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "zydeco" },
    ],
    traceOutputChannel: trace,
    initializationOptions: formatterInitializationOptions(),
  };

  client = new LanguageClient(
    "cajun",
    "Cajun Zydeco Language Server",
    serverOptions,
    clientOptions,
  );
  await client.start();
}

export async function deactivate(): Promise<void> {
  const activeClient = client;
  client = undefined;
  await activeClient?.stop();
}

class CajunExecutable {
  static async resolve(): Promise<string | undefined> {
    const configured = workspace
      .getConfiguration("cajun")
      .get<string>("server.path", "")
      .trim() || (process.env.CAJUN_SERVER_PATH ?? "").trim();
    const command = configured || SERVER_NAME;
    const resolved = await this.find(command);
    if (resolved) {
      return resolved;
    }

    if (configured) {
      window.showErrorMessage(
        `Cajun was not found at the configured path: ${configured}`,
      );
      return undefined;
    }

    const install = await window.showErrorMessage(
      "Cajun is required for Zydeco language support.",
      "Install with Cargo",
    );
    if (install !== "Install with Cargo") {
      return undefined;
    }

    try {
      await this.install();
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      window.showErrorMessage(`Failed to install Cajun: ${message}`);
      return undefined;
    }

    const installed = await this.find(SERVER_NAME);
    if (!installed) {
      window.showErrorMessage(
        "Cargo installed Cajun, but it is not visible on PATH. Configure cajun.server.path explicitly.",
      );
    }
    return installed;
  }

  private static async find(command: string): Promise<string | undefined> {
    if (path.isAbsolute(command) || path.dirname(command) !== ".") {
      return (await this.isExecutable(command)) ? command : undefined;
    }

    const directories = (process.env.PATH ?? "")
      .split(path.delimiter)
      .filter((directory) => directory.length > 0);
    const suffixes = process.platform === "win32"
      ? path.extname(command)
        ? [""]
        : ["", ...(process.env.PATHEXT ?? ".EXE;.CMD;.BAT;.COM").split(";")]
      : [""];
    const candidates = directories.flatMap((directory) =>
      suffixes.map((suffix) => path.join(directory, command + suffix)),
    );
    const matches = await Promise.all(
      candidates.map(async (candidate) =>
        (await this.isExecutable(candidate)) ? candidate : undefined
      ),
    );
    return matches.find((candidate) => candidate !== undefined);
  }

  private static async isExecutable(candidate: string): Promise<boolean> {
    try {
      await access(candidate, process.platform === "win32" ? constants.F_OK : constants.X_OK);
      return true;
    } catch {
      return false;
    }
  }

  private static async install(): Promise<void> {
    await window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: "Installing Cajun",
        cancellable: false,
      },
      () => new Promise<void>((resolve, reject) => {
        const cargo = spawn(
          "cargo",
          [
            "install",
            "--git",
            INSTALL_REPOSITORY,
            "cajun",
            "--bin",
            "cajun",
            "--locked",
          ],
          { stdio: ["ignore", "ignore", "pipe"] },
        );
        const errors: Buffer[] = [];
        cargo.stderr.on("data", (chunk: Buffer) => errors.push(chunk));
        cargo.on("error", reject);
        cargo.on("close", (code) => {
          if (code === 0) {
            resolve();
          } else {
            const details = Buffer.concat(errors).toString("utf8").trim();
            reject(new Error(details || `cargo exited with status ${code}`));
          }
        });
      }),
    );
  }
}
