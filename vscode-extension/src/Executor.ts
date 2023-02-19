// bootstrapping the interpreter
import fetch from "node-fetch";
import * as childProcess from "node:child_process";
import * as fs from "fs/promises";

import * as net from "net";

import * as vscode from "vscode";
import { Readable } from "node:stream";
import { finished } from "stream/promises";

type ExecutorHashResponse = {
  hash: string;
  date: Date;
};

export async function latestExecutorHash(): Promise<string> {
  const apiResponse = await fetch(
    `https://editor.darklang.com/latest-executor`,
  );
  let responseBody = (await apiResponse.json()) as ExecutorHashResponse;
  return responseBody.hash;
}

// Match the dotnet runtime identifier which we use in our download string
function getRuntimeIdentifier(): string {
  let platform = "";
  if (process.platform === "win32") {
    platform = "win";
  } else if (process.platform === "darwin") {
    platform = "osx";
  } else if (process.platform === "linux") {
    // TODO: linux-musl
    platform = "linux";
  } else {
    throw new Error("unknown platform");
  }
  let arch = "";
  if (process.arch === "x64") {
    arch = "x64";
  } else if (process.arch === "arm64") {
    arch = "arm64";
  } else {
    throw new Error("unknown arch");
  }

  return `${platform}-${arch}`;
}

export function getFilename(hash: string): string {
  let rid = getRuntimeIdentifier();
  return `darklang-executor-${hash}-${rid}`;
}

/** returns: path (on disk) to downloaded executor */
export async function downloadExecutor(
  filename: string,
  destination: vscode.Uri,
): Promise<void> {
  // Fetch, save, and start the interpreter
  const url = `https://downloads.darklang.com/${filename}`;

  const apiResponse = await fetch(url);
  const buffer = await apiResponse.arrayBuffer();
  const array = new Uint8Array(buffer);

  await vscode.workspace.fs.writeFile(destination, array);
}

export async function downloadLatestExecutor(
  storageUri: vscode.Uri,
): Promise<vscode.Uri> {
  vscode.workspace.fs.createDirectory(storageUri);

  let hash = await latestExecutorHash();
  let filename = getFilename(hash);
  let destination = vscode.Uri.parse(`${storageUri}/${filename}`);

  // Only download if it's not already there
  try {
    await fs.access(destination.fsPath, fs.constants.F_OK);
  } catch {
    await downloadExecutor(filename, destination);
  }
  // Do this regardless in case something went wrong somewhere
  await fs.chmod(destination.fsPath, "755");
  return destination;
}

async function waitUntilTCPConnectionIsReady(port: number): Promise<void> {
  // I've observed it taking 1.5 seconds to start the server up, let's set it to 10s
  // for slow machines
  const host = "localhost";
  let retryCount = 0;

  let checkConnection = async () => {
    return new Promise((resolve, reject) => {
      const client = net.connect(port, host, () => {
        resolve(true);
      });
      client.on("error", () => {
        retryCount++;
        if (retryCount > 100) {
          console.error(`not connected after 10s, fail: ${retryCount}`);
          reject(false);
        } else {
          let callback = async () => {
            resolve(await checkConnection());
          };
          setTimeout(callback, 100);
        }
      });
    });
  };

  let result = await checkConnection();
  if (!result) {
    throw new Error("connection to dark-executor failed");
  }
}

let executorSubprocess: childProcess.ChildProcessWithoutNullStreams;

export async function startExecutorHttpServer(
  executorUri: vscode.Uri,
  port: number,
): Promise<void> {
  // TODO: kill all existing processes
  executorSubprocess = childProcess.spawn(executorUri.fsPath, [
    "serve",
    `--port=${port}`,
  ]);

  executorSubprocess.stdout?.on("data", data => {
    console.log(`darklang-executor stdout: ${data}`);
  });

  executorSubprocess.stderr?.on("data", data => {
    vscode.window.showInformationMessage(`darklang-executor stderr: ${data}`);
  });

  executorSubprocess.on("error", error => {
    vscode.window.showInformationMessage(
      `darklang-executor error: ${error.message}`,
    );
  });

  executorSubprocess.on("close", code => {
    vscode.window.showInformationMessage(`darklang executor exited: ${code}`);
  });

  await waitUntilTCPConnectionIsReady(port);
}

export async function stopExecutor(): Promise<void> {
  executorSubprocess.kill();
}

export async function evalDarklang(
  port: number,
  main: string,
  vars: string[],
  functions: string,
  types: string,
): Promise<string> {
  const apiResponse = await fetch(
    `http://localhost:${port}/api/v0/execute-text`,
    {
      method: "POST",
      body: JSON.stringify({
        code: code,
        symtable: symtable,
        functions: functions,
        types: types,
      }),
    },
  );

  return await apiResponse.text();
}
type ExecutorVersion = {
  hash: string;
  buildDate: Date;
  inDevelopment: boolean;
};

export async function getVersion(port: number): Promise<ExecutorVersion> {
  const apiResponse = await fetch(`http://localhost:${port}/version`, {
    method: "GET",
  });

  return (await apiResponse.json()) as ExecutorVersion;
}
