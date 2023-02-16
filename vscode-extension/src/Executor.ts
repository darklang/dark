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

/** returns: path (on disk) to downloaded executor */
export async function downloadExecutor(
  storageUri: vscode.Uri,
  latestExecutorHash: string,
): Promise<vscode.Uri> {
  // TODO: this URL should be a function of the host OS
  let rid = getRuntimeIdentifier();
  let fileToDownload = `darklang-executor-${latestExecutorHash}-${rid}`;

  // Fetch, save, and start the interpreter
  const url = `https://downloads.darklang.com/${fileToDownload}`;

  const apiResponse = await fetch(url);
  const buffer = await apiResponse.arrayBuffer();
  const array = new Uint8Array(buffer);

  let destPathUri = vscode.Uri.parse(`${storageUri}/${fileToDownload}`);

  await vscode.workspace.fs.writeFile(destPathUri, array);
  await fs.chmod(destPathUri.fsPath, "755"); // executable

  return destPathUri;
}

let executorSubprocess: childProcess.ChildProcessWithoutNullStreams;

// Match the dotnet runtime identifier which we use in our download string
function getRuntimeIdentifier(): string {
  let platform = "";
  if (process.platform === "win32") {
    platform = "win";
  } else if (process.platform === "darwin") {
    platform = "osx";
  } else if (process.platform === "linux") {
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

async function waitUntilTCPConnectionIsReady(port: number): Promise<void> {
  const host = "localhost";
  let retryCount = 0;

  let checkConnection = async () => {
    return new Promise((resolve, reject) => {
      const client = net.connect(port, host, () => {
        console.log(`connected to server! ${retryCount}`);
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

  console.log("waiting for cvonnection");
  let result = await checkConnection();
  console.log("waited for cvonnection");
  if (result) {
    console.log("tcp connected");
    return;
  }
  throw new Error("tcp connection failed");
}

// returns the pid?
export async function startExecutorHttpServer(
  executorUri: vscode.Uri,
  port: number,
): Promise<void> {
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

export async function evalSomeCodeAgainstHttpServer(
  port: number,
  code: string,
): Promise<string> {
  const apiResponse = await fetch(
    `http://localhost:${port}/api/v0/execute-text`,
    { method: "POST", body: { code: code, symtable: {} } },
  );

  return await apiResponse.text();
}

export async function stopExecutor(): Promise<void> {
  executorSubprocess.kill();
}
