// bootstrapping the interpreter
import fetch from "node-fetch";
import * as childProcess from "node:child_process";
import * as fs from "fs/promises";

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
  let fileToDownload = `darklang-executor-${latestExecutorHash}-linux-x64`;

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

// returns the pid?
export async function startExecutorHttpServer(
  executorUri: vscode.Uri,
  port: string,
): Promise<void> {
  // const pwd = exec("pwd");
  // pwd.stdout?.on("data", data => {
  //   vscode.window.showInformationMessage(`pwd: ${data}`);
  // });

  console.log(`executorLocation: ${executorUri}`);
  console.log(`port: ${port}`);
  executorSubprocess = childProcess.spawn(executorUri.fsPath, [
    "serve",
    `--port=${port}`,
  ]);

  executorSubprocess.stdout?.on("data", data => {
    vscode.window.showInformationMessage(`stdout: ${data}`);
  });

  executorSubprocess.stderr?.on("data", data => {
    vscode.window.showInformationMessage(`stderr: ${data}`);
  });

  executorSubprocess.on("error", error => {
    vscode.window.showInformationMessage(`error: ${error.message}`);
  });

  executorSubprocess.on("close", code => {
    vscode.window.showInformationMessage(
      `child process exited with code ${code}`,
    );
  });
}

export async function evalSomeCodeAgainstHttpServer(
  port: string,
  code: string,
): Promise<string> {
  const apiResponse = await fetch(
    `http://localhost:${port}/api/v0/execute-text`,
    { method: "POST", body: code },
  );

  return await apiResponse.text();
}

export async function stopExecutor(): Promise<void> {
  executorSubprocess.kill();
}
