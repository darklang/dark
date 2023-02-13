// bootstrapping the interpreter
import fetch from "node-fetch";
import {
  ChildProcess,
  spawn,
  exec,
  ChildProcessWithoutNullStreams,
} from "node:child_process";
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
  latestExecutorHash: string,
): Promise<string> {
  // TODO: this URL should be a function of the host OS
  let fileToDownload = `darklang-executor-${latestExecutorHash}-linux-x64`;

  // Fetch, save, and start the interpreter
  const url = `https://downloads.darklang.com/${fileToDownload}`;

  const apiResponse = await fetch(url);

  let destPath = `./${fileToDownload}`;

  const blob = await apiResponse.blob();
  const bos = blob.stream();

  await fs.writeFile(destPath, bos);
  await fs.chmod(destPath, "755"); // executable

  return destPath;
}

export async function downloadLatestExecutor(): Promise<string> {
  const hash = await latestExecutorHash();
  return downloadExecutor(hash);
}

let executorSubprocess: ChildProcessWithoutNullStreams;

// returns the pid?
export async function startExecutorHttpServer(
  executorLocation: string,
  port: string,
): Promise<void> {
  // const pwd = exec("pwd");
  // pwd.stdout?.on("data", data => {
  //   vscode.window.showInformationMessage(`pwd: ${data}`);
  // });

  executorSubprocess = spawn(`${executorLocation} serve --port ${port}`);

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
