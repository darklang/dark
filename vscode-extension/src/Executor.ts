// bootstrapping the interpreter
import axios from "axios";
import { ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import * as fs from "fs";

export async function latestExecutorHash(): Promise<string> {
  const apiResponse = await axios.get(
    `https://editor.darklang.com/latest-executor`,
  );
  const hash = apiResponse.data.hash;
  return hash;
}

/** returns: path (on disk) to downloaded executor */
export async function downloadExecutor(
  latestExecutorHash: string,
): Promise<string> {
  // TODO: this URL should be a function of the host OS
  let fileToDownload = `darklang-executor-${latestExecutorHash}-linux-x64`;

  // Fetch, save, and start the interpreter
  const url = `https://downloads.darklang.com/${fileToDownload}`;
  console.log("url", url);

  const fetchedInterpreterScript = await axios.get(url);

  let destPath = `./${fileToDownload}`;

  fs.writeFileSync(destPath, fetchedInterpreterScript.data);
  fs.chmodSync(destPath, "755"); // executable

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
  executorSubprocess = spawn(`${executorLocation} serve --port ${port}`);

  // todo: error handling, etc.
}

export async function stopExecutor(): Promise<void> {
  executorSubprocess.kill();
}

export async function evalSomeCodeAgainstHttpServer(
  port: string,
  code: string,
): Promise<string> {
  const response = await axios.post(
    `https://localhost:${port}/api/v0/execute-text`,
    code,
  );

  return response.data;
}
