import * as vscode from "vscode";

import * as Executor from "./Executor";

// Types to communicate with the editor
type Commands = { name: "InitialLoad"; args: string[] };

// type Intents = {
//   | { name: "SetHandlers", handlers: Handler[], functions: Function[], types: Type[] }

async function readDarkFiles(): Promise<string[]> {
  const darkFiles = await vscode.workspace.findFiles("**/*.dark");
  const darkFileContents = await Promise.all(
    darkFiles.map(async file => {
      const textDocument = await vscode.workspace.openTextDocument(file);
      return textDocument.getText();
    }),
  );
  return darkFileContents;
}

export async function evalCommand(
  command: string,
  args: string,
): Promise<string> {
  let program = (await readDarkFiles()).join("\n");
  let code = `
    let command = "${command}"
    let args = ${args}
    main command args
  `;
  let full = program + code;
  console.log(full);
  return await Executor.evalCode(full);
}

export async function initialLoad() {
  return await evalCommand("InitialLoad", "[]");
}
