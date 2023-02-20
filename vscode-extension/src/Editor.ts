import * as vscode from "vscode";

import * as Executor from "./Executor";

import * as path from "path";
import * as fs from "fs/promises";
import * as glob from "glob-promise";

// Types to communicate with the editor
type Commands = { name: "InitialLoad"; args: string[] };

// type Intents = {
//   | { name: "SetHandlers", handlers: Handler[], functions: Function[], types: Type[] }

async function readDarkFiles(): Promise<string[]> {
  const files = await glob(__dirname + "/*.dark");
  const contents = [];
  for (const file of files) {
    const content = await fs.readFile(file, "utf-8");
    contents.push(content);
  }
  return contents;
}

export async function evalCommand(
  command: string,
  args: string,
): Promise<string> {
  let program = (await readDarkFiles()).join("\n");
  let code = `let command = "${command}" in \nlet args = ${args} in\n`;
  // Parens around it so that it's a single expression
  let full = `(${code}${program})`;
  let response = await Executor.evalCode(full);
  return response;
}

export async function initialLoad() {
  return await evalCommand("InitialLoad", "[]");
}
