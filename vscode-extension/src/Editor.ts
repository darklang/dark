// Types to communicate with the editor
import * as Executor from "./Executor";

type Commands =
  | { name : "InitialLoad", args : string[] }

type Intents = {
  | { name: "SetHandlers", handlers: Handler[], functions: Function[], types: Type[] }

export async function evalCommand(
  port: number,
  command: string,
  args: string[],
): Promise<string> {
  await Executor.evalDarklang(port, "main(command, args)", { "command": command, "args": args });
}