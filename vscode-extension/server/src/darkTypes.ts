// types corresponding to package types in Darklang

export type Position = { line: number; character: number };

export type Range = {
  start: Position;
  end_: Position;
};

export type DiagnosticSeverity = "Warning" | "Error";

export type Diagnostic = {
  severity: DiagnosticSeverity;
  range: Range;
  message: string;
};

export type ComputeDiagnosticsOutput = {
  diagnostics: Diagnostic[];
};
