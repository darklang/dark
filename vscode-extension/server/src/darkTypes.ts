// types corresponding to package types in Darklang

export type Position = { line: number; character: number };

export type Range = {
  start: Position;
  end: Position;
};

export type DiagnosticSeverity = "Warning" | "Error";

export type Diagnostic = {
  severity: DiagnosticSeverity;
  range: Range;
  message: string;
};

export type ComputeDiagnosticsInput = {
  uri: string;
  text: string;
  maxNumberOfProblems: number;
};

export type ComputeDiagnosticsOutput = {
  diagnostics: Diagnostic[];
};
