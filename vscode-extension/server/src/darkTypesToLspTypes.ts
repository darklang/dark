import * as VSC from "vscode-languageserver";

import * as DT from "./darkTypes";

export function position(p: DT.Position): VSC.Position {
  return VSC.Position.create(p.line, p.character);
}

export function range(r: DT.Range): VSC.Range {
  return VSC.Range.create(position(r.start), position(r.end_));
}

export function diagnosticSeverify(
  s: DT.DiagnosticSeverity,
): VSC.DiagnosticSeverity {
  switch (s) {
    case "Warning":
      return VSC.DiagnosticSeverity.Warning;

    case "Error":
      return VSC.DiagnosticSeverity.Error;

    default:
      throw new Error(`unknown diagnostic severity: ${s}`);
  }
}

export function diagnostic(dt: DT.Diagnostic): VSC.Diagnostic {
  return {
    severity: diagnosticSeverify(dt.severity),
    range: range(dt.range),
    message: dt.message,
  };
}
