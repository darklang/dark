import { ParsedUrl } from "../urlPatternRouter";
import { LanguageClient } from "vscode-languageclient/node";

/**
 * Content provider for package browsing URLs
 * Handles: dark://package/Name.Space.item[?view=type]
 */
export class PackageContentProvider {
  private static client: LanguageClient | null = null;

  static setClient(client: LanguageClient): void {
    this.client = client;
  }

  static async getContentAsync(parsedUrl: ParsedUrl): Promise<string> {
    const { target, view } = parsedUrl;

    console.log(`PackageContentProvider.getContentAsync: target="${target}", view="${view}"`);

    if (!target) {
      return this.getPackageListContent();
    }

    switch (view) {
      case 'ast':
        return this.getAstView(target);

      case 'module':
        return await this.getSourceView(target, true);

      default:
        return await this.getSourceView(target, false);
    }
  }

  static getContent(parsedUrl: ParsedUrl): string {
    const { target } = parsedUrl;
    if (!target) {
      return this.getPackageListContent();
    }
    return this.getLoadingContent(target);
  }

  private static async getSourceView(target: string, isModule: boolean = false): Promise<string> {
    if (!this.client) {
      return this.getGenericPackageContent(target);
    }

    try {
      const packagePath = target;
      const uri = `darkfs:/${packagePath}.dark`;

      const response = await this.client.sendRequest<{ content: string }>(
        'fileSystem/read',
        { uri }
      );

      return response.content;
    } catch (error) {
      console.error(`Failed to fetch package content for ${target}:`, error);
      return this.getGenericPackageContent(target);
    }
  }

  private static getLoadingContent(target: string): string {
    return `# ${target}

Loading package definition...`;
  }

  private static getAstView(target: string): string {
    return `# AST View: ${target}

## Abstract Syntax Tree

\`\`\`
FunctionDef {
  name: "${target.split('.').pop()}"
  parameters: [
    Parameter { name: "fn", type: "'a -> 'b" },
    Parameter { name: "list", type: "List<'a>" }
  ]
  returnType: "List<'b>"
  body: MatchExpression {
    expr: Variable("list")
    cases: [
      Case {
        pattern: EmptyList
        body: EmptyList
      },
      Case {
        pattern: Cons(Variable("head"), Variable("tail"))
        body: Cons(
          FunctionCall(Variable("fn"), [Variable("head")]),
          FunctionCall(Variable("map"), [Variable("fn"), Variable("tail")])
        )
      }
    ]
  }
}
\`\`\`

## Type Inference

- **Input Types**: \`'a -> 'b\`, \`List<'a>\`
- **Output Type**: \`List<'b>\`
- **Type Variables**: \`'a\`, \`'b\` (polymorphic)
- **Constraints**: None

## Compilation Target

\`\`\`javascript
function map(fn, list) {
  if (list.length === 0) return [];
  return [fn(list[0])].concat(map(fn, list.slice(1)));
}
\`\`\``;
  }


  private static getGenericPackageContent(target: string): string {
    return `# ${target}
(imagine we have package content here)`;
  }


  private static getPackageListContent(): string {
    return `# Darklang Packages
    TODO: some pretty page`;
  }
}