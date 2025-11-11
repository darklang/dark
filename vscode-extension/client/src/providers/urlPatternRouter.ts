/** URL Pattern Router */

// CLEANUP this should all be in Darklang
// and should be modeled as a Page, maybe? or Document
export interface ParsedUrl {
  scheme: string;
  mode: UrlMode;
  context?: string;
  target?: string;
  view?: string;
  queryParams?: Record<string, string>;
}

export type UrlMode =
  | 'instance'
  | 'branch'
  | 'package';

export class UrlPatternRouter {
  static parseUrl(url: string): ParsedUrl | null {
    try {
      const urlObj = new URL(url);

      if (urlObj.protocol !== 'dark:') {
        return null;
      }

      const pathParts = urlObj.pathname.split('/').filter(p => p);
      const queryParams = this.parseQueryParams(urlObj.search);

      if (pathParts.length === 0) {
        console.log('UrlPatternRouter: No path parts found');
        // CLEANUP what _should_ we do here? `dark://` -- maybe some 'home' page?
        return null;
      }

      const mode = pathParts[0] as UrlMode;

      switch (mode) {
        case 'package':
          return this.parsePackageUrl(pathParts, queryParams);

        case 'branch':
          return this.parseBranchUrl(pathParts, queryParams);

        default:
          return null;
      }
    } catch (error) {
      return null;
    }
  }

  /**
   * Parse package URLs: dark://package/Name.Space.item[?view=type]
   */
  private static parsePackageUrl(pathParts: string[], queryParams: Record<string, string>): ParsedUrl {
    // pathParts: ['package', 'module', 'Darklang.Stdlib.Bool']
    // or: ['package', 'Darklang.Stdlib.Bool']

    const secondPart = pathParts[1];
    const knownViews = ['module', 'source'];

    let target: string;
    let view: string;

    if (knownViews.includes(secondPart)) {
      // Second part is a view specifier: ['package', 'module', 'Darklang.Stdlib.Bool']
      view = secondPart;
      target = pathParts.slice(2).join('.');
    } else {
      // No view specified: ['package', 'Darklang.Stdlib.Bool']
      view = queryParams.view || 'source';
      target = pathParts.slice(1).join('.');
    }

    return {
      scheme: 'dark',
      mode: 'package',
      target: target,
      view: view,
      queryParams
    };
  }


  /**
   * Parse branch URLs: dark://branch/branch-id[/action]
   */
  private static parseBranchUrl(pathParts: string[], queryParams: Record<string, string>): ParsedUrl {
    return {
      scheme: 'dark',
      mode: 'branch',
      context: pathParts[1], // branch-id
      view: pathParts[2] || queryParams.view || 'overview',
      queryParams
    };
  }

  private static parseQueryParams(search: string): Record<string, string> {
    const params: Record<string, string> = {};
    if (search) {
      const urlParams = new URLSearchParams(search);
      for (const [key, value] of urlParams) {
        params[key] = value;
      }
    }
    return params;
  }
}