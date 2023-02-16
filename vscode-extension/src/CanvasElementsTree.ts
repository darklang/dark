import * as vscode from "vscode";

type CanvasItem = {
  // i.e. "http handler"
  title: string;

  children: CanvasItem[];
};

let theList: CanvasItem[] = [
  {
    title: "HTTP Handlers",
    children: [
      { title: "GET /", children: [] },
      { title: "GET /cli?", children: [] },
      { title: "GET /about", children: [] },
      { title: "GET /help", children: [] },
      { title: "GET /extension", children: [] },
      { title: "GET /initial-load", children: [] },
      { title: "GET /add-op", children: [] }, // todo: update add-op to make updates to .dark files
    ],
  },
  { title: "Workers", children: [] },
  { title: "CRONs", children: [] },
  { title: "REPLs", children: [] },
  { title: "DBs (Key-value)", children: [{ title: "Users", children: [] }] },
  { title: "Functions", children: [] },
  { title: "404s", children: [] },
  { title: "Deleted", children: [] },
  { title: "Packages", children: [] },
  { title: "Secrets", children: [] },
];

export class CanvasElementsTreeProvider
  implements vscode.TreeDataProvider<CanvasItem>
{
  constructor() {}

  getTreeItem(element: CanvasItem): vscode.TreeItem {
    if (element === undefined) {
      return new vscode.TreeItem(
        "root",
        vscode.TreeItemCollapsibleState.Expanded,
      );
    } else if (element.children.length > 0) {
      return new vscode.TreeItem(
        `${element.title} (${element.children.length})`,
        vscode.TreeItemCollapsibleState.Collapsed,
      );
    } else {
      return new vscode.TreeItem(
        element.title,
        vscode.TreeItemCollapsibleState.Collapsed,
      );
    }
  }

  getChildren(element: CanvasItem): Thenable<CanvasItem[]> {
    if (element === undefined || element.title === undefined) {
      return Promise.resolve(theList);
    }
    const found = theList.find(el => el.title === element.title);

    if (found === undefined) {
      return Promise.resolve([]);
    }

    return Promise.resolve(found.children);
  }
}
