import * as vscode from "vscode";
import { PackageNode, BranchNode, StatusBarData } from "../types";

export enum DemoBranchId {
  Default = "default",
  FeatureFilterMap = "feature-filtermap",
  RefactorDbLayer = "refactor-db-layer",
}

export interface BranchState {
  id: DemoBranchId;
  name: string;
  description: string;
  statusBar: StatusBarData;
  packages: PackageNode[];
  branches: BranchNode[];
}

export class BranchStateManager {
  private static _instance: BranchStateManager;
  private _currentBranch: DemoBranchId = DemoBranchId.Default;
  private _currentBranchName: string = "default";
  private _onBranchChanged = new vscode.EventEmitter<DemoBranchId>();
  readonly onBranchChanged = this._onBranchChanged.event;

  static getInstance(): BranchStateManager {
    if (!BranchStateManager._instance) {
      BranchStateManager._instance = new BranchStateManager();
    }
    return BranchStateManager._instance;
  }

  get currentBranch(): DemoBranchId {
    return this._currentBranch;
  }

  switchBranch(branchId: DemoBranchId): void {
    console.log(`🌿 switchBranch called with: ${branchId}, current: ${this._currentBranch}`);
    if (this._currentBranch !== branchId) {
      this._currentBranch = branchId;
      // Reset branch name to default when switching branches
      const branchState = this.branches[branchId];
      this._currentBranchName = branchState.statusBar.branch.name;
      console.log(`🌿 Branch changed to: ${branchId}, firing event`);
      this._onBranchChanged.fire(branchId);
    } else {
      console.log(`🌿 Branch unchanged, not firing event`);
    }
  }

  setBranchName(name: string): void {
    this._currentBranchName = name;
    console.log(`📝 Branch name updated to: ${name}`);
    this._onBranchChanged.fire(this._currentBranch);
  }

  getBranchName(): string {
    return this._currentBranchName;
  }

  getCurrentBranchState(): BranchState {
    const branchState = this.branches[this._currentBranch];
    // Override branch name with custom name if set
    return {
      ...branchState,
      statusBar: {
        ...branchState.statusBar,
        branch: {
          ...branchState.statusBar.branch,
          name: this._currentBranchName
        }
      }
    };
  }

  getAllBranches(): BranchState[] {
    return Object.values(this.branches);
  }

  private branches: Record<DemoBranchId, BranchState> = {
    [DemoBranchId.Default]: {
      id: DemoBranchId.Default,
      name: "default",
      description: "Default branch - clean workspace",
      statusBar: {
        branch: { name: "default", active: true },
        patch: { current: "None", changes: 0 },
        conflicts: { count: 0, hasUnresolved: false },
        sync: { incoming: 0, outgoing: 0 }
      },
      packages: this.getPackages(),
      branches: this.getCleanBranches(),
    },

    [DemoBranchId.FeatureFilterMap]: {
      id: DemoBranchId.FeatureFilterMap,
      name: "feature-filtermap",
      description: "Feature branch: Add List.filterMap function",
      statusBar: {
        branch: { name: "feature-filtermap", active: true },
        patch: { current: "add-filtermap", changes: 3 },
        conflicts: { count: 0, hasUnresolved: false },
        sync: { incoming: 1, outgoing: 0 }
      },
      packages: this.getPackages(),
      branches: this.getActiveDevBranches(),
    },

    [DemoBranchId.RefactorDbLayer]: {
      id: DemoBranchId.RefactorDbLayer,
      name: "refactor-db-layer",
      description: "Refactor branch: Database connection pooling",
      statusBar: {
        branch: { name: "refactor-db-layer", active: true },
        patch: { current: "db-pooling", changes: 5 },
        conflicts: { count: 0, hasUnresolved: false },
        sync: { incoming: 2, outgoing: 1 }
      },
      packages: this.getPackages(),
      branches: this.getReadyForReviewBranches(),
    },
  };

  // Branch-specific data methods
  /**
   * Base Darklang packages available across all branches
   */
  private getPackages(): PackageNode[] {
    return [
      {
        id: "Darklang",
        label: "Darklang",
        type: "owner",
        collapsibleState: 1,
        contextValue: "owner",
        children: [
          {
            id: "Darklang.Stdlib",
            label: "Stdlib",
            type: "module",
            collapsibleState: 1,
            contextValue: "module",
            children: [
              {
                id: "Darklang.Stdlib.List",
                label: "List",
                type: "module",
                collapsibleState: 1,
                contextValue: "module",
                children: [
                  {
                    id: "Darklang.Stdlib.List.head",
                    label: "head",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.List.head",
                    packagePath: "Darklang.Stdlib.List.head"
                  },
                  {
                    id: "Darklang.Stdlib.List.map",
                    label: "map",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.List.map",
                    packagePath: "Darklang.Stdlib.List.map"
                  },
                  {
                    id: "Darklang.Stdlib.List.filter",
                    label: "filter",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.List.filter",
                    packagePath: "Darklang.Stdlib.List.filter"
                  },
                  {
                    id: "Darklang.Stdlib.List.reverse",
                    label: "reverse",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.List.reverse",
                    packagePath: "Darklang.Stdlib.List.reverse"
                  }
                ]
              },
              {
                id: "Darklang.Stdlib.String",
                label: "String",
                type: "module",
                collapsibleState: 1,
                contextValue: "module",
                children: [
                  {
                    id: "Darklang.Stdlib.String.isEmpty",
                    label: "isEmpty",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.String.isEmpty",
                    packagePath: "Darklang.Stdlib.String.isEmpty"
                  },
                  {
                    id: "Darklang.Stdlib.String.length",
                    label: "length",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.Stdlib.String.length",
                    packagePath: "Darklang.Stdlib.String.length"
                  }
                ]
              }
            ]
          },
          {
            id: "Darklang.LanguageTools",
            label: "LanguageTools",
            type: "module",
            collapsibleState: 1,
            contextValue: "module",
            children: [
              {
                id: "Darklang.LanguageTools.Parser",
                label: "Parser",
                type: "module",
                collapsibleState: 1,
                contextValue: "module",
                children: [
                  {
                    id: "Darklang.LanguageTools.Parser.parseExpression",
                    label: "parseExpression",
                    type: "function",
                    collapsibleState: 0,
                    contextValue: "fn:Darklang.LanguageTools.Parser.parseExpression",
                    packagePath: "Darklang.LanguageTools.Parser.parseExpression"
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        id: "Stachu",
        label: "Stachu",
        type: "owner",
        collapsibleState: 1,
        contextValue: "owner",
        children: [
          {
            id: "Stachu.Json",
            label: "Json",
            type: "module",
            collapsibleState: 1,
            contextValue: "module",
            children: [
              {
                id: "Stachu.Json.serialize",
                label: "serialize",
                type: "function",
                collapsibleState: 0,
                contextValue: "fn:Stachu.Json.serialize",
                packagePath: "Stachu.Json.serialize"
              },
              {
                id: "Stachu.Json.deserialize",
                label: "deserialize",
                type: "function",
                collapsibleState: 0,
                contextValue: "fn:Stachu.Json.deserialize",
                packagePath: "Stachu.Json.deserialize"
              }
            ]
          },
          {
            id: "Stachu.Timespan",
            label: "Timespan",
            type: "module",
            collapsibleState: 1,
            contextValue: "module",
            children: [
              {
                id: "Stachu.Timespan.fromSeconds",
                label: "fromSeconds",
                type: "function",
                collapsibleState: 0,
                contextValue: "fn:Stachu.Timespan.fromSeconds",
                packagePath: "Stachu.Timespan.fromSeconds"
              }
            ]
          }
        ]
      }
    ];
  }


  // Placeholder implementations for branches
  private getCleanBranches(): BranchNode[] { return []; }
  private getActiveDevBranches(): BranchNode[] { return []; }
  private getReadyForReviewBranches(): BranchNode[] { return []; }
}
