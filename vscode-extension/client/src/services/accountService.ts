import * as vscode from "vscode";

type AccountListener = (accountID: string) => void;

/** Centralized account management - placeholder until proper auth */
class AccountServiceImpl {
  // TODO: Replace with proper auth when available
  private _currentAccountId: string = "Darklang";
  private _listeners: Set<AccountListener> = new Set();

  getCurrentAccountId(): string {
    return this._currentAccountId;
  }

  setCurrentAccount(accountID: string): void {
    if (this._currentAccountId !== accountID) {
      this._currentAccountId = accountID;
      this._listeners.forEach(listener => listener(accountID));
    }
  }

  onDidChange(listener: AccountListener): vscode.Disposable {
    this._listeners.add(listener);
    return { dispose: () => this._listeners.delete(listener) };
  }
}

export const AccountService = new AccountServiceImpl();
