import * as vscode from "vscode";

type AccountListener = (accountId: string) => void;

/** Centralized account management - placeholder until proper auth */
class AccountServiceImpl {
  // TODO: Replace with proper auth when available
  private _currentAccountId: string = "Darklang";
  private _listeners: Set<AccountListener> = new Set();

  getCurrentAccountId(): string {
    return this._currentAccountId;
  }

  setCurrentAccount(accountId: string): void {
    if (this._currentAccountId !== accountId) {
      this._currentAccountId = accountId;
      this._listeners.forEach(listener => listener(accountId));
    }
  }

  onDidChange(listener: AccountListener): vscode.Disposable {
    this._listeners.add(listener);
    return { dispose: () => this._listeners.delete(listener) };
  }
}

export const AccountService = new AccountServiceImpl();
