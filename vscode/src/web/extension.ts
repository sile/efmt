// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	vscode.languages.registerDocumentFormattingEditProvider('erlang', {
		provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.ProviderResult<vscode.TextEdit[]> {
			const firstLine = document.lineAt(0);
			if (firstLine.text !== '42') {
				return [vscode.TextEdit.insert(firstLine.range.start, '42\n')];
			}
		}
	});
}

// This method is called when your extension is deactivated
export function deactivate() {}
