// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {
	const formatter = await Formatter.load(context);

	vscode.languages.registerDocumentFormattingEditProvider('erlang', {
		provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.ProviderResult<vscode.TextEdit[]> {
			formatter.setText(document.getText());
			const errorText = formatter.check(document.getText());
			if (errorText.length === 0) {
				const formattedText = formatter.format(document.getText());
				formatter.freeText();
				return [vscode.TextEdit.replace(new vscode.Range(0, 0, document.lineCount, 0), formattedText)];
			} else {
				formatter.freeText();
				vscode.window.showErrorMessage(errorText);
			}
		}
	});
}

// This method is called when your extension is deactivated
export function deactivate() {}


class Formatter {
	private wasmInstance: WebAssembly.Instance;
	private wasmMemory: WebAssembly.Memory;
	private buffer: number = 0 ;
	private bufferOffset: number = 0;
	private bufferLength: number = 0;

	static async load(context: vscode.ExtensionContext): Promise<Formatter> {
		let wasmUri = vscode.Uri.joinPath(context.extensionUri, "./dist/efmt.wasm");
		const wasmInstance = (await WebAssembly.instantiateStreaming(fetch(wasmUri.toString()))).instance;
		const wasmMemory = wasmInstance.exports.memory as WebAssembly.Memory;
		return new Formatter(wasmInstance, wasmMemory);
	}

	private constructor(wasmInstance: WebAssembly.Instance, wasmMemory: WebAssembly.Memory) {
		this.wasmInstance = wasmInstance;
		this.wasmMemory = wasmMemory;
	}

	setText(text: string): void {
		const originalTextBytes = new TextEncoder().encode(text);
		this.buffer = (this.wasmInstance.exports.allocate_vec as CallableFunction)(originalTextBytes.length);
		this.bufferOffset = (this.wasmInstance.exports.vec_offset as CallableFunction)(this.buffer);
		this.bufferLength = originalTextBytes.length;
		new Uint8Array(this.wasmMemory.buffer, this.bufferOffset, originalTextBytes.length).set(originalTextBytes);
	}

	freeText(): void {
		(this.wasmInstance.exports.free_vec as CallableFunction)(this.buffer);
	}

	check(text: string): string {
		const error = (this.wasmInstance.exports.check as CallableFunction)(this.bufferOffset, this.bufferLength);		
 		const errorOffset = (this.wasmInstance.exports.vec_offset as CallableFunction)(error);
		const errorLength = (this.wasmInstance.exports.vec_len as CallableFunction)(error);
		const errorText = new TextDecoder('utf-8').decode(new Uint8Array(this.wasmMemory.buffer, errorOffset, errorLength));
		(this.wasmInstance.exports.free_vec as CallableFunction)(error);
		return errorText;
	}
 
	format(text: string): string {
		const formatted = (this.wasmInstance.exports.format as CallableFunction)(this.bufferOffset, this.bufferLength);
		const formattedOffset = (this.wasmInstance.exports.vec_offset as CallableFunction)(formatted);
		const formattedLength = (this.wasmInstance.exports.vec_len as CallableFunction)(formatted);
		const formattedText = new TextDecoder('utf-8').decode(new Uint8Array(this.wasmMemory.buffer, formattedOffset, formattedLength));
		(this.wasmInstance.exports.free_vec as CallableFunction)(formatted);
		return formattedText;
	}
}
