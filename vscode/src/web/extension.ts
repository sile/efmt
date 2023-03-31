// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {
	let wasmUri = vscode.Uri.joinPath(context.extensionUri, "./dist/efmt.wasm");
	console.log(`wasmUri: ${wasmUri}`); // TODO: remove
	// TODO:
	// let fixedUri = 'importScripts' in globalThis ? wasmUri.toString() : wasmUri.fsPath;
	// console.log(`fixedUri: ${fixedUri}`);
    //const wasmBinary = await ;
	const wasmInstance = (await WebAssembly.instantiateStreaming(fetch(wasmUri.toString()))).instance;
	const wasmMemory = wasmInstance.exports.memory as WebAssembly.Memory;
	console.log(`wasmInstance: ${wasmInstance}`); // TODO: remove

	vscode.languages.registerDocumentFormattingEditProvider('erlang', {
		provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.ProviderResult<vscode.TextEdit[]> {
			const originalText = document.getText();
			const originalTextBytes = new TextEncoder().encode(originalText);

			const buffer = (wasmInstance.exports.allocate_vec as CallableFunction)(originalTextBytes.length);
			const bufferOffset = (wasmInstance.exports.vec_offset as CallableFunction)(buffer);
			new Uint8Array(wasmMemory.buffer, bufferOffset, originalTextBytes.length).set(originalTextBytes);

			const formatted = (wasmInstance.exports.format as CallableFunction)(bufferOffset, originalTextBytes.length);
            (wasmInstance.exports.free_vec as CallableFunction)(buffer);

            const formattedOffset = (wasmInstance.exports.vec_offset as CallableFunction)(formatted);
            const formattedLen = (wasmInstance.exports.vec_len as CallableFunction)(formatted);
            const formattedText = new TextDecoder('utf-8').decode(new Uint8Array(wasmMemory.buffer, formattedOffset, formattedLen));
            (wasmInstance.exports.free_vec as CallableFunction)(formatted);

			return [vscode.TextEdit.replace(new vscode.Range(0, 0, document.lineCount, 0), formattedText)];

/* 			const error = wasmInstance.exports.check(bufferOffset, originalTextBytes.length);
			const errorOffset = wasmInstance.exports.vec_offset(error);
			const errorLen = wasmInstance.exports.vec_len(error);
			const errorText = new TextDecoder('utf-8').decode(
				new Uint8Array(wasmMemory.buffer, errorOffset, errorLen));
			wasmInstance.exports.free_vec(error);

			if (errorText.length == 0) {
               const formatted = wasmInstance.exports.format(bufferOffset, originalTextBytes.length);
               wasmInstance.exports.free_vec(buffer);

               const formattedOffset = wasmInstance.exports.vec_offset(formatted);
               const formattedLen = wasmInstance.exports.vec_len(formatted);
               const formattedText = new TextDecoder('utf-8').decode(
                   new Uint8Array(wasmMemory.buffer, formattedOffset, formattedLen));
               wasmInstance.exports.free_vec(formatted);
               document.getElementById("after").value = formattedText;
           } else{
               document.getElementById("after").value = errorText;
           }
 */ 
		}
	});
}

// This method is called when your extension is deactivated
export function deactivate() {}
