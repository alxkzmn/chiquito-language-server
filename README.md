# Chiquito language server powered by `tower-lsp`

## Introduction

This Chiquito language server is based on the [boilerplate tower-lsp](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate).

## Development using VSCode

1. `npm i`
2. `cargo build`
3. Open the project in VSCode: `code .`
4. In VSCode, press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>
   > **Note**
   >
   > If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
   > please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details

## Features

- [x] semantic token  
       make sure your semantic token is enabled, you could enable your `semantic token` by
      adding this line to your `settings.json`

```json
{
  "editor.semanticHighlighting.enabled": true
}
```
