# Ranvier.AutoComplete (`ranvierautocomplete`)

Demand-driven, declaration-incremental F# Language Server Protocol (LSP) server built on [Ranvier](https://github.com/ranvier-org/Ranvier) and [`fickle-fsharp`](https://github.com/shayanhabibi/fickle-fsharp) (`FSharp.Compiler.Service`).

`ranvierautocomplete` is a drop-in replacement for `fsautocomplete` (FSAC) in **Ionide for VS Code**, **Neovim**, **Helix**, **Zed**, **Emacs**, and any stdio LSP client.

## Why Ranvier.AutoComplete?

Traditional F# language servers re-typecheck entire files (and downstream files) on edits or retain heavy compiler `TcState` / `FSharpCheckFileResults` graphs across open buffers. `ranvierautocomplete` models F# parsing, dependency extraction, declaration-level type-checking, symbol uses, and cross-project signatures as a reactive [Ranvier](https://github.com/ranvier-org/Ranvier) computation graph:

- **Declaration-level incremental checking**: Edits inside a function or binding body re-check only the affected declaration (`~1.7–12.6` declarations/edit instead of the whole file) and stop propagation immediately when the inferred signature is unchanged.
- **Zero compiler-object retention at rest**: Diagnostics, hovers, semantic classifications, symbol uses, and signature projections are extracted eagerly into compact immutable records; `FSharpCheckFileResults` are never retained on the heap.
- **Live in-memory cross-project propagation**: Unsaved buffer edits propagate across `ProjectReference` boundaries in memory without rebuilding DLLs on disk.
- **Full LSP & Ionide protocol parity**: Semantic highlighting (`full`, `full/delta`, `range`), hover, completion (+ auto-open namespace edits), go-to definition / declaration / type definition / implementation, find references, rename (`prepareRename`), signature help, inlay hints, code lenses, inline/pipeline hints, document & workspace symbols, call & type hierarchy, folding & selection ranges, 12 quick-fix/refactoring families, Fantomas formatting, `.slnx` / `.sln` workspace loading, MSBuild glob items (`Compile` / `None` / `Content`), F# analyzers, and Ionide `fsharp/*` custom endpoints.

### Benchmark Highlights (`ranvierautocomplete` vs FSAC)

Measured on identical multi-project corpora (`results/server-final/`):

| Scenario | Metric | `fsac-core-bg` | `fsac-core-tc` | `ranvierautocomplete` |
| --- | --- | ---: | ---: | ---: |
| `mid-body` | Edit p50 / p95 (ms) | 87.5 / 291.1 | 74.9 / 261.0 | **41.3 / 104.5** |
| `mid-body` | Peak Working Set (MB) | 762.7 | 811.8 | **422.8** |
| `mid-crossproject` | Edit p50 / p95 (ms) | 74.4 / 312.8 | 41.6 / 308.2 | **32.2 / 74.4** |
| `mid-references` | `textDocument/references` p50 (ms) | 65.3 | 103.5 | **7.6** |
| `small-references` | `textDocument/references` p50 (ms) | 64.6 | 28.2 | **5.5** |
| `mid-openmany` | Peak Working Set (MB) | 3,120.4 | 706.2 | **441.4** |
| `large-cold` | Peak Working Set (MB) | 1,036.2 | 1,035.3 | **636.9** |

---

## Prerequisites

1. **`.NET 10.0` SDK (`10.0.x`)**:
   `ranvierautocomplete` runs on `.NET 10` and loads MSBuild in-process from an installed `.NET 10` SDK via `MSBuildLocator` (automatically pinning a `.NET 10` SDK in `%TEMP%/rfs-sdk` even if you also have `.NET 8`, `.NET 9`, or `.NET 11` preview SDKs installed). Verify at least one `10.0.*` SDK is installed:
   ```bash
   dotnet --list-sdks
   ```
2. **Fantomas (optional, for document/range formatting)**:
   `textDocument/formatting` and `textDocument/rangeFormatting` invoke `fantomas` (either via a workspace `.config/dotnet-tools.json` local tool or a global tool on `PATH`):
   ```bash
   dotnet tool install -g fantomas
   ```

---

## Installation

### Option A — Install as a `.NET` Global Tool (Recommended for Neovim, Helix, Zed, Emacs, CLI)

From NuGet (once published):
```bash
dotnet tool install -g Ranvier.AutoComplete --prerelease
```

Or from a locally packed `.nupkg` (see [Building & Packing from Source](#building--packing-from-source)):
```bash
dotnet tool install -g Ranvier.AutoComplete --add-source ./artifacts/nupkg --prerelease
```

Verify installation:
```bash
ranvierautocomplete --version
```

### Option B — Portable Bundle (Recommended for VS Code Ionide `netCoreDllPath`)

Ionide for VS Code overrides FSAC via `FSharp.fsac.netCoreDllPath`, which expects the full path to `ranvierautocomplete.dll`. Either download `ranvierautocomplete-portable-net10.0.zip` from GitHub Releases and extract it to a permanent folder, or produce `./artifacts/publish/ranvierautocomplete.dll` locally with:

```powershell
pwsh -File build/pack.ps1
```

---

## Editor Setup

### 1. VS Code (Ionide for F#)

1. Install the [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) extension (`Ionide.Ionide-fsharp`).
2. Point `FSharp.fsac.netCoreDllPath` in your User or Workspace `settings.json` to `ranvierautocomplete.dll`:

   - **If using the portable bundle (`build/pack.ps1` or release zip):**
     ```jsonc
     {
       // Windows example:
       "FSharp.fsac.netCoreDllPath": "C:\\tools\\ranvierautocomplete\\ranvierautocomplete.dll",
       // macOS / Linux example:
       // "FSharp.fsac.netCoreDllPath": "/opt/ranvierautocomplete/ranvierautocomplete.dll",

       // Ensure VS Code semantic highlighting is enabled for F#:
       "editor.semanticHighlighting.enabled": true
     }
     ```
   - **If installed via `dotnet tool install -g Ranvier.AutoComplete`:**
     Point `FSharp.fsac.netCoreDllPath` to the DLL inside the `.NET` tool store:
     - Windows: `%USERPROFILE%\.dotnet\tools\.store\ranvier.autocomplete\<version>\ranvier.autocomplete\<version>\tools\net10.0\any\ranvierautocomplete.dll`
     - Linux / macOS: `~/.dotnet/tools/.store/ranvier.autocomplete/<version>/ranvier.autocomplete/<version>/tools/net10.0/any/ranvierautocomplete.dll`

3. Run **`Developer: Reload Window`** (or **`F#: Restart Language Server`**) from the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`).
4. Check the **F#** output channel (`View -> Output -> F#`) — Ionide passes its standard FSAC CLI flags (`-v`, `--adaptive-lsp-server-enabled`, `--project-graph-enabled`, `--use-fcs-transparent-compiler`, `--state-directory`, etc.), all of which `ranvierautocomplete` accepts seamlessly.

#### Recommended Optional Ionide Settings (`settings.json`)

```jsonc
{
  "FSharp.fsac.netCoreDllPath": "C:\\tools\\ranvierautocomplete\\ranvierautocomplete.dll",
  "editor.semanticHighlighting.enabled": true,
  "FSharp.keywordsAutocomplete": true,
  "FSharp.resolveNamespaces": true,
  "FSharp.unusedOpensAnalyzer": true,
  "FSharp.unusedDeclarationsAnalyzer": true,
  "FSharp.simplifyNameAnalyzer": true,
  "FSharp.unnecessaryParenthesesAnalyzer": true,
  "FSharp.inlayHints.typeAnnotations": true,
  "FSharp.inlayHints.parameterNames": true,
  "FSharp.codeLenses.signature.enabled": true,
  "FSharp.codeLenses.references.enabled": true,
  "FSharp.pipelineHints.enabled": true
}
```

---

### 2. Neovim

> **Important for non-Ionide clients:** Generic LSP clients do not send Ionide's custom `fsharp/workspacePeek` + `fsharp/workspaceLoad` handshake. Pass `"AutomaticWorkspaceInit": true` in `init_options` so `ranvierautocomplete` automatically discovers and loads `.slnx`, `.sln`, or `.fsproj` files under your workspace root on startup!

#### Neovim 0.11+ (`vim.lsp.config`)

```lua
vim.lsp.config('fsautocomplete', {
  cmd = { 'ranvierautocomplete' },
  filetypes = { 'fsharp' },
  root_markers = { '*.slnx', '*.sln', '*.fsproj', '.git' },
  init_options = {
    AutomaticWorkspaceInit = true,
  },
  settings = {
    FSharp = {
      keywordsAutocomplete = true,
      resolveNamespaces = true,
      unusedOpensAnalyzer = true,
      unusedDeclarationsAnalyzer = true,
      simplifyNameAnalyzer = true,
      unnecessaryParenthesesAnalyzer = true,
      inlayHints = {
        typeAnnotations = true,
        parameterNames = true,
      },
    },
  },
})
vim.lsp.enable('fsautocomplete')
```

#### Neovim (`nvim-lspconfig`)

```lua
local lspconfig = require('lspconfig')

lspconfig.fsautocomplete.setup({
  cmd = { 'ranvierautocomplete' },
  init_options = {
    AutomaticWorkspaceInit = true,
  },
  settings = {
    FSharp = {
      keywordsAutocomplete = true,
      resolveNamespaces = true,
      unusedOpensAnalyzer = true,
      unusedDeclarationsAnalyzer = true,
    },
  },
})
```

#### Neovim (`Ionide-vim`)

```lua
vim.g['fsharp#fsautocomplete_command'] = { 'ranvierautocomplete' }
```

---

### 3. Helix (`~/.config/helix/languages.toml`)

```toml
[language-server.fsautocomplete]
command = "ranvierautocomplete"
config = { AutomaticWorkspaceInit = true, FSharp = { keywordsAutocomplete = true, resolveNamespaces = true, unusedOpensAnalyzer = true, unusedDeclarationsAnalyzer = true } }

[[language]]
name = "fsharp"
language-servers = ["fsautocomplete"]
auto-format = false
```

---

### 4. Emacs (`eglot` / `lsp-mode`)

#### `eglot`

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(fsharp-mode . ("ranvierautocomplete"
                                :initializationOptions
                                (:AutomaticWorkspaceInit t
                                 :KeywordsAutocomplete t
                                 :ResolveNamespaces t
                                 :UnusedOpensAnalyzer t
                                 :UnusedDeclarationsAnalyzer t)))))
```

#### `lsp-mode`

```elisp
(with-eval-after-load 'lsp-fsharp
  (setq lsp-fsharp-server-install-dir nil)
  (setq lsp-clients-fsharp-server-command '("ranvierautocomplete")))
```

---

### 5. Zed / Sublime LSP / Generic LSP Clients

Launch `ranvierautocomplete` over `stdio` (no flags required, or `--logfile <path>` for file logging) with initialization options:

```json
{
  "AutomaticWorkspaceInit": true,
  "KeywordsAutocomplete": true,
  "ResolveNamespaces": true,
  "UnusedOpensAnalyzer": true,
  "UnusedDeclarationsAnalyzer": true,
  "SimplifyNameAnalyzer": true,
  "UnnecessaryParenthesesAnalyzer": true
}
```

---

## Configuration Reference (`FSharp.*`)

All settings can be passed either in LSP `initializeParams.initializationOptions` or dynamically via `workspace/didChangeConfiguration` / `workspace/configuration` (top-level or nested under `"FSharp"`):

| Setting | Type | Default | Description |
| --- | --- | --- | --- |
| `AutomaticWorkspaceInit` | `bool` | `false` | Automatically discover and load `.slnx` / `.sln` / `.fsproj` at `rootUri` on `initialized` (set `true` for non-Ionide editors) |
| `WorkspaceModePeekDeepLevel` | `int` | `2` | Directory recursion depth when scanning for solutions/projects |
| `ExcludeProjectDirectories` | `string[]` | `[".git", "node_modules", ".ionide"]` | Directory names skipped during workspace discovery |
| `KeywordsAutocomplete` | `bool` | `false` | Include F# language keywords in completion lists |
| `ResolveNamespaces` | `bool` | `false` | Offer unopened module/namespace completions with automatic `open` insertion |
| `IndentationSize` | `int` | `4` | Indentation width used by code-generation code actions and formatting fallback |
| `UnusedOpensAnalyzer` | `bool` | `false` | Report unused `open` declarations (`FSAC0001` / `Unnecessary` tag) |
| `UnusedDeclarationsAnalyzer` | `bool` | `false` | Report unused bindings and parameters (`FSAC0003` / `Unnecessary` tag) |
| `SimplifyNameAnalyzer` | `bool` | `false` | Report redundant qualified prefixes (`FSAC0002` / `Unnecessary` tag) |
| `UnnecessaryParenthesesAnalyzer` | `bool` | `true` | Report unnecessary parentheses (`FSAC0004` / `Unnecessary` tag) |
| `UnionCaseStubGeneration` | `bool` | `false` | Enable `Generate union pattern match case` code actions |
| `RecordStubGeneration` | `bool` | `false` | Enable `Generate record stubs` code actions |
| `InterfaceStubGeneration` | `bool` | `false` | Enable `Implement interface` stub generation code actions |
| `AbstractClassStubGeneration` | `bool` | `true` | Enable `Implement abstract class` stub generation code actions |
| `InlayHints.TypeAnnotations` | `bool` | `true` | Show inferred type annotation inlay hints |
| `InlayHints.ParameterNames` | `bool` | `true` | Show parameter name inlay hints at call sites |
| `CodeLenses.Signature.Enabled` | `bool` | `true` | Show inferred signature CodeLenses above top-level bindings |
| `CodeLenses.References.Enabled` | `bool` | `true` | Show reference count CodeLenses |
| `PipelineHints.Enabled` / `InlineValues.Enabled` | `bool` | `true` | Provide inferred stage types along `\|>` pipelines |
| `LineLens.Enabled` | `string` | `"never"` | Ionide LineLens mode (`"never"`, `"replaceCodeLens"`, `"always"`) |

---

## Building & Packing from Source

### Repository Layout

`ranvier-fsharp` builds against the sibling [`fickle-fsharp`](https://github.com/shayanhabibi/fickle-fsharp) fork (`fickle_decl` branch), which exposes declaration-level checking and non-retaining semantic classification in `FSharp.Compiler.Service.dll` + `FSharp.Core.dll`.

```text
parent-dir/
├── fickle-fsharp/    # https://github.com/shayanhabibi/fickle-fsharp (branch: fickle_decl)
└── ranvier-fsharp/   # https://github.com/ranvier-org/ranvier-fsharp (branch: main)
```

> Override the fork output directory at any time with `-p:FickleFcsDir=/path/to/net10.0/` (or environment variable `FickleFcsDir`).

### 1. Clone and Build the FCS Fork

```bash
git clone -b fickle_decl https://github.com/shayanhabibi/fickle-fsharp.git
git clone https://github.com/ranvier-org/ranvier-fsharp.git
cd ranvier-fsharp

# Builds fslex, fsyacc, and FSharp.Compiler.Service targeting net10.0:
pwsh -File build/fork.ps1
```

### 2. Build & Test `ranvier-fsharp`

```bash
dotnet build RanvierFSharp.slnx -c Release

# Run the test suites:
dotnet run --project tests/Rfs.Lsp.Tests -c Release --no-build
dotnet run --project tests/Rfs.Harness.Tests -c Release --no-build
dotnet run --project tests/Rfs.Checkpoint.Tests -c Release --no-build
dotnet run --project tests/Rfs.Decl.Tests -c Release --no-build
dotnet run --project tests/Rfs.Checker.Tests -c Release --no-build
dotnet run --project tests/Rfs.Server.Tests -c Release --no-build
```

### 3. Pack `.NET` Tool & Portable Bundle

```powershell
pwsh -File build/pack.ps1
```

This outputs:
- `artifacts/nupkg/Ranvier.AutoComplete.<version>.nupkg` — installable via `dotnet tool install -g Ranvier.AutoComplete --add-source ./artifacts/nupkg --prerelease`
- `artifacts/publish/ranvierautocomplete.dll` — ready to set as `"FSharp.fsac.netCoreDllPath"` in VS Code

---

## Debugging & Causal Graph Inspection (`rfs/why`)

- **Release builds** (`-c Release`, default for `pack.ps1`) link the untraced `Ranvier` package for zero tracing overhead.
- **Debug / Traced builds** (`-c Debug` or `-p:RanvierTrace=true`) link `Ranvier.Traced` and enable the custom **`rfs/why`** JSON-RPC request (`{"uri": "file:///...", "version": 3}`), returning the exact causal chain from changed input signals to the last diagnostic publish.

---

## Publishing & Releases

1. Ensure `shayanhabibi/fickle-fsharp` has the latest `fickle_decl` branch pushed:
   ```bash
   git -C ../fickle-fsharp push origin fickle_decl
   ```
2. Push `main` and tag a release (`v0.1.0-preview.1`):
   ```bash
   git push -u origin main
   git tag v0.1.0-preview.1
   git push origin v0.1.0-preview.1
   ```
   The [`.github/workflows/release.yml`](.github/workflows/release.yml) workflow builds `fickle-fsharp` + `ranvier-fsharp`, smoke-tests the `.NET` tool, attaches portable & RID archives (`win-x64`, `linux-x64`, `osx-arm64`) + `.nupkg` to a GitHub Release, and pushes `Ranvier.AutoComplete` to NuGet.org when `NUGET_API_KEY` is configured in repository secrets.

For manual NuGet publication:
```bash
pwsh -File build/pack.ps1 -Version 0.1.0-preview.1
dotnet nuget push ./artifacts/nupkg/Ranvier.AutoComplete.0.1.0-preview.1.nupkg \
  --api-key <NUGET_API_KEY> \
  --source https://api.nuget.org/v3/index.json
```

## License

[MIT](LICENSE) — Copyright (c) 2026 Shayan Habibi.
