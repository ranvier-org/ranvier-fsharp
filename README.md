# Ranvier.AutoComplete (`ranvierautocomplete`)

Demand-driven, declaration-incremental F# Language Server Protocol (LSP) server built on [Ranvier](https://github.com/ranvier-org/Ranvier) and [`fickle-fsharp`](https://github.com/shayanhabibi/fickle-fsharp) (`FSharp.Compiler.Service`).

`ranvierautocomplete` is a drop-in replacement for `fsautocomplete` (FSAC) in **Ionide for VS Code**, **Neovim**, **Helix**, **Zed**, **Emacs**, and any stdio LSP client.

> **Massive Props to the [Ionide](https://ionide.io) Team:** `Ranvier.AutoComplete` would not exist without the talent and dedication of the **[Ionide](https://github.com/ionide)** maintainers and contributors. The language server surface, protocol handling, workspace loading, analyzers, code actions, and editor feature behavior in this repository are largely based on and adapted from their landmark work across **[FsAutoComplete (FSAC)](https://github.com/ionide/FsAutoComplete)**, **[Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol)**, **[Ionide.ProjInfo](https://github.com/ionide/proj-info)**, and **[ionide-vscode-fsharp](https://github.com/ionide/ionide-vscode-fsharp)**. While `ranvier-fsharp` introduces a new declaration-incremental [Ranvier](https://github.com/ranvier-org/Ranvier) checking graph underneath, the rich F# IDE experience it delivers stands squarely on the foundation Ionide built for the F# community. See [Acknowledgements & Attribution](#acknowledgements--attribution) for full credit.

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

From NuGet.org:
```bash
dotnet tool install -g Ranvier.AutoComplete
```

Or from a locally packed `.nupkg` (see [Building & Packing from Source](#building--packing-from-source)):
```bash
dotnet tool install -g Ranvier.AutoComplete --add-source ./artifacts/nupkg
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

### Option C — Consume as `.NET` Libraries from NuGet

All layers of `ranvier-fsharp` are published as composable `net10.0` NuGet packages (in both zero-overhead untraced and `.Traced` variants):

| Package | Assembly / Namespace | Description |
| --- | --- | --- |
| `Ranvier.AutoComplete` | `ranvierautocomplete` (`.NET` Tool) | Drop-in `stdio` F# LSP server CLI executable for Ionide and LSP editors |
| `Ranvier.FSharp.Lsp` | `Rfs.Lsp` | Framing (`JsonRpc`), UTF-16 incremental buffer (`Document`), and `LspClient` |
| `Ranvier.FSharp.Projects` | `Rfs.Projects` | MSBuild `ProjectSpec` evaluation (`MSBuildLocator`), `.sln`/`.slnx` workspace discovery, `Glob`, and bundled `fickle-fsharp` `FSharp.Compiler.Service` |
| `Ranvier.FSharp.Checker` | `Rfs.Checker` | Demand-driven declaration-incremental F# type-checking graph (`GraphHost`, `ProjectGraph`) built on `Ranvier` |
| `Ranvier.FSharp.Checker.Traced` | `Rfs.Checker` | Traced build of `Ranvier.FSharp.Checker` with causal node labels (`ProjectGraph.Why`), built on `Ranvier.Traced` |
| `Ranvier.FSharp.Server` | `Rfs.Server` | Embeddable in-process F# LSP server (`Server.run` / `Server.runWith` / `RanvierServer`) |
| `Ranvier.FSharp.Server.Traced` | `Rfs.Server` | Traced build of `Ranvier.FSharp.Server` with `rfs/why` causal inspection enabled |

```bash
# Embed the reactive F# type-checking graph directly:
dotnet add package Ranvier.FSharp.Checker --prerelease

# Or embed the full in-process LSP server (untraced or traced):
dotnet add package Ranvier.FSharp.Server --prerelease
# dotnet add package Ranvier.FSharp.Server.Traced --prerelease
```

> `Ranvier.FSharp.Projects` bundles the declaration-incremental `fickle-fsharp` `FSharp.Compiler.Service.dll` directly in `lib/net10.0/`, and `Ranvier.FSharp.Server` (`.Traced`) includes transitive MSBuild `.targets` so `MSBuildLocator` works out of the box without manual `StringTools` exclusions.

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
  "FSharp.pipelineHints.enabled": true,
  // Optional: publish diagnostics for unopened files on project load (default false):
  "FSharp.workspaceDiagnostics": false
}
```

> **Closed-file & downstream diagnostics:** By default (`"FSharp.workspaceDiagnostics": false`), live typing updates diagnostics for open editors only, closing a tab retains any real on-disk problems in the Problems panel (discarding unsaved buffer-only errors), and saving a file (`Ctrl+S` / `Cmd+S`) or external disk changes settles and publishes downstream breaks across closed files in the same and referencing projects. Set `"FSharp.workspaceDiagnostics": true` (or pass `--workspace-diagnostics` on the CLI) to also publish diagnostics for unopened files immediately on cold project load.

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
| `WorkspaceDiagnostics` | `bool` | `false` | Publish compiler diagnostics for unopened files across all loaded projects on load/reload (also available via `--workspace-diagnostics` CLI flag); live typing remains open-file-only until save |
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

### 3. Pack & Verify All NuGet Packages + Portable Bundle

```powershell
pwsh -File build/pack.ps1
pwsh -File build/verify-packages.ps1 -NupkgDir ./artifacts/nupkg
```

This outputs all 7 `.nupkg` archives in `artifacts/nupkg/` (`Ranvier.FSharp.{Lsp,Projects,Checker,Checker.Traced,Server,Server.Traced}` + `Ranvier.AutoComplete`) and `artifacts/publish/ranvierautocomplete.dll` (ready to set as `"FSharp.fsac.netCoreDllPath"` in VS Code), then verifies them in isolated out-of-repo F# and C# consumer builds.

---

## Debugging & Causal Graph Inspection (`rfs/why`)

- **Untraced packages / Release builds** (`Ranvier.FSharp.Checker`, `Ranvier.FSharp.Server`, `Ranvier.AutoComplete`) link the untraced `Ranvier` package for zero tracing overhead.
- **Traced packages / Debug builds** (`Ranvier.FSharp.Checker.Traced`, `Ranvier.FSharp.Server.Traced`, or `-c Debug` / `-p:RanvierTrace=true`) link `Ranvier.Traced` and enable the custom **`rfs/why`** JSON-RPC request (`{"textDocument": {"uri": "file:///..."}, "position": {"line": 4, "character": 5}}`), returning the exact causal chain from changed input signals to the declaration's last check.

---

## Publishing & Releases

1. Ensure `shayanhabibi/fickle-fsharp` has the latest `fickle_decl` branch pushed:
   ```bash
   git -C ../fickle-fsharp push origin fickle_decl
   ```
2. Push `main` and tag either a standalone CLI tool release (`ranvierautocomplete-v0.1.1`) or a full workspace suite release (`v0.1.0`):
   ```bash
   git push -u origin main

   # Release Ranvier.AutoComplete CLI tool + standalone archives:
   git tag ranvierautocomplete-v0.1.1
   git push origin ranvierautocomplete-v0.1.1

   # Or release all 7 NuGet packages (libraries + CLI tool):
   # git tag v0.1.0
   # git push origin v0.1.0
   ```
   The [`.github/workflows/publish.yml`](.github/workflows/publish.yml) workflow authenticates via NuGet Trusted Publishing (OIDC, `NuGet/login@v1`), builds `fickle-fsharp` + `ranvier-fsharp`, runs isolated consumer smoke tests (`build/verify-packages.ps1`), pushes to NuGet.org, and attaches portable & RID archives (`win-x64`, `linux-x64`, `osx-arm64`) + `.nupkg` files to a GitHub Release.

To verify published packages live from `https://api.nuget.org/v3/index.json` in a clean NuGet cache:
```powershell
# Verify the 0.1.1 CLI tool:
pwsh -File build/verify-packages.ps1 -OnlineOnly -ToolOnly -ToolVersion 0.1.1

# Verify the full library + tool suite:
pwsh -File build/verify-packages.ps1 -OnlineOnly
```

---

## Acknowledgements & Attribution

`ranvier-fsharp` stands on the shoulders of the **[Ionide](https://ionide.io)** team ([github.com/ionide](https://github.com/ionide)), without whom this project simply would not be possible. Over more than a decade of open-source stewardship, the Ionide maintainers and contributors designed, refined, and maintained the modern cross-editor F# developer experience — and the code in this repository is largely based on their talented work:

- **[FsAutoComplete (FSAC)](https://github.com/ionide/FsAutoComplete)** *(Apache-2.0 — Tomas Petricek, Ben Winkel, Igor Siguta, Robin Neatherway, Dave Thomas, Krzysztof Cieślak, Chet Husk, Jimmy Byrd, Florian Verdonck, and all FSAC contributors)*: `Ranvier.FSharp.Server` draws heavily on FSAC's architecture, protocol conventions, and feature semantics — including hover & signature formatting, completion & auto-open namespace edits, semantic token mapping, inlay hints, code lenses, pipeline/inline value hints, navigation & rename rules, built-in `FSAC0001`–`FSAC0004` analyzers, quick fixes & stub-generation code actions, and the custom `fsharp/*` (`workspacePeek`, `workspaceLoad`, `project`, `lineLens`, `pipelineHint`, `documentation*`) protocol surface.
- **[Ionide.LanguageServerProtocol](https://github.com/ionide/LanguageServerProtocol)** *(MIT — Krzysztof Cieślak & Ionide contributors)*: Provides the strongly typed LSP protocol types and serialization contracts used across `Ranvier.FSharp.Server`.
- **[Ionide.ProjInfo](https://github.com/ionide/proj-info)** *(MIT — Enrico Sada, Chet Husk & Ionide contributors)*: Powers MSBuild SDK discovery (`MSBuildLocator`), design-time F# project evaluation, and `.sln` inspection in `Ranvier.FSharp.Projects`.
- **[ionide-vscode-fsharp](https://github.com/ionide/ionide-vscode-fsharp)** *(MIT — Krzysztof Cieślak, Chet Husk, Jimmy Byrd & Ionide contributors)*: The flagship F# extension for VS Code whose `FSharp.fsac.netCoreDllPath` integration makes `ranvierautocomplete` a seamless drop-in experience.

Special thanks as well to:
- **The F# Compiler & `FSharp.Compiler.Service` Team** ([dotnet/fsharp](https://github.com/dotnet/fsharp), `.NET Foundation` and contributors), upon which [`fickle-fsharp`](https://github.com/shayanhabibi/fickle-fsharp) builds declaration-level incremental type-checking.
- **[Fantomas](https://github.com/fsprojects/fantomas)** *(Florian Verdonck, Dave Thomas & fsprojects contributors)* for F# source formatting.

Full upstream copyright and license notices are included in [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md) (distributed in every NuGet package and release archive).

## License

[MIT](LICENSE) — Copyright (c) 2026 Shayan Habibi. See [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md) for third-party attribution and Apache-2.0 / MIT notices for adapted upstream components.
