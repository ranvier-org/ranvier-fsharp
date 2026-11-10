# Full FSAC LSP & Ionide Protocol Parity on `ranvier-fsharp`

**Date:** 2026-11-11
**Status:** Approved (Autonomous Execution Authorized)
**Repositories:**
- `ranvier-fsharp` (`C:\Users\shaya\RiderProjects\ranvier-fsharp`, worktree `ranvier-fsharp-parity` on `lsp-parity`)
- `fickle-fsharp` (`C:\Users\shaya\RiderProjects\fickle-fsharp`, worktree `fickle-fsharp-i1` on `lsp-parity`)

---

## 1. Goal & Scope Boundaries

Bring `ranvierautocomplete` (`Rfs.Server` + `Rfs.Checker` + targeted `fickle-fsharp` fork support) to **full FSAC LSP & Ionide protocol parity** across all compiler-, AST-, and workspace-backed language server features while preserving Ranvier's core reactive invariants (`0` oracle diffs, single-state consistent graph reads, plain-data memos, zero unpromoted `FSharpCheckFileResults` retention, and `install nothing`).

### 1.1 In-Scope Feature Surface

1. **Semantic Highlighting (`textDocument/semanticTokens/full`, `textDocument/semanticTokens/full/delta`, `textDocument/semanticTokens/range`)**:
   - Fork parity fix in `fcs/service_decl.fs`: wire `ISourceText` into all `TcResultsSinkImpl` constructions (`buildCheckResults`, `CheckSignatureFile`, `BeginFile`, `CheckDeclarationLocked` via `ParsedDeclaration.Source`, and `FinishFile`) so `CheckFormatStrings` records `Printf` and interpolated-string format-specifier ranges (`%d`, `%s`, `%0.2f`, `%%`, etc.) identically to `FSharpChecker`, including across `DeclarationChecker.Shift` (`W1`).
   - Expose plain-data `SemanticClassification: SemanticClassificationItem[]` on `FileSignature` (for `.fsi` files, captured at `CheckSignatureFile` with zero extra checks) and `FileResult.GetSemanticClassification(?range: range)` on `FileResult` (merging sinks directly on declaration-checked `FinishFile` results without forcing `FileResult.CheckResults`, running `conformFile`, or running `FoldImplFile`, and using `CheckResultsNotKept` on `CheckFile` whole results so unpromoted files retain nothing).
   - Match FSAC's exact 26-entry `SemanticTokenTypes` and 13-entry `SemanticTokenModifier` legend (`ClassificationUtils`), `scrubRanges`, AST parameter-definition override (`LocalValue` -> `Parameter`), `SynType.WithNull` `null` keyword tokens, multiline token splitting, LSP relative `uint32[]` encoding, and `resultId` delta computation (`SemanticTokensDelta` with minimal `SemanticTokensEdit`).
   - Support both `.fs` and `.fsi` files.

2. **Runtime Configuration (`workspace/didChangeConfiguration`, `FSharpConfig`)**:
   - Parse Ionide's `{ "FSharp": ... }` configuration payload in `Initialize` (`InitializationOptions`) and `WorkspaceDidChangeConfiguration`, plus pull `workspace/configuration` (`section = "FSharp"`) when client capability `workspace.configuration` is true.
   - Store an immutable `FSharpConfig` snapshot (`Volatile.Read`/`Write` or lock-free reference swap) controlling `InlayHints`, `CodeLenses`, `LineLens`, `InlineValues`/`PipelineHints`, built-in analyzer toggles (`UnusedOpensAnalyzer`, `UnusedDeclarationsAnalyzer`, `SimplifyNameAnalyzer`, `UnnecessaryParenthesesAnalyzer`), `ResolveNamespaces`, `KeywordsAutocomplete`, and stub-generation templates (`InterfaceStubGeneration*`, `UnionCaseStubGeneration*`, `RecordStubGeneration*`, `AbstractClassStubGeneration*`).

3. **Pure-Syntax / Zero-Check Features (`foldingRange`, `selectionRange`, `documentSymbol`)**:
   - Expose `ProjectGraph.LatestParse(file: string, ct: CancellationToken) : Task<TextView>` supporting **both `.fs` and `.fsi`** files, waiting only for the latest text's parse (`Parsed`) and **never** waiting on typechecking.
   - `textDocument/foldingRange`: `FSharp.Compiler.EditorServices.Structure.getOutliningRanges` mapped to `FoldingRangeKind` (`Imports`, `Comment`, or `None`).
   - `textDocument/selectionRange`: nested parent-chain `SelectionRange`s enclosing each requested cursor position from `ParsedInput` (`.fs` and `.fsi`).
   - `textDocument/documentSymbol`: hierarchical `DocumentSymbol[]` (`U2.C2`) via `FSharp.Compiler.EditorServices.Navigation.getNavigation` (`GetNavigationItems`), honoring `FSharpGlyph` -> `SymbolKind` mapping.

4. **Signature Help & Extended Navigation (`signatureHelp`, `documentHighlight`, `declaration`, `typeDefinition`, `implementation`, `.fsi` navigation)**:
   - Expose `ProjectGraph.ReadSignature(file: string, ct: CancellationToken) : Task<struct (TextView * FileSignature * FileUses)>` so `.fsi` files participate in settled single-file queries (`semanticTokens`, `documentHighlight`, `definition`, `declaration`, `implementation`).
   - `textDocument/signatureHelp` (`TriggerCharacters = [| "("; ","; " " |]`, `RetriggerCharacters = [| ","; ")"; " " |]`): method overloads (`GetMethods` / `FindParameterLocations`) and curried function applications from `FileView.CheckResultsAt(line, col)`, with active signature, active parameter, parameter labels, and live `FileDocs` markdown documentation.
   - `textDocument/documentHighlight`: same-file symbol occurrences (`DocumentHighlightKind.Write` for definitions/mutations, `Read` for uses) on both `.fs` and `.fsi`.
   - `textDocument/declaration` & `.fsi` <-> `.fs` `definition` toggle:
     - `textDocument/declaration` jumps to the `.fsi` signature declaration when a paired signature defines the symbol (else primary declaration).
     - `textDocument/definition` from a `.fsi` declaration jumps to the corresponding `.fs` implementation definition (matching Ionide's `.fsi` <-> `.fs` navigation workflow).
   - `textDocument/typeDefinition`: resolves the symbol at cursor (`FSharpMemberOrFunctionOrValue.FullType`, `FSharpField.FieldType`, `FSharpUnionCase.ReturnType`, `FSharpEntity`), extracts its head `FSharpEntity`, and resolves its current workspace location via `SymbolKey` + `CurrentLocation`.
   - `textDocument/implementation`: jumps from an interface `FSharpEntity`, abstract member/dispatch slot, or `.fsi` declaration to all concrete implementations across the workspace in one consistent graph turn (`ReadInOneTurn` over plain-data implementation relations in `FileUses`).

5. **Workspace Symbols, Call Hierarchy & Type Hierarchy**:
   - Plain-data per-file syntax navigation memo (`NavigableItem[]` / declaration metadata on `FileText`) read across all loaded `ProjectGraph`s without forcing checks, plus plain-data type-inheritance and enclosing-binding call relations in `FileUses` read in one consistent turn via `ReadInOneTurn`:
   - `workspace/symbol` & `workspaceSymbol/resolve`: case-insensitive substring and camelCase-initials matching across all workspace `.fs` and `.fsi` files.
   - `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls`:
     - `prepareCallHierarchy`: resolves callable symbol at cursor and returns `CallHierarchyItem` carrying serialized plain-data target identity (`Data`).
     - `incomingCalls`: one-turn workspace-wide lookup (`ReadInOneTurn`) finding all call sites of the target key and grouping them by their enclosing declaration/member in each caller file.
     - `outgoingCalls`: resolves callees invoked within the `CallHierarchyItem`'s enclosing declaration body range and resolves their current declaration ranges.
   - `textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`, `typeHierarchy/subtypes`:
     - `prepareTypeHierarchy`: resolves `FSharpEntity` at cursor.
     - `supertypes`: resolves base type and declared interfaces of the entity to workspace `TypeHierarchyItem`s (when workspace-defined) or external summary items.
     - `subtypes`: one-turn workspace-wide lookup (`ReadInOneTurn`) of all workspace entities whose plain-data supertype keys include the target entity's `SymbolKey`.

6. **Safe Cross-Project Rename, Inlay Hints, CodeLens, Inline Value & Ionide Custom Endpoints**:
   - `textDocument/prepareRename` & `textDocument/rename` (`PrepareProvider = Some true`):
     - Validates target symbol is defined in loaded workspace sources (never external DLLs), is not an active-pattern case or unrenamable intrinsic/operator construct unless valid identifier rules are met, isolates the exact identifier sub-range (stripping module/type qualifiers and handling ` ``...`` ` backticked identifiers), and computes a single-turn cross-project + `.fs`/`/.fsi` paired `WorkspaceEdit` (`Changes`) via `ReadInOneTurn`. Automatically wraps/unwraps ` ``...`` ` when renaming to/from F# reserved keywords or non-plain identifiers, and rejects empty or invalid names containing backticks/newlines.
   - `textDocument/inlayHint` (`ResolveProvider = Some false`):
     - `InlayHintKind.Type` (`: <type>` after untyped `let`/parameter bindings, with `TextEdits` to insert the explicit annotation) and `InlayHintKind.Parameter` (`<paramName> =` at non-obvious argument sites, `PaddingRight = Some true`), scoped to the requested `Range` and respecting `FSharpConfig.InlayHints`.
   - `textDocument/codeLens` & `codeLens/resolve`:
     - Emits `signature` and `reference` lenses on declarations from `LatestParse` (`Navigation.getNavigation`) when enabled by `FSharpConfig.CodeLenses` and `FSharpConfig.LineLens.Enabled <> "replaceCodeLens"`.
     - `codeLens/resolve` formats the type signature (`ReadFile` + `CheckResultsAt`) for signature lenses and counts workspace uses (`References.find`) for reference lenses (`fsharp.showReferences`).
   - `textDocument/inlineValue`:
     - Emits pipeline intermediate type hints on `|>` lines within the requested viewport range (`InlineValue.C1`).
   - **Ionide Custom Endpoints & Notifications**:
     - `fsharp/signature` -> `PlainNotification` (`Kind = "typesig"`, `Data = string`)
     - `fsharp/signatureData` -> `PlainNotification` (`Kind = "signatureData"`, `Data = { OutputType; Parameters; Generics }`)
     - `fsharp/lineLens` -> `PlainNotification` (`Kind = "declarations"`, `Data = DeclarationResponse[]` matching `CommandResponse.declarations`)
     - `fsharp/pipelineHint` -> `PlainNotification` (`Kind = "pipelineHint"`, `Data = { Line; Types; PrecedingNonPipeExprLine }[]`)
     - `fsharp/f1Help` -> `PlainNotification` (`Kind = "help"`, `Data = string` via `GetF1Keyword`)
     - `fsharp/documentation` & `fsharp/documentationSymbol` -> `PlainNotification` (`Kind = "formattedDocumentation"`, `Data = DocumentationDescription`)
     - `fsharp/documentationGenerator` -> computes XML `/// <summary>` + `<param>` + `<returns>` stub at cursor and applies via `client.WorkspaceApplyEdit`
     - `fsharp/compilerLocation` -> `PlainNotification` (`Kind = "compilerlocation"`, `Data = { Fsc; Fsi; MSBuild; SdkRoot }`)
     - `fsharp/loadAnalyzers` -> graceful `Ok ()` no-op so Ionide never faults on startup
     - Rich `fsharp/notifyWorkspace` metadata in `Workspace.fs` (`Output`, `TargetFramework`, `IsTestProject`, `PackageReferences` populated from `ProjectSpec` where available) plus `fsharp/fileParsed` and `fsharp/documentAnalyzed` notifications on `RanvierClient`/`Publisher` so Ionide refreshes LineLens and PipelineHints reactively!
   - Also add `"{"` to `CompletionProvider.TriggerCharacters` (`[| "."; "'"; "{" |]`) and keyword completions when `KeywordsAutocomplete` is enabled.

7. **Built-In Diagnostics Analyzers, Code Actions / Code Fixes & Out-of-Process Fantomas Formatting**:
   - Optional built-in diagnostic hints in `Publisher` when enabled in `FSharpConfig`: `FSAC0001` (`UnusedOpens.getUnusedOpens`, `DiagnosticTag.Unnecessary`), `FSAC0002` (`SimplifyNames.getSimplifiableNames`, `DiagnosticTag.Unnecessary`), `FSAC0003` (`UnusedDeclarations.getUnusedDeclarations`, `DiagnosticTag.Unnecessary`), plus `DiagnosticTag.Unnecessary` on compiler `FS1182` / `FS0044` (`Deprecated`).
   - `textDocument/codeAction` (`QuickFix` + `Refactor*`) covering 12 high-value compiler, FCS-analyzer, and AST fixes:
     1. Remove unused `open` (`FSAC0001` / on-demand `UnusedOpens.getUnusedOpens`)
     2. Prefix/replace unused value with `_` (`FS1182` / `FSAC0003`)
     3. Simplify redundant qualified name (`FSAC0002` / on-demand `SimplifyNames.getSimplifiableNames`)
     4. Unresolved identifier (`FS0039`): replace with compiler spellcheck suggestion (`Maybe you want one of the following:`) AND suggest `open <Namespace>` / qualify `<Namespace>.<Ident>` from project + referenced assembly entities (`ResolveNamespaces`)
     5. Add `rec` to `let` / `module` (`FS0039` self-reference or `FS0576` mutual recursion)
     6. Add `new` keyword to `IDisposable` constructor invocation (`FS0760`)
     7. Fix `=` vs `<-` / missing `=` (`FS0020` mutable assignment `x <- y` + `|> ignore`, `FS3360` missing `=` in type definition, `==` -> `=`, `!=` -> `<>`)
     8. Add explicit type annotation to binding/parameter at cursor
     9. Implement interface / object-expression stub (`FS0366` or cursor on `interface ...`, via `InterfaceStubGenerator`)
     10. Generate missing union cases in `match` (`FS0025`)
     11. Generate missing record fields (`FS0764`)
     12. Sync paired `.fsi` signature file (`FS0034` update value signature / `FS0193` or refactoring add missing `val` binding to `.fsi`).
   - `textDocument/formatting` & `textDocument/rangeFormatting`:
     - Out-of-process invocation of `dotnet fantomas` (local tool or PATH `fantomas`) via `System.Diagnostics.Process` with stdin/temp-file diffing into minimal whole-document/range `TextEdit[]`, respecting `"install nothing"` (zero new NuGet dependencies) and returning `Ok None` cleanly when `fantomas` is not installed.

### 1.2 Explicitly Out-of-Scope External Subsystems

The following require non-LSP external runtimes, dynamic third-party plugin hosting, or prohibited package installations and are intentionally out of scope:
- `.fsx` FSI interactive session process hosting (`fsharp/fsi*`).
- External `FSharp.Analyzers.SDK` dynamic assembly loading and `FSharpLint` (`fsharp/loadAnalyzers` is wired as a clean no-op).
- `VSTest` / `Microsoft.Testing.Platform` test execution (`test/discoverTests`, `test/runTests`).
- `dotnet new` / `dotnet sln` CLI wrappers and MSBuild `.fsproj` XML item reordering (`fsharp/dotnetnew*`, `fsproj/*`).
- Retired `fsharp/fsdn` endpoint.
- External binary decompilation (`ICSharpCode.Decompiler` / SourceLink HTTP fetching — `"install nothing"`).

---

## 2. Core Reactive & Memory Invariants

Every feature MUST uphold all Ranvier checker and server invariants:

1. **Zero Wrong Answers (`0` Oracle Diffs):**
   - Settled answers reflect the exact state of the graph turn read. Stale/last-state answers are only served where explicitly designed and progress-marked (`Hover`, `Definition`, `References`).
2. **One Consistent Turn Per Cross-File Answer:**
   - Every cross-file or cross-project query (`implementation`, `incomingCalls`, `subtypes`, `rename`, `workspace/symbol`, `codeLens` reference counts) reads across graphs on the shared `GraphHost` in **one single turn** (`ReadInOneTurn` or `WhenSettled`), never `Peek`ing across nodes or dispatching inside a batch.
3. **Plain Data Only Past the Graph:**
   - Every value stored in a memo, `FileUses`, `FileSignature.SemanticClassification`, `NavigableItem` index, or `ConditionalWeakTable` cache MUST be 100% plain data (strings, numbers, structs of `range` + enums, plain records) — **never** `FSharpSymbol`, `FSharpEntity`, `TcState`, `DeclarationResult`, or `FSharpCheckFileResults`.
4. **Lock & Unpromoted-Memory Discipline:**
   - Live compiler reads on declaration-checked files run exclusively inside `ProjectGraph.ReadFile` (holding `FileStart.Lock` exclusively and verifying `inEffect` so superseded `FileStart`s never race with `UndoLastFinish` or `conformFile`).
   - Whole-checked (`CheckFile`) files MUST use `CheckResultsNotKept` (or `FileResult.GetSemanticClassification` which delegates to `CheckResultsNotKept` when unbuilt) so unpromoted closed files NEVER retain `FSharpCheckFileResults` in memory.
   - Declaration-checked (`FinishFile`) semantic classification merges `sinks` (`start.Sink`, `results`, `exits`, `FinishFile.sink`) directly without forcing `FileResult.CheckResults`, running `conformFile`, or folding `CheckedImplFile`.

---

## 3. Mandatory Verification & Mutation-Testing Gate

1. **TDD First:** Every task writes failing tests (`RED`), verifies failure for the expected reason, implements minimal clean production code (`GREEN`), and passes all existing + new tests.
2. **Mandatory Mutation Testing Per Task:**
   - Before committing each task, systematically mutate critical predicates, line/column offsets (`+1`/`-1`), bitmasks/encodings, range boundary comparisons (`<` vs `<=`), guard conditions, and fallback branches in the task's newly written code (at least 6–10 targeted mutants per task), run the task's test suite against each mutant, require **100% mutant kill rate (`0` surviving mutants)** — adding missing assertions/tests whenever any mutant survives — and restore the clean working tree and re-verify green before committing.
3. **Whole-Branch Suites & Benchmark Check:**
   - All test projects (`Rfs.Decl.Tests`, `Rfs.Checker.Tests` with `RFS_M6_TESTS=1`, `Rfs.Server.Tests`, `Rfs.Lsp.Tests`, `Rfs.Harness.Tests`, `Rfs.Checkpoint.Tests`) pass in `Release`, and `small` harness check passes with `0` oracle diffs.
