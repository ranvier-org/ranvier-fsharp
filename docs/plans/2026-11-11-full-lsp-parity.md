# Full FSAC LSP & Ionide Protocol Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `subagent-driven-development` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver full FSAC LSP & Ionide protocol parity in `ranvierautocomplete` (`Rfs.Server` + `Rfs.Checker` + targeted `fickle-fsharp` fork parity fixes), explicitly including 100% oracle-exact Semantic Highlighting (`full`, `full/delta`, `range`), pure-syntax features, signature help, extended navigation, workspace symbols, call/type hierarchies, cross-project rename, inlay hints, code lenses, inline values, Ionide `fsharp/*` custom endpoints, built-in analyzers + 12 code-action families, and out-of-process Fantomas formatting — verified with TDD and **100% mutant-kill mutation testing** on every task.

**Architecture:**
- **Fork layer (`C:\Users\shaya\RiderProjects\fickle-fsharp-i1`, branch `lsp-parity`):** Pass the exact `ISourceText` of each parse/declaration into every `TcResultsSinkImpl` construction in `fcs/service_decl.fs` so `Printf` and interpolated format specifiers are captured and shifted identically to `FSharpChecker`; expose plain-data `FileSignature.SemanticClassification` for `.fsi` files and lightweight `FileResult.GetSemanticClassification(?range)` for `.fs` files (merging sinks directly on `FinishFile` without forcing `FileResult.CheckResults`, `conformFile`, or `FoldImplFile`, and delegating to `CheckResultsNotKept` on `CheckFile`).
- **Checker layer (`src/Rfs.Checker`):** Add non-typecheck-blocking `ProjectGraph.LatestParse` & `ProjectGraph.LatestParsesInOneTurn` (for `.fs` and `.fsi`), settled `ProjectGraph.ReadSignature` (for `.fsi`), and single-pass plain-data relations in `FileUses` (`ImplementationsOf`, `SubtypesOf`, `EnclosingCallables`) so every cross-file feature runs in one atomic settled graph turn (`ReadInOneTurn`) holding zero live compiler objects.
- **Server layer (`src/Rfs.Server`):** Add thread-safe runtime `FSharpConfig`, modular `Features/*.fs` handlers adhering strictly to FSAC's wire schemas (`ClassificationUtils`, `CommandResponse`, `Ionide.LanguageServerProtocol.Types`), capability registration in `Initialize`, and method registration in `Server.implemented` + `Server.handlings ()`.

**Tech Stack:** F# / .NET 10 (`net10.0`), `Ranvier` reactive graph, `fickle-fsharp` (`FSharp.Compiler.Service` declaration-checking fork), `Ionide.LanguageServerProtocol` `0.7.0`, `Newtonsoft.Json`, Expecto / xUnit test suites. **Zero new NuGet packages (`install nothing`).**

**Build & Test Convention Against Isolated Fork Worktree:**
All builds and tests on `ranvier-fsharp-parity` MUST pass:
```powershell
-p:FickleFcsDir=C:\Users\shaya\RiderProjects\fickle-fsharp-i1\artifacts\bin\FSharp.Compiler.Service\Release\net10.0\
```
and prefix shell commands with `rtk`.

---

## File Structure Overview

- **Fork (`C:\Users\shaya\RiderProjects\fickle-fsharp-i1`):**
  - Modify: `fcs/service_decl.fs`
- **Checker (`C:\Users\shaya\RiderProjects\ranvier-fsharp-parity\src\Rfs.Checker`):**
  - Modify: `FileUses.fs`
  - Modify: `FileLevel.fs`
  - Modify: `ProjectGraph.fs`
- **Server (`C:\Users\shaya\RiderProjects\ranvier-fsharp-parity\src\Rfs.Server`):**
  - Create: `Config.fs`
  - Create: `Features/SemanticTokens.fs`
  - Create: `Features/Folding.fs`
  - Create: `Features/SelectionRange.fs`
  - Create: `Features/DocumentSymbols.fs`
  - Create: `Features/SignatureHelp.fs`
  - Create: `Features/DocumentHighlight.fs`
  - Create: `Features/Declaration.fs`
  - Create: `Features/TypeDefinition.fs`
  - Create: `Features/Implementation.fs`
  - Create: `Features/WorkspaceSymbols.fs`
  - Create: `Features/CallHierarchy.fs`
  - Create: `Features/TypeHierarchy.fs`
  - Create: `Features/Rename.fs`
  - Create: `Features/InlayHints.fs`
  - Create: `Features/CodeLens.fs`
  - Create: `Features/InlineValue.fs`
  - Create: `Features/IonideEndpoints.fs`
  - Create: `Features/CodeActions.fs`
  - Create: `Features/Formatting.fs`
  - Modify: `Client.fs`, `Workspace.fs`, `Publisher.fs`, `Features/Completion.fs`, `Features/Definition.fs`, `Server.fs`, `Rfs.Server.fsproj`
- **Tests (`C:\Users\shaya\RiderProjects\ranvier-fsharp-parity\tests`):**
  - Modify / Create tests in: `tests/Rfs.Decl.Tests`, `tests/Rfs.Checker.Tests`, `tests/Rfs.Server.Tests`, `tests/Rfs.Lsp.Tests`

---

### Task 1: FCS Fork `SemanticClassification` Parity & `Rfs.Checker` Plain-Data Query Primitives

**Files:**
- Modify (Fork): `C:\Users\shaya\RiderProjects\fickle-fsharp-i1\fcs\service_decl.fs`
- Modify (R): `src/Rfs.Checker/FileUses.fs`
- Modify (R): `src/Rfs.Checker/FileLevel.fs`
- Modify (R): `src/Rfs.Checker/ProjectGraph.fs`
- Modify (R): `tests/Rfs.Decl.Tests/DeclTests.fs` (or new test file in `tests/Rfs.Decl.Tests/`)
- Modify (R): `tests/Rfs.Checker.Tests/` (add semantic-classification, `.fsi` read, latest-parse, and `FileUses` relation tests)

**Interfaces:**
```fsharp
// In FSharp.Compiler.Declarations (fcs/service_decl.fs):
type FileSignature with
    /// Plain-data semantic classifications captured during CheckSignatureFile; empty on implementation signatures.
    member SemanticClassification: SemanticClassificationItem[]

type FileResult with
    /// Semantic classifications for the checked file (filtered to `range` when given).
    /// On FinishFile results, merges sink resolutions + format specifiers under start.Sync without forcing CheckResults,
    /// conformFile, or compiledNamesOf. On CheckFile results, uses CheckResultsNotKept when CheckResults is not yet built.
    member GetSemanticClassification: ?range: range -> SemanticClassificationItem[]

// In Rfs.Checker (FileUses.fs):
[<Struct>]
type CallableDecl =
    { Key: string
      Name: string
      Glyph: FSharpGlyph
      Range: range
      SelectionRange: range }

[<Struct>]
type TypeRelationEntry =
    { Key: string
      Name: string
      Glyph: FSharpGlyph
      Range: range
      SelectionRange: range
      SuperKeys: string[] }

type FileUses with
    /// Concrete member/type implementation ranges in this file for an interface entity key or abstract slot key.
    member ImplementationsOf: targetKey: string -> range[]
    /// Declared types in this file whose immediate base class or interfaces include `superKey`.
    member SubtypesOf: superKey: string -> TypeRelationEntry[]
    /// Declared type entry in this file by its SymbolKey, including its immediate SuperKeys.
    member TypeOf: key: string -> TypeRelationEntry voption
    /// Innermost callable declaration (function/member/binding) whose range encloses `pos`.
    member EnclosingCallable: pos: pos -> CallableDecl voption

// In Rfs.Checker (ProjectGraph.fs):
type ProjectGraph with
    /// Latest text and parse for `.fs` OR `.fsi`, never waiting on typecheck.
    member LatestParse: file: string * ct: CancellationToken -> Task<TextView>
    /// Settled `.fsi` signature file read: text/parse view, FileSignature, and FileUses in one consistent state.
    member ReadSignature: file: string * ct: CancellationToken -> Task<struct (TextView * FileSignature * FileUses)>
    /// Latest parsed TextViews across workspace graphs on the same GraphHost in one turn, never waiting on typecheck.
    member LatestParsesInOneTurn: scope: (unit -> struct (bool * (ProjectGraph * string list)[])) * ct: CancellationToken -> Task<(ProjectGraph * (string * TextView)[])[] voption>
```

**Review Focus:**
- Verify **every** `TcResultsSinkImpl` in `fcs/service_decl.fs` receives the right `ISourceText`:
  - `ParsedDeclaration` stores `source: ISourceText` (`member internal _.Source = source`) populated in `DeclarationChecker.Split`.
  - `CheckDeclarationLocked` passes `?sourceText = Some decl.Source` (NEVER `start.Parse`, which is stale after edits!).
  - `buildCheckResults`, `CheckSignatureFile`, `BeginFile`, and `FinishFile` pass `?sourceText = DeclarationCheck.sourceOf parse`.
- Verify `FileResult.GetSemanticClassification` on `FinishFile` acquires `lock sync` (matching `FileStart.Sync` lock order; never acquiring `FileStart.Sync` inside `FileLock`) and includes `start.Sink`, `results`, `exits`, AND `FinishFile`'s tail `sink` (so postponed SRTP/trait member resolutions in `CheckImplFileTail` are included!).
- Verify `FileResult.GetSemanticClassification` on `CheckFile` does NOT force `checkResults.Value` (`CheckResultsBuilt` increments once per call via `CheckResultsNotKept`, and `FSharpCheckFileResults` is NOT retained).
- Verify `FileUses` stores ONLY plain data (`string`, `range`, `FSharpGlyph`, structs/arrays) — zero `FSharpSymbol` or `FSharpEntity` references!

- [ ] **Step 1: Create branch `lsp-parity` in `fickle-fsharp-i1` and write failing `Rfs.Decl.Tests` & `Rfs.Checker.Tests` (RED)**
  - In `C:\Users\shaya\RiderProjects\fickle-fsharp-i1`, create branch `lsp-parity` from `HEAD` (`c39df403f3`).
  - Add failing oracle tests in `Rfs.Decl.Tests` asserting `GetSemanticClassification` parity against `FSharpChecker` for:
    1. `sprintf "%d %s %0.2f %%" 1 "a" 2.5` and interpolated `$"val = %04d{x} and {y}"` in whole check, initial declaration check, edited declaration check (changing line/column offsets after `BeginFile`!), and shifted declaration (`DeclarationChecker.Shift`).
    2. Postponed SRTP / trait member call resolved in `FinishFile` tail.
    3. `.fsi` signature file `FileSignature.SemanticClassification` parity against `FSharpChecker.ParseAndCheckFileInProject`.
    4. Unpromoted `CheckFile` `GetSemanticClassification` does not force `checkResults.IsValueCreated` (retains nothing).
  - Add failing tests in `Rfs.Checker.Tests` for `ProjectGraph.LatestParse` (both `.fs` and `.fsi`, completing while typecheck `FlightHook` is blocked!), `ProjectGraph.ReadSignature`, `ProjectGraph.LatestParsesInOneTurn`, and `FileUses` (`ImplementationsOf`, `SubtypesOf`, `TypeOf`, `EnclosingCallable`).
- [ ] **Step 2: Implement fork changes in `fcs/service_decl.fs` and build `fickle-fsharp-i1`**
  - Implement `DeclarationCheck.sourceOf`, `ParsedDeclaration.Source`, `?sourceText` on all `TcResultsSinkImpl` sites, `FileSignature.SemanticClassification`, and `FileResult.GetSemanticClassification`.
  - Build fork DLL via `rtk pwsh -NoProfile -File build/fork.ps1 -ForkDir C:\Users\shaya\RiderProjects\fickle-fsharp-i1`.
- [ ] **Step 3: Implement `Rfs.Checker` primitives (`FileUses.fs`, `FileLevel.fs`, `ProjectGraph.fs`) and verify GREEN**
  - Extend `SymbolUses.ofCheck` / `ofSignature` (passing `ParsedInput` into `ofCheck`/`ofSignature` from `FileLevel.fs`) to populate plain-data `ImplementationsOf`, `SubtypesOf`, `TypeOf`, and `EnclosingCallable`.
  - Add `LatestParse`, `ReadSignature`, and `LatestParsesInOneTurn` to `ProjectGraph`.
  - Run `Rfs.Decl.Tests` and `Rfs.Checker.Tests` in `Release` against `fickle-fsharp-i1` until 100% green.
- [ ] **Step 4: Mandatory Mutation Testing & Commit**
  - Mutate at least 8 critical sites across `fcs/service_decl.fs`, `FileUses.fs`, and `ProjectGraph.fs` (e.g., omit `decl.Source` fallback to `start.Parse`, omit `FinishFile` tail `sink`, flip range containment in `EnclosingCallable`, swap `ImplementationsOf` interface/slot key, remove `.fsi` branch in `LatestParse`, etc.), verify tests kill 100% of mutants (`0` survivors; add tests for any survivor), restore clean code, re-verify green, and commit on both `fickle-fsharp-i1` (`lsp-parity`) and `ranvier-fsharp-parity` (`lsp-parity`).

---

### Task 2: Runtime `FSharpConfig` & Full LSP Semantic Highlighting (`full`, `full/delta`, `range`)

**Files:**
- Create: `src/Rfs.Server/Config.fs`
- Create: `src/Rfs.Server/Features/SemanticTokens.fs`
- Modify: `src/Rfs.Server/Client.fs` (add `WorkspaceConfiguration` request helper if not exposed)
- Modify: `src/Rfs.Server/Server.fs`
- Modify: `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/` & `tests/Rfs.Lsp.Tests/`

**Interfaces:**
```fsharp
// Config.fs
namespace Rfs.Server

type InlayHintsConfig =
    { TypeAnnotations: bool
      ParameterNames: bool
      DisableLongTooltip: bool }

type CodeLensConfig =
    { SignatureEnabled: bool
      ReferencesEnabled: bool }

type LineLensConfig =
    { Enabled: string
      Prefix: string }

type InlineValuesConfig =
    { Enabled: bool option
      Prefix: string }

type FSharpConfig =
    { AutomaticWorkspaceInit: bool
      WorkspaceModePeekDeepLevel: int
      ExcludeProjectDirectories: string list
      KeywordsAutocomplete: bool
      ResolveNamespaces: bool
      IndentationSize: int
      UnionCaseStubGeneration: bool
      UnionCaseStubGenerationBody: string
      RecordStubGeneration: bool
      RecordStubGenerationBody: string
      InterfaceStubGeneration: bool
      InterfaceStubGenerationObjectIdentifier: string
      InterfaceStubGenerationMethodBody: string
      AbstractClassStubGeneration: bool
      AbstractClassStubGenerationObjectIdentifier: string
      AbstractClassStubGenerationMethodBody: string
      UnusedOpensAnalyzer: bool
      UnusedDeclarationsAnalyzer: bool
      SimplifyNameAnalyzer: bool
      UnnecessaryParenthesesAnalyzer: bool
      LineLens: LineLensConfig
      CodeLenses: CodeLensConfig
      InlayHints: InlayHintsConfig
      InlineValues: InlineValuesConfig }
    static member Default: FSharpConfig
    static member UpdateFromJson: current: FSharpConfig -> token: Newtonsoft.Json.Linq.JToken -> FSharpConfig

// Features/SemanticTokens.fs
module Rfs.Server.SemanticTokens

val legend: Ionide.LanguageServerProtocol.Types.SemanticTokensLegend
val full: cache: TokenDeltaCache -> graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<Ionide.LanguageServerProtocol.Types.SemanticTokens option>
val fullDelta: cache: TokenDeltaCache -> graph: ProjectGraph -> path: string -> previousResultId: string -> ct: CancellationToken -> Task<Ionide.LanguageServerProtocol.Types.U2<Ionide.LanguageServerProtocol.Types.SemanticTokens, Ionide.LanguageServerProtocol.Types.SemanticTokensDelta> option>
val range: graph: ProjectGraph -> path: string -> lspRange: Ionide.LanguageServerProtocol.Types.Range -> ct: CancellationToken -> Task<Ionide.LanguageServerProtocol.Types.SemanticTokens option>
```

**Review Focus:**
- Exact FSAC legend indices (`SemanticTokenTypes` 0..25, `SemanticTokenModifier` bitflags 0..12) and `ClassificationUtils.map` rules so Ionide / VS Code themes highlight identically to FSAC.
- Scrubbing overlapping/zero-width ranges, overriding `SemanticClassificationType.LocalValue` to `SemanticTokenTypes.Parameter` at AST parameter binding sites (`ParsedInput`), emitting `Keyword` tokens for `SynType.WithNull` (`| null`), and splitting multiline tokens across lines using actual line lengths from `ISourceText`/`Text`.
- Relative LSP 5-integer-per-token encoding (`deltaLine`, `deltaStart`, `length`, `tokenType`, `tokenModifiers`) and exact `SemanticTokensEdit` computation (`Start`, `DeleteCount`, `Data`) in `TokenDeltaCache` (bounded per open file, cleared on `didClose`).
- `.fsi` files MUST be routed in `Server.fs` (`readingAny` / `SemanticTokens` dispatches `.fsi` to `ProjectGraph.ReadSignature` and `.fs` to `ProjectGraph.ReadFile`, caching plain `SemanticClassificationItem[]` in a `ConditionalWeakTable<obj, SemanticClassificationItem[]>` so repeated `range` + `full` calls on the same `FileResult` do zero redundant work!).
- Wire `"textDocument/semanticTokens/full"`, `"textDocument/semanticTokens/full/delta"`, `"textDocument/semanticTokens/range"`, and `"workspace/didChangeConfiguration"` in `Server.implemented` and `SemanticTokensProvider` in `Initialize`.

- [ ] **Step 1: Write failing unit & LSP protocol tests for `FSharpConfig` and `SemanticTokens` (`full`, `full/delta`, `range`, `.fs` + `.fsi`, multiline, printf, parameters, `| null`) (RED)**
- [ ] **Step 2: Implement `Config.fs`, `Features/SemanticTokens.fs`, and `Server.fs` `.fsi`-capable `reading` + capability/method wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate relative-encoding `deltaLine`/`deltaStart` reset on new line, modifier bitflag shifts, `TokenDeltaCache` prefix/suffix trim arithmetic, multiline splitting bounds, parameter override check, and `.fsi` routing; verify 100% mutant kill (`0` survivors), restore, re-test green, and commit.

---

### Task 3: Pure-Syntax Features (`foldingRange`, `selectionRange`, `documentSymbol`) & Completion Parity

**Files:**
- Create: `src/Rfs.Server/Features/Folding.fs`
- Create: `src/Rfs.Server/Features/SelectionRange.fs`
- Create: `src/Rfs.Server/Features/DocumentSymbols.fs`
- Modify: `src/Rfs.Server/Features/Completion.fs`
- Modify: `src/Rfs.Server/Server.fs`, `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/`

**Interfaces:**
```fsharp
module Rfs.Server.Folding
val ranges: graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<FoldingRange[] option>

module Rfs.Server.SelectionRange
val atPositions: graph: ProjectGraph -> path: string -> positions: Position[] -> ct: CancellationToken -> Task<SelectionRange[] option>

module Rfs.Server.DocumentSymbols
val hierarchical: graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<U2<SymbolInformation[], DocumentSymbol[]> option>
```

**Review Focus:**
- All three features MUST call `ProjectGraph.LatestParse(path, ct)` for **both `.fs` and `.fsi`** — NEVER `ReadFile`! Verify via a test that blocks typecheck (`FlightHook`) after an edit and asserts `foldingRange`, `selectionRange`, and `documentSymbol` return the **new** text's results immediately without waiting for typechecking!
- `DocumentSymbols`: ensure `SelectionRange` is always strictly contained within `Range` (LSP 3.17 requirement — VS Code drops the entire outline if `SelectionRange` falls outside `Range`!).
- `SelectionRange`: walk `ParsedInput` (`ImplFile` and `SigFile`) to collect nested syntax ranges containing each cursor position, deduplicate identical ranges, sort innermost-to-outermost, and link `Parent`.
- `Completion`: add `"{"` trigger character in `Initialize`, and append F# language keywords when `config.KeywordsAutocomplete` is true (outside dot-member access `x.`).

- [ ] **Step 1: Write failing tests (including blocked-typecheck non-blocking proof on `.fs` and `.fsi`, `SelectionRange` containment invariant, folding kinds, keyword completion) (RED)**
- [ ] **Step 2: Implement `Folding.fs`, `SelectionRange.fs`, `DocumentSymbols.fs`, `Completion.fs` updates, and `Server.fs` wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate `SelectionRange` containment clamping, `FoldingRangeKind` mapping, 1-based FCS to 0-based LSP line conversions, `SelectionRange` parent chain ordering, and dot-access guard in keyword completion; verify 100% mutant kill, restore, re-test green, and commit.

---

### Task 4: Signature Help (`signatureHelp`) & Extended Navigation (`documentHighlight`, `declaration`, `typeDefinition`, `implementation`, `.fsi` <-> `.fs` `definition`)

**Files:**
- Create: `src/Rfs.Server/Features/SignatureHelp.fs`
- Create: `src/Rfs.Server/Features/DocumentHighlight.fs`
- Create: `src/Rfs.Server/Features/Declaration.fs`
- Create: `src/Rfs.Server/Features/TypeDefinition.fs`
- Create: `src/Rfs.Server/Features/Implementation.fs`
- Modify: `src/Rfs.Server/Features/Definition.fs`
- Modify: `src/Rfs.Server/Server.fs`, `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/`

**Interfaces:**
```fsharp
module Rfs.Server.SignatureHelp
val at: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<Ionide.LanguageServerProtocol.Types.SignatureHelp option>

module Rfs.Server.DocumentHighlight
val at: graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<DocumentHighlight[] option>

module Rfs.Server.Declaration
val at: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<Range option> // FCS range with FileName

module Rfs.Server.TypeDefinition
val at: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<Range option>

module Rfs.Server.Implementation
val find: scope: References.Scope -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<Result<Range[], unit>>
```

**Review Focus:**
- `SignatureHelp`: support both method-call parameter lists (`FindParameterLocations` + `GetMethods` on `CheckResultsAt`) and curried F# function calls (`let add x y = x + y` -> `add 1 |`), computing accurate `ActiveParameter` (from comma count / space-separated argument index) and `ActiveSignature`, plus live `///` XML docs via `DocsInTurn` / `FileDocs`.
- `.fsi` <-> `.fs` navigation parity:
  - `Definition.at` on a `.fsi` declaration jumps to the paired `.fs` implementation (`inImplementation`), and on a `.fs` use jumps to `.fsi` (existing behavior) — and when invoked *already* on the `.fsi` declaration, toggles to `.fs`!
  - `Declaration.at` jumps to the `.fsi` signature declaration when one exists, else primary definition.
- `Implementation.find`: runs `ReadInOneTurn` across `scope` (referencing projects + own project) collecting `uses.ImplementationsOf(targetKey)` for interfaces and abstract/virtual members, plus `.fsi` -> `.fs` definition jumps!
- `DocumentHighlight`: works on both `.fs` and `.fsi`, stripping module qualifiers from ranges so only the identifier token is highlighted, marking definitions as `DocumentHighlightKind.Write` and references as `DocumentHighlightKind.Read`.

- [ ] **Step 1: Write failing tests for `SignatureHelp` (overloaded methods, curried functions, active parameter index), `DocumentHighlight` (`.fs` + `.fsi`), `Declaration` & `.fsi` <-> `.fs` toggle, `TypeDefinition`, and cross-project `Implementation` (RED)**
- [ ] **Step 2: Implement all 5 modules + `Definition.fs` `.fsi` support + `Server.fs` capabilities & method wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate `ActiveParameter` comma/arg counting, qualifier-stripping column calculation in `DocumentHighlight`, `.fsi` <-> `.fs` toggle branch, `TypeDefinition` abbreviated-type unwrapping, and cross-project `Implementation` scope; verify 100% mutant kill, restore, re-test green, and commit.

---

### Task 5: Workspace Symbols, Call Hierarchy & Type Hierarchy

**Files:**
- Create: `src/Rfs.Server/Features/WorkspaceSymbols.fs`
- Create: `src/Rfs.Server/Features/CallHierarchy.fs`
- Create: `src/Rfs.Server/Features/TypeHierarchy.fs`
- Modify: `src/Rfs.Server/Server.fs`, `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/`

**Interfaces:**
```fsharp
module Rfs.Server.WorkspaceSymbols
val search: projectsNow: (unit -> ProjectGraph list) -> loadingNow: (unit -> bool) -> filesNow: (ProjectGraph -> string list) -> query: string -> ct: CancellationToken -> Task<U2<SymbolInformation[], WorkspaceSymbol[]> option>
val resolve: sym: WorkspaceSymbol -> WorkspaceSymbol

module Rfs.Server.CallHierarchy
val prepare: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<CallHierarchyItem[] option>
val incoming: scope: References.Scope -> hostGraph: ProjectGraph -> item: CallHierarchyItem -> ct: CancellationToken -> Task<CallHierarchyIncomingCall[] option>
val outgoing: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> item: CallHierarchyItem -> ct: CancellationToken -> Task<CallHierarchyOutgoingCall[] option>

module Rfs.Server.TypeHierarchy
val prepare: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<TypeHierarchyItem[] option>
val supertypes: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> item: TypeHierarchyItem -> ct: CancellationToken -> Task<TypeHierarchyItem[] option>
val subtypes: scope: References.Scope -> hostGraph: ProjectGraph -> item: TypeHierarchyItem -> ct: CancellationToken -> Task<TypeHierarchyItem[] option>
```

**Review Focus:**
- **Single-Turn Consistency:**
  - `WorkspaceSymbols.search` reads all loaded graphs on `GraphHost` in one turn via `LatestParsesInOneTurn` (never waiting for typecheck, deduplicating shared files across multi-project graphs, matching case-insensitive substring + camelCase initials).
  - `CallHierarchy.incoming` reads across `scope` in **one single turn** (`ReadInOneTurn`), finding non-definition hits of `targetKey` in each file's `FileUses` and grouping them by `uses.EnclosingCallable(hitRange.Start)` from that **exact same `FileUses`**!
  - `TypeHierarchy.subtypes` reads across `scope` in **one single turn** (`ReadInOneTurn`) via `uses.SubtypesOf(targetKey)`.
- **Plain-Data `Data` Payload:** `CallHierarchyItem.Data` and `TypeHierarchyItem.Data` serialize `{ Key: string; Path: string }` as JSON (`JToken`), never holding live compiler state.
- Wire `WorkspaceSymbolProvider`, `CallHierarchyProvider`, and `TypeHierarchyProvider` (in `Experimental` or standard `ServerCapabilities` field in `Ionide.LanguageServerProtocol` `0.7.0` — verify exact `ServerCapabilities` property name!) and add all 8 methods to `Server.implemented`.

- [ ] **Step 1: Write failing tests for `WorkspaceSymbols` (substring + camelCase + non-blocking), cross-project `CallHierarchy` (`prepare`, `incoming`, `outgoing`), and cross-project `TypeHierarchy` (`prepare`, `supertypes`, `subtypes`) (RED)**
- [ ] **Step 2: Implement `WorkspaceSymbols.fs`, `CallHierarchy.fs`, `TypeHierarchy.fs`, and `Server.fs` wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate camelCase matcher, `incomingCalls` definition-hit exclusion (`not isDefinition`), `FromRanges` grouping per caller, `outgoingCalls` body-range filter, and `supertypes`/`subtypes` key lookup; verify 100% mutant kill, restore, re-test green, and commit.

---

### Task 6: Safe Cross-Project Rename, Inlay Hints, CodeLens, Inline Value & Ionide Custom Endpoints (`fsharp/*`)

**Files:**
- Create: `src/Rfs.Server/Features/Rename.fs`
- Create: `src/Rfs.Server/Features/InlayHints.fs`
- Create: `src/Rfs.Server/Features/CodeLens.fs`
- Create: `src/Rfs.Server/Features/InlineValue.fs`
- Create: `src/Rfs.Server/Features/IonideEndpoints.fs`
- Modify: `src/Rfs.Server/Client.fs`, `src/Rfs.Server/Workspace.fs`, `src/Rfs.Server/Publisher.fs`, `src/Rfs.Server/Server.fs`, `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/` & `tests/Rfs.Lsp.Tests/`

**Interfaces:**
```fsharp
module Rfs.Server.Rename
val prepare: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<PrepareRenameResult option>
val rename: scope: References.Scope -> graph: ProjectGraph -> path: string -> line: int -> character: int -> newName: string -> ct: CancellationToken -> Task<Result<WorkspaceEdit option, string>>

module Rfs.Server.InlayHints
val provide: config: InlayHintsConfig -> graph: ProjectGraph -> path: string -> range: Range -> ct: CancellationToken -> Task<InlayHint[] option>

module Rfs.Server.CodeLens
val lenses: config: FSharpConfig -> graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<CodeLens[] option>
val resolve: scope: References.Scope -> ownersOf: (string -> ProjectGraph list) -> lens: CodeLens -> ct: CancellationToken -> Task<CodeLens>

module Rfs.Server.InlineValue
val provide: config: InlineValuesConfig -> graph: ProjectGraph -> path: string -> range: Range -> ct: CancellationToken -> Task<InlineValue[] option>

module Rfs.Server.IonideEndpoints
val signature: graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<string option>
val signatureData: graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<string option>
val lineLens: graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<string option>
val pipelineHint: graph: ProjectGraph -> path: string -> ct: CancellationToken -> Task<string option>
val f1Help: graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<string option>
val documentation: ownersOf: (string -> ProjectGraph list) -> graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<string option>
val documentationSymbol: projectsNow: (unit -> ProjectGraph list) -> xmlSig: string -> assembly: string -> ct: CancellationToken -> Task<string option>
val documentationGenerator: graph: ProjectGraph -> path: string -> line: int -> character: int -> ct: CancellationToken -> Task<WorkspaceEdit option>
val compilerLocation: unit -> string
```

**Review Focus:**
- **Rename Safety & Identifier Rules:**
  - Refuse symbols declared outside loaded workspace projects, active pattern cases, constructors (`new`), and invalid `newName`s (empty, containing newlines or ` `` ` inside backticks).
  - Automatically wrap `newName` in ` ``...`` ` when it is an F# keyword or contains spaces/non-identifier chars, and strip redundant ` ``...`` ` if already wrapped.
  - Strip module/type qualifier prefixes (`Module.foo` -> only `foo` range!) on every hit across `.fs` and `.fsi` files, verifying against the settled turn's text (`ReadInOneTurn` + text check) that every replaced slice matches the symbol's identifier (or backticked form), aborting cleanly if any compiler-synthesized/implicit range doesn't match!
- **Exact Ionide Wire Envelopes (`CommandResponse` JSON):**
  - PascalCase `{ "Kind": "...", "Data": ... }` inside `PlainNotification.Content` (`typesig`, `signatureData`, `declarations`, `pipelineHint`, `help`, `formattedDocumentation`, `compilerlocation`).
  - `fsharp/lineLens` takes `ProjectParms` (`{ Project: { Uri: DocumentUri } }` — note `Project.Uri` is the **source file** URI in Ionide!) and runs on `LatestParse` (non-blocking!).
  - Emit `fsharp/fileParsed` and `fsharp/documentAnalyzed` notifications from `Publisher` when files parse/settle, and populate `Output` & `TargetFramework` in `Workspace.fs` `fsharp/notifyWorkspace`.

- [ ] **Step 1: Write failing tests for cross-project + `.fsi` `Rename` (qualifier stripping, keyword auto-backticking, external-symbol rejection), `InlayHints` (type + parameter hints + `TextEdits`), `CodeLens` + `resolve`, `InlineValue`, and all `IonideEndpoints` JSON schemas (RED)**
- [ ] **Step 2: Implement `Rename.fs`, `InlayHints.fs`, `CodeLens.fs`, `InlineValue.fs`, `IonideEndpoints.fs`, `Publisher`/`Workspace`/`Client` notifications, and `Server.fs` wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate `Rename` qualifier-stripping offset, keyword backtick wrapper, external-symbol refusal check, `InlayHint` range filter & padding flags, `CodeLens` `replaceCodeLens` suppression, and `IonideEndpoints` `Kind` discriminators; verify 100% mutant kill, restore, re-test green, and commit.

---

### Task 7: Built-In Diagnostic Analyzers, 12 Code-Action Families & Out-of-Process Fantomas Formatting

**Files:**
- Create: `src/Rfs.Server/Features/CodeActions.fs`
- Create: `src/Rfs.Server/Features/Formatting.fs`
- Modify: `src/Rfs.Server/Publisher.fs` (optional built-in analyzer hint diagnostics + `DiagnosticTag.Unnecessary`/`Deprecated` tags)
- Modify: `src/Rfs.Server/Server.fs`, `src/Rfs.Server/Rfs.Server.fsproj`
- Modify: `tests/Rfs.Server.Tests/` & `tests/Rfs.Lsp.Tests/`

**Interfaces:**
```fsharp
module Rfs.Server.CodeActions
val compute:
    config: FSharpConfig ->
    ownersOf: (string -> ProjectGraph list) ->
    graph: ProjectGraph ->
    path: string ->
    p: CodeActionParams ->
    ct: CancellationToken ->
    Task<TextDocumentCodeActionResult option>

module Rfs.Server.Formatting
/// Optional override executable/runner for tests; defaults to probing local/global `dotnet fantomas` / `fantomas`.
val formatDocument: runnerOverride: (string -> string -> bool -> CancellationToken -> Task<string option>) option -> graph: ProjectGraph -> path: string -> options: FormattingOptions -> ct: CancellationToken -> Task<TextEdit[] option>
val formatRange: runnerOverride: (string -> string -> bool -> CancellationToken -> Task<string option>) option -> graph: ProjectGraph -> path: string -> range: Range -> options: FormattingOptions -> ct: CancellationToken -> Task<TextEdit[] option>
```

**Review Focus:**
- **All 12 Code-Action Families Verified End-to-End:**
  1. Remove unused `open` (`UnusedOpens.getUnusedOpens` / `FSAC0001`)
  2. Prefix `_` / replace with `_` on unused value (`FS1182` / `UnusedDeclarations.getUnusedDeclarations`)
  3. Simplify redundant qualified name (`SimplifyNames.getSimplifiableNames` / `FSAC0002`)
  4. `FS0039` unresolved identifier: compiler `"Maybe you want one of the following:"` spellcheck fixes + `ResolveNamespaces` (`open <Ns>` and `<Ns>.<Ident>` from project + referenced assembly entities)
  5. Add `rec` to binding/module (`FS0039` self-call inside `let` or `FS0576` `and` without `rec`)
  6. Add `new` keyword to `IDisposable` constructor (`FS0760`)
  7. Operator/syntax quick-fixes (`FS0020` mutable `x <- y` + `|> ignore`, `FS3360` missing `=` in type definition, `==` -> `=`, `!=` -> `<>`)
  8. Add explicit type annotation to `let` binding / parameter at cursor
  9. Implement interface / object-expression stub (`FS0366` or cursor on `interface ...` via `InterfaceStubGenerator`)
  10. Generate missing union cases in `match` (`FS0025`)
  11. Generate missing record fields (`FS0764`)
  12. Sync paired `.fsi` signature file (`FS0034` update `val` type / `FS0193` or refactoring add missing `val` to `.fsi`).
- **Publisher Diagnostic Tags & Built-in Analyzers:**
  - Tag compiler `FS1182` (unused variable) with `[| DiagnosticTag.Unnecessary |]` and `FS0044` (deprecated) with `[| DiagnosticTag.Deprecated |]`.
  - When `FSharpConfig` enables `UnusedOpensAnalyzer`, `SimplifyNameAnalyzer`, or `UnusedDeclarationsAnalyzer` on an open file, append `FSAC0001`/`FSAC0002`/`FSAC0003` `Hint` diagnostics with `DiagnosticTag.Unnecessary` without keeping live `FSharpCheckFileResults` on unpromoted files.
- **Out-of-Process Fantomas Formatting:**
  - Probe `dotnet tool run fantomas` / `fantomas` with `--stdin` / `--stdout` (`--sig` for `.fsi`), passing cancellation cleanly (`Process.Kill(entireProcessTree = true)` on cancel), returning `Ok None` gracefully when Fantomas is not installed or text is unchanged, and computing minimal line-bounded `TextEdit[]` for `formatDocument` and `formatRange`.

- [ ] **Step 1: Write failing tests for all 12 `CodeActions` families, `Publisher` `DiagnosticTag`s + built-in analyzer diagnostics, and `Formatting` (`formatDocument`, `formatRange`, `.fsi` `--sig`, missing-tool graceful `None`, mock + CLI integration) (RED)**
- [ ] **Step 2: Implement `CodeActions.fs`, `Formatting.fs`, `Publisher.fs` tags/analyzers, and `Server.fs` wiring (GREEN)**
- [ ] **Step 3: Mandatory Mutation Testing & Commit**
  - Mutate diagnostic code matchers (`FS0039`, `FS1182`, `FS0025`, `FS0764`, `FS0760`, `FS0020`), `open` insertion line finder, `rec` keyword insertion offset, `.fsi` signature generation, and `Formatting` range-slice edit calculation; verify 100% mutant kill (`0` survivors), restore, re-test green, and commit.

---

## Plan Self-Review

1. **Spec Coverage Check:**
   - Semantic highlighting (`full`, `full/delta`, `range`, `.fs` + `.fsi`, `Printf` format specifiers, shifted declarations, exact FSAC legend, parameter AST override, `| null`) -> Task 1 + Task 2. ✅
   - `workspace/didChangeConfiguration` + `FSharpConfig` -> Task 2. ✅
   - Pure-syntax non-blocking `foldingRange`, `selectionRange`, `documentSymbol` (`.fs` + `.fsi`) + completion `{` & keywords -> Task 3. ✅
   - `signatureHelp`, `documentHighlight`, `declaration`, `.fsi` <-> `.fs` `definition`, `typeDefinition`, `implementation` -> Task 4. ✅
   - `workspace/symbol` + `resolve`, `CallHierarchy` (`prepare`, `incoming`, `outgoing`), `TypeHierarchy` (`prepare`, `supertypes`, `subtypes`) -> Task 5. ✅
   - `prepareRename` + `rename`, `inlayHint`, `codeLens` + `resolve`, `inlineValue`, all Ionide `fsharp/*` custom endpoints & reactive notifications -> Task 6. ✅
   - Built-in analyzers + `DiagnosticTag`s, all 12 `codeAction` families, out-of-process `formatting` + `rangeFormatting` -> Task 7. ✅
   - Mandatory mutation testing -> Explicit Step in every Task 1..7. ✅
2. **Placeholder Scan:** Zero `TODO`, `TBD`, or vague hand-waves; every task specifies exact file paths, module signatures, invariants, and mutation targets. ✅
3. **Type & Interface Consistency:** `ProjectGraph.LatestParse`, `ProjectGraph.ReadSignature`, `ProjectGraph.LatestParsesInOneTurn`, and `FileUses` plain-data extensions (`ImplementationsOf`, `SubtypesOf`, `TypeOf`, `EnclosingCallable`) are built once in Task 1 and consumed cleanly in Tasks 2–7 without cross-task churn. ✅
