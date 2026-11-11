namespace Rfs.Server

open System
open Newtonsoft.Json.Linq

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

module private JsonConfigHelpers =
    let tryProp (obj: JObject) (name: string) : JToken option =
        obj.Properties()
        |> Seq.tryFind (fun p -> String.Equals(p.Name, name, StringComparison.OrdinalIgnoreCase))
        |> Option.bind (fun p ->
            if isNull p.Value || p.Value.Type = JTokenType.Null then None
            else Some p.Value)

    let tryBool (obj: JObject) (name: string) : bool option =
        match tryProp obj name with
        | Some t when t.Type = JTokenType.Boolean -> Some(t.Value<bool>())
        | _ -> None

    let tryInt (obj: JObject) (name: string) : int option =
        match tryProp obj name with
        | Some t when t.Type = JTokenType.Integer -> Some(t.Value<int>())
        | _ -> None

    let tryString (obj: JObject) (name: string) : string option =
        match tryProp obj name with
        | Some t when t.Type = JTokenType.String -> Some(t.Value<string>())
        | _ -> None

    let tryStringList (obj: JObject) (name: string) : string list option =
        match tryProp obj name with
        | Some(:? JArray as arr) ->
            if arr |> Seq.forall (fun item -> not (isNull item) && item.Type = JTokenType.String) then
                Some [ for item in arr -> item.Value<string>() ]
            else
                None
        | _ -> None

    let tryObj (obj: JObject) (name: string) : JObject option =
        match tryProp obj name with
        | Some(:? JObject as sub) -> Some sub
        | _ -> None

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

    static member Default: FSharpConfig =
        { AutomaticWorkspaceInit = false
          WorkspaceModePeekDeepLevel = 2
          ExcludeProjectDirectories = [ ".git"; "node_modules"; ".ionide" ]
          KeywordsAutocomplete = false
          ResolveNamespaces = false
          IndentationSize = 4
          UnionCaseStubGeneration = false
          UnionCaseStubGenerationBody = "failwith \"Not Implemented\""
          RecordStubGeneration = false
          RecordStubGenerationBody = "failwith \"Not Implemented\""
          InterfaceStubGeneration = false
          InterfaceStubGenerationObjectIdentifier = "this"
          InterfaceStubGenerationMethodBody = "failwith \"Not Implemented\""
          AbstractClassStubGeneration = true
          AbstractClassStubGenerationObjectIdentifier = "this"
          AbstractClassStubGenerationMethodBody = "failwith \"Not Implemented\""
          UnusedOpensAnalyzer = false
          UnusedDeclarationsAnalyzer = false
          SimplifyNameAnalyzer = false
          UnnecessaryParenthesesAnalyzer = true
          LineLens = { Enabled = "never"; Prefix = "" }
          CodeLenses =
            { SignatureEnabled = true
              ReferencesEnabled = true }
          InlayHints =
            { TypeAnnotations = true
              ParameterNames = true
              DisableLongTooltip = true }
          InlineValues = { Enabled = None; Prefix = "//" } }

    static member private ApplyObject (current: FSharpConfig) (obj: JObject) : FSharpConfig =
        let openLineLens =
            match JsonConfigHelpers.tryObj obj "LineLens" with
            | None -> current.LineLens
            | Some ll ->
                { Enabled = JsonConfigHelpers.tryString ll "Enabled" |> Option.defaultValue current.LineLens.Enabled
                  Prefix = JsonConfigHelpers.tryString ll "Prefix" |> Option.defaultValue current.LineLens.Prefix }

        let openCodeLenses =
            let baseCodeLenses =
                match JsonConfigHelpers.tryBool obj "EnableReferenceCodeLens" with
                | Some b -> { current.CodeLenses with ReferencesEnabled = b }
                | None -> current.CodeLenses

            match JsonConfigHelpers.tryObj obj "CodeLenses" with
            | None -> baseCodeLenses
            | Some cl ->
                let sigEnabled =
                    JsonConfigHelpers.tryObj cl "Signature"
                    |> Option.bind (fun s -> JsonConfigHelpers.tryBool s "Enabled")
                    |> Option.orElseWith (fun () -> JsonConfigHelpers.tryBool cl "SignatureEnabled")
                    |> Option.defaultValue baseCodeLenses.SignatureEnabled

                let refEnabled =
                    JsonConfigHelpers.tryObj cl "References"
                    |> Option.bind (fun r -> JsonConfigHelpers.tryBool r "Enabled")
                    |> Option.orElseWith (fun () -> JsonConfigHelpers.tryBool cl "ReferencesEnabled")
                    |> Option.defaultValue baseCodeLenses.ReferencesEnabled

                { SignatureEnabled = sigEnabled
                  ReferencesEnabled = refEnabled }

        let openInlayHints =
            match JsonConfigHelpers.tryObj obj "InlayHints" with
            | None -> current.InlayHints
            | Some ih ->
                { TypeAnnotations =
                    JsonConfigHelpers.tryBool ih "TypeAnnotations"
                    |> Option.defaultValue current.InlayHints.TypeAnnotations
                  ParameterNames =
                    JsonConfigHelpers.tryBool ih "ParameterNames"
                    |> Option.defaultValue current.InlayHints.ParameterNames
                  DisableLongTooltip =
                    JsonConfigHelpers.tryBool ih "DisableLongTooltip"
                    |> Option.defaultValue current.InlayHints.DisableLongTooltip }

        let openInlineValues =
            let ivObj =
                JsonConfigHelpers.tryObj obj "PipelineHints"
                |> Option.orElseWith (fun () -> JsonConfigHelpers.tryObj obj "InlineValues")

            match ivObj with
            | None -> current.InlineValues
            | Some iv ->
                { Enabled =
                    match JsonConfigHelpers.tryBool iv "Enabled" with
                    | Some b -> Some b
                    | None -> current.InlineValues.Enabled
                  Prefix =
                    JsonConfigHelpers.tryString iv "Prefix"
                    |> Option.defaultValue current.InlineValues.Prefix }

        { AutomaticWorkspaceInit =
            JsonConfigHelpers.tryBool obj "AutomaticWorkspaceInit"
            |> Option.defaultValue current.AutomaticWorkspaceInit
          WorkspaceModePeekDeepLevel =
            JsonConfigHelpers.tryInt obj "WorkspaceModePeekDeepLevel"
            |> Option.defaultValue current.WorkspaceModePeekDeepLevel
          ExcludeProjectDirectories =
            JsonConfigHelpers.tryStringList obj "ExcludeProjectDirectories"
            |> Option.defaultValue current.ExcludeProjectDirectories
          KeywordsAutocomplete =
            JsonConfigHelpers.tryBool obj "KeywordsAutocomplete"
            |> Option.defaultValue current.KeywordsAutocomplete
          ResolveNamespaces =
            JsonConfigHelpers.tryBool obj "ResolveNamespaces"
            |> Option.defaultValue current.ResolveNamespaces
          IndentationSize =
            JsonConfigHelpers.tryInt obj "IndentationSize"
            |> Option.defaultValue current.IndentationSize
          UnionCaseStubGeneration =
            JsonConfigHelpers.tryBool obj "UnionCaseStubGeneration"
            |> Option.defaultValue current.UnionCaseStubGeneration
          UnionCaseStubGenerationBody =
            JsonConfigHelpers.tryString obj "UnionCaseStubGenerationBody"
            |> Option.defaultValue current.UnionCaseStubGenerationBody
          RecordStubGeneration =
            JsonConfigHelpers.tryBool obj "RecordStubGeneration"
            |> Option.defaultValue current.RecordStubGeneration
          RecordStubGenerationBody =
            JsonConfigHelpers.tryString obj "RecordStubGenerationBody"
            |> Option.defaultValue current.RecordStubGenerationBody
          InterfaceStubGeneration =
            JsonConfigHelpers.tryBool obj "InterfaceStubGeneration"
            |> Option.defaultValue current.InterfaceStubGeneration
          InterfaceStubGenerationObjectIdentifier =
            JsonConfigHelpers.tryString obj "InterfaceStubGenerationObjectIdentifier"
            |> Option.defaultValue current.InterfaceStubGenerationObjectIdentifier
          InterfaceStubGenerationMethodBody =
            JsonConfigHelpers.tryString obj "InterfaceStubGenerationMethodBody"
            |> Option.defaultValue current.InterfaceStubGenerationMethodBody
          AbstractClassStubGeneration =
            JsonConfigHelpers.tryBool obj "AbstractClassStubGeneration"
            |> Option.defaultValue current.AbstractClassStubGeneration
          AbstractClassStubGenerationObjectIdentifier =
            JsonConfigHelpers.tryString obj "AbstractClassStubGenerationObjectIdentifier"
            |> Option.defaultValue current.AbstractClassStubGenerationObjectIdentifier
          AbstractClassStubGenerationMethodBody =
            JsonConfigHelpers.tryString obj "AbstractClassStubGenerationMethodBody"
            |> Option.defaultValue current.AbstractClassStubGenerationMethodBody
          UnusedOpensAnalyzer =
            JsonConfigHelpers.tryBool obj "UnusedOpensAnalyzer"
            |> Option.defaultValue current.UnusedOpensAnalyzer
          UnusedDeclarationsAnalyzer =
            JsonConfigHelpers.tryBool obj "UnusedDeclarationsAnalyzer"
            |> Option.defaultValue current.UnusedDeclarationsAnalyzer
          SimplifyNameAnalyzer =
            JsonConfigHelpers.tryBool obj "SimplifyNameAnalyzer"
            |> Option.defaultValue current.SimplifyNameAnalyzer
          UnnecessaryParenthesesAnalyzer =
            JsonConfigHelpers.tryBool obj "UnnecessaryParenthesesAnalyzer"
            |> Option.defaultValue current.UnnecessaryParenthesesAnalyzer
          LineLens = openLineLens
          CodeLenses = openCodeLenses
          InlayHints = openInlayHints
          InlineValues = openInlineValues }

    static member UpdateFromJson (current: FSharpConfig) (token: JToken) : FSharpConfig =
        try
            if isNull token || token.Type = JTokenType.Null then
                current
            else
                match token with
                | :? JArray as arr ->
                    arr |> Seq.fold FSharpConfig.UpdateFromJson current
                | :? JObject as root ->
                    let afterRoot = FSharpConfig.ApplyObject current root
                    match JsonConfigHelpers.tryObj root "FSharp" with
                    | Some nested -> FSharpConfig.ApplyObject afterRoot nested
                    | None -> afterRoot
                | _ -> current
        with _ ->
            current
