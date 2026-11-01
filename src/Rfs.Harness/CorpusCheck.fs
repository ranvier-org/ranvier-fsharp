namespace Rfs.Harness

open System.Threading.Tasks

module CorpusCheck =
    /// Checks one corpus project unedited with the oracle and prints what it found. Any error means the corpus is not
    /// set up (a missing reference, say), and every measurement against it would be meaningless; false then.
    let verify (oracle: OracleClient) (name: string) (project: string) : Task<bool> =
        task {
            try
                let! diagnostics = oracle.Check project Map.empty
                let errors = diagnostics |> List.filter (fun d -> d.Severity = 1)
                printfn "%s: %d errors, %d other diagnostics" name errors.Length (diagnostics.Length - errors.Length)

                for d in errors |> List.truncate 20 do
                    printfn "  %s:%d:%d %s %s" d.File (d.Range.Start.Line + 1) (d.Range.Start.Character + 1) d.Code d.Message

                return errors.IsEmpty
            with e ->
                eprintfn "%s: %s" name e.Message
                return false
        }
