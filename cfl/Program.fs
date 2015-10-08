open libcontextfree
open FileHelpers
open System
open System.IO

/// Gets the program's list of "subprograms",
/// which are really just pairs of strings and 
/// procedures that take a list of strings.
let subprograms : Map<string, string list -> unit> = 
    Map.ofList [
        "head", printTreeProperty ParseTree.showTreeHead
        "yield", printTreeProperty ParseTree.showTreeYield
        "derive-leftmost", printTreeProperty ParseTree.showLeftmostDerivationSequence
        "derive-rightmost", printTreeProperty ParseTree.showRightmostDerivationSequence
        "tree-rules", printTreeProperty ParseTree.showProductionRules
        "tree-grammar", performReadWrite readTreeGrammar writeCfgXmlFile
        "dot", performReadWrite readParseTreeFile writeGraphvizFile
        // Insert additional subprograms here.
    ]

/// Prints the list of registered subprograms to stderr.
let eprintSubprogramList () : unit =
    subprograms |> Seq.iter (fun x -> eprintfn " * %s" x.Key)

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | x :: xs ->
        // If there is at least one command-line argument,
        // we'll run the subprogram belonging to the
        // first argument.
        match Map.tryFind x subprograms with
        | Some func -> 
            // We found a known subprogram.
            func xs
        | None ->
            // `x`, whatever it was, was not a known subprogram.
            eprintfn "Argument '%s' was not recognized as a known subprogram." x
            eprint "List of subprograms:"
            eprintSubprogramList ()
    | _ ->
        // If no subprogram has been specified, all we can realistically do is
        // rage quit.
        eprint "Please specify a subprogram. List of subprograms:"
        eprintSubprogramList ()
    0 // return an integer exit code
