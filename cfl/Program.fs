open libcontextfree
open FileHelpers
open System
open System.IO

type Subprogram = { Name : string;
                    Doc : string;
                    Action : string list -> unit }

let subprograms : Subprogram list =
    [
        { Name = "tree-head";
          Doc = "Read a parse tree and print its head.";
          Action = printTreeProperty ParseTree.showTreeHead };

        { Name = "tree-yield";
          Doc = "Read a parse tree and print its head.";
          Action = printTreeProperty ParseTree.showTreeYield };

        { Name = "tree-derive-leftmost";
          Doc = "Read a parse tree and print its leftmost derivation sequence.";
          Action = printTreeProperty ParseTree.showLeftmostDerivationSequence };

        { Name = "tree-derive-rightmost";
          Doc = "Read a parse tree and print its rightmost derivation sequence.";
          Action = printTreeProperty ParseTree.showRightmostDerivationSequence };

        { Name = "tree-rules";
          Doc = "Read a parse tree and print its production rules.";
          Action = printTreeProperty ParseTree.showProductionRules };

        { Name = "tree-grammar";
          Doc = "Read a parse tree and print its inferred grammar.";
          Action = performReadWrite readTreeGrammar writeCfgXmlFile };

        { Name = "tree-dot";
          Doc = "Read a parse tree and visualize it as a Graphviz .dot file.";
          Action = performReadWrite readParseTreeFile writeGraphvizFile }

        // Insert additional subprograms here.
    ]

/// Prints the list of registered subprograms to stderr.
let eprintSubprogramList () : unit =
    for sp in subprograms do
        eprintfn " * %s - %s" sp.Name sp.Doc

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | firstArg :: restArgs ->
        // If there is at least one command-line argument,
        // we'll run the subprogram belonging to the
        // first argument.
        match List.tryFind (fun p -> p.Name = firstArg) subprograms with
        | Some subprogram ->
            // We found a known subprogram. Call it with the rest of the arguments.
            subprogram.Action restArgs
        | None ->
            // `firstArg`, whatever it was, was not a known subprogram.
            eprintfn "Argument '%s' was not recognized as a known subprogram." firstArg
            eprint "List of subprograms:"
            eprintSubprogramList ()
    | _ ->
        // If no subprogram has been specified, all we can realistically do is
        // rage quit.
        eprint "Please specify a subprogram. List of subprograms:"
        eprintSubprogramList ()
    0 // return an integer exit code
