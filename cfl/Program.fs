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
          Doc = "Read a parse tree and print its yield.";
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
          Action = performReadWrite readParseTreeFile writeParseTreeGraphvizFile }

        { Name = "earley";
          Doc = "Parses an input string from standard input according to the given grammar. \
                 All possible parse trees are written to the specified output location.";
          Action = performEarleyParse };

        { Name = "cfg-nonterminals";
          Doc = "Read a context-free grammar and print its nonterminals.";
          Action = printCfgProperty ContextFreeGrammar.showNonterminals }

        { Name = "cfg-terminals";
          Doc = "Read a context-free grammar and print its terminals.";
          Action = printCfgProperty ContextFreeGrammar.showTerminals }

        { Name = "cfg-rules";
          Doc = "Read a context-free grammar and print its rules.";
          Action = printCfgProperty ContextFreeGrammar.showRules }

        { Name = "cfg-start-symbol";
          Doc = "Read a context-free grammar and print its start symbol.";
          Action = printCfgProperty ContextFreeGrammar.showStartSymbol }

        { Name = "pda-dot";
          Doc = "Outputs a dot file that represents an if-else push-down automaton.";
          // TODO: PLEASE, IMPLEMENT ME
          Action = performReadWrite (fun _ -> Success PushdownAutomaton.ifElseAutomaton) writePdaGraphvizFile }

        // Insert additional subprograms here.
    ]

/// Prints the list of registered subprograms to stderr.
let eprintSubprogramList () : unit =
    for sp in subprograms do
        let lefts = seq {
            let first = sprintf " * %s - " sp.Name
            yield first
            while true do
                yield String.replicate first.Length " "
        }
        let rights = IOHelpers.wordWrap 60 sp.Doc
        for l, r in Seq.zip lefts rights do
            eprintfn "%s%s" l r

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | firstArg :: restArgs ->
        // Look for a subprogram with the given name.
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
