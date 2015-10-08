open libcontextfree
open System
open System.IO

/// Print a string, followed by a newline, to stdout.
let print = printfn "%s"

/// Print a string, followed by a newline, to stderr.
let eprint = eprintfn "%s"

/// Tries to read a file that contains a parse tree. If something goes wrong,
/// log an error.
let readParseTreeFile (fileName : string) : Result<ParseTree<string, string>> =
    try
        use fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)
        use reader = new StreamReader(fs)
        TreeHandler.readNode reader
    with
    | _ -> Error (sprintf "Couldn't open file for reading: '%s'." fileName)

/// Perform a file write action that fails with a helpful error message.
let writeFile (fileName : string) (action : FileStream -> unit) : Result<unit> =
    try
        use fs = new FileStream(fileName, FileMode.Create, FileAccess.Write)
        action fs
        Success ()
    with
    | _ ->  Error (sprintf "Couldn't open file for writing: '%s'." fileName)

/// Tries to write the given parse tree to the given output file.
let writeGraphvizFile (fileName : string) (tree : ParseTree<string, string>) : Result<unit> =
    writeFile fileName <| fun fs ->
        use writer = new StreamWriter(fs)
        GraphvizHandler.writeGraphviz writer tree

/// Tries to write the given character CFG to the given output file.
let writeCfgXmlFile (fileName : string) (grammar : ContextFreeGrammar<char, char>) : Result<unit> =
    let xmlNode = XmlHandler.ofCfg grammar
    writeFile fileName (fun fs -> xmlNode.XElement.Save(fs))

/// Defines a subprogram that prints the given property of the parse tree
/// in file referred to by the single argument.
let printTreeProperty (show : ParseTree<string, string> -> string) (argv : string list) =
    match argv with
    | [fileName] ->
        Result.printWith show (readParseTreeFile fileName)
    | _ ->
        eprint "The specified subprogram takes exactly one argument: the file name of the parse tree file."

/// Defines a subprogram function that reads input from a file, and writes it to another file.
let performReadWrite (read : string -> Result<'a>) (write : string -> 'a -> Result<unit>) (argv : string list) =
    match argv with
    | [inputFileName; outputFileName] ->
        Result.eprintf (read inputFileName |> Result.bind (write outputFileName))
    | _ ->
        eprint "The specified subprogram takes exactly two arguments: the input and output file names."


let readTreeGrammar (fileName : string) : Result<ContextFreeGrammar<char, char>> =
    readParseTreeFile fileName
    |> Result.bind ContextFreeGrammar.ofParseTree 
    |> Result.bind ContextFreeGrammar.toCharacterGrammar

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
