module FileHelpers

open libcontextfree
open System
open System.IO

/// Print a string, followed by a newline, to stdout.
let print = printfn "%s"

/// Print a string, followed by a newline, to stderr.
let eprint = eprintfn "%s"

/// Perform a file read action that fails with a helpful error message.
let readFile (fileName : string) (action : FileStream -> Result<'a>) : Result<'a> =
    try
        use fs = new FileStream(fileName, FileMode.Create, FileAccess.Write)
        action fs
    with
    | _ -> Error (sprintf "Couldn't open file for reading: '%s'." fileName)

/// Tries to read a file that contains a parse tree. If something goes wrong,
/// log an error.
let readParseTreeFile (fileName : string) : Result<ParseTree<string, string>> =
    readFile fileName <| fun fs ->
        use reader = new StreamReader(fs)
        TreeHandler.readNode reader

/// Tries to read a file that contains a parse tree and turn it into an inferred
/// context-free grammar. (If something goes wrong, an error is logged through the
/// use of Result.bind.)
let readTreeGrammar (fileName : string) : Result<ContextFreeGrammar<char, char>> =
    readParseTreeFile fileName
    |> Result.bind ContextFreeGrammar.ofParseTree 
    |> Result.bind ContextFreeGrammar.toCharacterGrammar

/// Tries to read a file that contains a context-free grammar over characters.
/// If something goes wrong, log an error.
let readContextFreeGrammarFile (fileName : string) : Result<ContextFreeGrammar<char, char>> =
    readFile fileName <| fun fs ->
        XmlHandler.toCfg (XmlHandler.CFGFile.Load(fs))

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
    writeFile fileName xmlNode.XElement.Save

/// Defines a subprogram that prints the given property of the parse tree
/// in file referred to by the single argument.
let printTreeProperty (show : ParseTree<string, string> -> string) (argv : string list) =
    match argv with
    | [fileName] ->
        Result.printWith show (readParseTreeFile fileName)
    | _ ->
        eprint "The specified subprogram takes exactly one argument: the file name of the parse tree file."

/// Defines a subprogram function that reads input from a file, and writes it to another file.
let performReadWrite (read : string -> Result<'a>)
                     (write : string -> 'a -> Result<unit>)
                     (argv : string list) =
    match argv with
    | [inputFileName; outputFileName] ->
        Result.eprintf (read inputFileName
                        |> Result.bind (write outputFileName))
    | _ ->
        eprint "The specified subprogram takes exactly two arguments: \
                the input and output file names."
