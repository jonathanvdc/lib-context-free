module FileHelpers

open libcontextfree
open System
open System.IO

/// Print a string, followed by a newline, to stdout.
let print = printfn "%s"

/// Print a string, followed by a newline, to stderr.
let eprint = eprintfn "%s"

/// Perform a file read action that fails with a helpful error message.
let readFile (path : string) (action : FileStream -> Result<'a>) : Result<'a> =
    try
        use fs = new FileStream(path, FileMode.Open, FileAccess.Read)
        action fs
    with
    | :? System.IO.IOException ->
        Error (sprintf "Couldn't open file for reading: '%s'." path)

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
        XmlHandler.toCfg (XmlHandler.CfgFile.Load(fs))

/// Perform a file write action that fails with a helpful error message.
let writeFile (path : string) (action : FileStream -> unit) : Result<unit> =
    try
        use fs = new FileStream(path, FileMode.Create, FileAccess.Write)
        action fs
        Success ()
    with
    | :? System.IO.IOException ->
        Error (sprintf "Couldn't open file for writing: '%s'." path)

/// Tries to write the given parse tree to the given output file path,
/// as a graphviz file.
let writeParseTreeGraphvizFile (path : string) (tree : ParseTree<string, string>) : Result<unit> =
    writeFile path <| fun fs ->
        use writer = new StreamWriter(fs)
        GraphvizHandler.writeParseTreeGraph writer tree

/// Tries to write the given parse tree to the given output file path,
/// as an S-expression.
let writeParseTreeFile (path : string) (tree : ParseTree<string, string>) : Result<unit> =
    writeFile path <| fun fs ->
        use writer = new StreamWriter(fs)
        TreeHandler.writeTree writer tree

/// Tries to write the given character CFG to the given output file path.
let writeCfgXmlFile (path : string) (grammar : ContextFreeGrammar<char, char>) : Result<unit> =
    let xmlNode = XmlHandler.ofCfg grammar
    writeFile path xmlNode.XElement.Save

let writePdaGraphvizFile (path : string) (tree : PushdownAutomaton<'Q, char, char>) : Result<unit> =
    writeFile path <| fun fs ->
        use writer = new StreamWriter(fs)
        GraphvizHandler.writePushdownAutomatonGraph writer tree

/// Defines a subprogram that prints the given property of the parse tree
/// in file referred to by the single argument.
let printTreeProperty (show : ParseTree<string, string> -> string) (argv : string list) =
    match argv with
    | [fileName] ->
        Result.printWith show (readParseTreeFile fileName)
    | _ ->
        eprint "The specified subprogram takes exactly one argument: \
                the file name of the parse tree file."

/// Defines a subprogram that prints the given property of the context-free grammar
/// in file referred to by the single argument. An error may occur.
let maybePrintCfgProperty (show : ContextFreeGrammar<char, char> -> Result<string>) (argv : string list) =
    match argv with
    | [fileName] ->
        readContextFreeGrammarFile fileName |> Result.bind show
                                            |> Result.print
    | _ ->
        eprint "The specified subprogram takes exactly one argument: \
                the file name of the context-free grammar file."

/// Defines a subprogram that prints the given property of the context-free grammar
/// in file referred to by the single argument.
let printCfgProperty (show : ContextFreeGrammar<char, char> -> string) (argv : string list) =
    maybePrintCfgProperty (show >> Result.Success) argv

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

/// Reads standard input to completion as a list of characters.
let rec readStdinToEnd() : char list =
    match Console.Read() with
    | -1 -> [] // This indicates either Ctrl-Z or the end of the input file.
    | c  -> char c :: readStdinToEnd()

/// Reads standard input to completion as a list of characters,
/// then trims trailing newlines.
let readStdinToTrimmedEnd() : char list =
    let folder (c : char) (isEnd : bool, result : char list) =
        if isEnd && (c = '\n' || c = '\r') then
            true, result
        else
            false, c :: result

    let rawInput = readStdinToEnd() 
    List.foldBack folder rawInput (true, []) |> snd

let writeAll (write : string -> 'a -> Result<unit>) (pathPattern : string) (emptyResult : Result<unit>) (values : seq<'a>) : Result<unit> =
    let values = Seq.cache values
    if values |> Seq.isEmpty then
        emptyResult
    else
        if values |> Seq.skip 1
                  |> Seq.isEmpty then
            write pathPattern (Seq.head values)
        else
            let name = Path.GetFileNameWithoutExtension pathPattern
            let extension = Path.GetExtension pathPattern
            let writeItem i value = 
                write (name + string i + extension) value
            
            values |> Seq.mapi writeItem
                   |> Result.sequence
                   |> Result.map ignore

let performParse (parse : ContextFreeGrammar<char, char> -> char list -> Result<#seq<ParseTree<char, char>>>) (argv : string list) =
    match argv with
    | [grammarPath; outputPath] ->
        match readContextFreeGrammarFile grammarPath with
        | Success grammar ->
            let inputString = readStdinToTrimmedEnd()
            inputString |> parse grammar
                        |> Result.map (Seq.map (ParseTree.map string string))
                        |> Result.map (writeAll writeParseTreeFile outputPath (Error "The given string does not belong to the given grammar."))
                        |> Result.map Result.eprintf
                        |> Result.eprintf
        | Error e ->
            eprint e
    | _ ->
        eprint "The specified subprogram takes exactly two arguments: \
                the file name of the context-free grammar file, and the \
                output path pattern."

let performEarleyParse = 
    let parseEarley grammar input = 
        Result.Success (EarleyParser.parse grammar input)

    performParse parseEarley

let performLRParse (createParser : ContextFreeGrammar<char, char> -> Result<LRParser.LRMapParser<char, char>>) =
    let parseLR (grammar : ContextFreeGrammar<char, char>) (input : char list) =
        createParser grammar |> Result.map (fun parser ->
            let parseInput = LRParser.parse <||| LRParser.toFunctionalParser parser
            match parseInput input with
            | Choice1Of2 tree -> [tree]
            | Choice2Of2 _    -> []
        )

    performParse parseLR
