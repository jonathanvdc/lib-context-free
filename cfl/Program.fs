open libcontextfree
open ParseTreeHelpers
open System
open System.IO

/// Tries to read a file that contains a parse tree. If something goes wrong,
/// None is returned.
let readParseTreeFile (fileName : string) : ParseTree<string, string> option =
    try
        use fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)
        use reader = new StreamReader(fs)
        TreeHandler.readNode reader
    with
    | _ -> None

/// Tries to write the given parse tree to the given output file.
/// A boolean is returned that indicates success or failure.
let writeGraphvizFile (tree : ParseTree<string, string>) (fileName : string) : bool =
    try
        use fs = new FileStream(fileName, FileMode.Create, FileAccess.Write)
        use writer = new StreamWriter(fs)
        GraphvizHandler.writeGraphviz writer tree
        true
    with
    | _ -> false

/// Defines a subprogram that prints the given property of the parse tree
/// in file referred to by the single argument.
let printTreeProperty (show : ParseTree<string, string> -> string) (argv : string list) =
    match argv with
    | [fileName] ->
        match readParseTreeFile fileName with
        | None ->
            printfn "%s" ("Could not read parse tree file '" + fileName + "'.")
        | Some tree ->
            printfn "%s" (show tree)
    | _          ->
        printfn "%s" "The specified subprogram takes exactly one argument: the file name of the parse tree file."

/// Defines a subprogram function that reads input from a file, and writes it to another file.
let performReadWrite (read : string -> 'a option) (write : 'a -> string -> bool) (argv : string list) =
    match argv with
    | [inputFileName; outputFileName] ->
        match read inputFileName with
        | None       -> printfn "%s" ("Could not read contents of file '" + inputFileName + "'.")
        | Some input -> 
            if not(write input outputFileName) then
                printfn "%s" ("Could not write data to file '" + outputFileName + "'.")
    | _                               ->
        printfn "%s" "The specified subprogram takes exactly two arguments: the input and output file names."

/// Gets the program's list of "subprograms",
/// which are really just pairs of strings and 
/// procedures that take a list of strings.
let subprograms : Map<string, string list -> unit> = 
    Map.ofList [
                   "head", printTreeProperty treeHead
                   "yield", printTreeProperty treeYield
                   "derive-leftmost", printTreeProperty showLeftmostDerivationSequence
                   "derive-rightmost", printTreeProperty showRightmostDerivationSequence
                   "dot", performReadWrite readParseTreeFile writeGraphvizFile
                   // Insert additional subprograms here.
               ]

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | x :: xs ->
        // If there is at least one command-line argument,
        // we'll run the subprogram belonging to the
        // first argument.
        subprograms.[x] xs
    | _ ->
        // If no subprogram has been specified, all we can realistically do is
        // rage quit.
        printfn "%s" "Please specify a subprogram. List of subprograms:"
        subprograms |> Seq.iter ((fun x -> x.Key) >> printfn " * %s")
    0 // return an integer exit code
