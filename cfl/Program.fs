open libcontextfree
open ParseTreeModule
open TreeHandler
open System
open System.IO

/// Tries to read a file that contains a parse tree. If something goes wrong,
/// None is returned.
let readParseTreeFile (fileName : string) : ParseTree<string, string> option =
    try
        use fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)
        use reader = new StreamReader(fs)
        readNode reader
    with
    | _ -> None

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

/// Gets the program's list of subprograms.
let subprograms = 
    Map.ofList [
                   "head", printTreeProperty treeHead
                   "yield", printTreeProperty treeYield
                   "derive-leftmost", printTreeProperty showLeftmostDerivationSequence
                   "derive-rightmost", printTreeProperty showRightmostDerivationSequence
               ]

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | x :: xs ->
        subprograms.[x] xs
    | _ ->
        printfn "%s" "Please specify a subprogram. List of subprograms:"
        subprograms |> Seq.iter ((fun x -> x.Key) >> printfn " * %s")
    0 // return an integer exit code
