namespace libcontextfree

open System
open System.IO
open IOHelpers

module GraphvizHandler =
    let writeLabel (writer : TextWriter) (label : string) : unit =
        writer.Write(" [label=\"")
        writer.Write(label)
        writer.Write("\"];")
        writer.WriteLine()

    let writeEdge (writer : TextWriter) (fromIndex : int) (toIndex : int) : unit =
        writer.Write(fromIndex)
        writer.Write(" -> ")
        writer.Write(toIndex)
        writer.Write(";")
        writer.WriteLine()

    let writeIndentation (writer : TextWriter) : unit =
        writer.Write(String.replicate 4 " ")

    let rec writeGraphvizNode (writer : TextWriter) (startIndex : int) (node : ParseTree<string, string>) : int = 
        writeIndentation writer
        writer.Write(startIndex)
        match node with
        | EpsilonLeaf              -> 
            writeLabel writer "&epsilon;"
            startIndex + 1
        | TerminalLeaf t           ->
            writeLabel writer t
            startIndex + 1
        | ProductionNode(n, items) ->
            writeLabel writer n
            let foldNode index item =
                let nextIndex = writeGraphvizNode writer index item
                writeIndentation writer
                writeEdge writer startIndex index
                nextIndex
            items |> List.fold foldNode (startIndex + 1)

    let writeGraphviz (writer : TextWriter) (node : ParseTree<string, string>) = 
        writer.WriteLine("digraph PARSETREE {")
        writeIndentation writer
        writer.WriteLine("node [shape = circle];")
        ignore(writeGraphvizNode writer 0 node)
        writer.WriteLine("}")