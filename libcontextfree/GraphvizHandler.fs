namespace libcontextfree

open System
open System.IO
open IOHelpers

module GraphvizHandler =
    /// Writes a label attribute with the given label text.
    /// Label attributes are formatted as ` [label="<label>"];`.
    let writeLabel (writer : TextWriter) (label : string) : unit =
        writer.Write(" [label=\"")
        writer.Write(label)
        writer.Write("\"];")
        writer.WriteLine()

    /// Writes an unlabeled edge from one node to another.
    /// Nodes are identified by their indices. 
    /// Edges are formatted as `<fromIndex> -> <toIndex>;`.
    let writeEdge (writer : TextWriter) (fromIndex : int) (toIndex : int) : unit =
        writer.Write(fromIndex)
        writer.Write(" -> ")
        writer.Write(toIndex)
        writer.Write(";")
        writer.WriteLine()

    /// Writes four spaces of indentation.
    let writeIndentation (writer : TextWriter) : unit =
        writer.Write(String.replicate 4 " ")

    /// Writes a graphviz representation for the given node.
    /// The output's root node will has the start index,
    /// and this function returns an index that is suitable for
    /// use as its successor's node index.
    let rec writeGraphvizNode (writer : TextWriter) (startIndex : int) (node : ParseTree<string, string>) : int = 
        // First, write some indentation and the node's index.
        writeIndentation writer
        writer.Write(startIndex)
        match node with
        | TerminalLeaf t           ->
            // Writes the terminal leaf's label, which is a terminal.
            writeLabel writer t
            startIndex + 1
        | ProductionNode(n, items) ->
            // Writes the node's label, which is a nonterminal.
            writeLabel writer n
            match items with
            | [] ->
                // Production node with empty bodies
                // are mapped to epsilon production rules.
                // They are visualized in graphviz by  
                // inserting an epsilon node.
                writeIndentation writer
                writer.Write(startIndex + 1)
                writeLabel writer "&epsilon;"
                writeIndentation writer
                writeEdge writer startIndex (startIndex + 1)
                startIndex + 2
            | _  ->
                let foldNode index item =
                    // Writes a child node to the output stream.
                    let nextIndex = writeGraphvizNode writer index item
                    // Write some indentation and an edge definition to the text writer.
                    writeIndentation writer
                    writeEdge writer startIndex index
                    nextIndex
                // Writes this production node's children.
                // They are folded one by one using the folding function above.
                items |> List.fold foldNode (startIndex + 1)

    /// Writes a graphviz file that represents the given parse tree to the given text writer.
    let writeGraphviz (writer : TextWriter) (node : ParseTree<string, string>) = 
        // Write the dot header.
        writer.WriteLine("digraph PARSETREE {")
        // Write some indentation, and make all nodes circles.
        writeIndentation writer
        writer.WriteLine("node [shape = circle];")
        // Write the parse tree itself
        ignore(writeGraphvizNode writer 0 node)
        // Closing brace
        writer.WriteLine("}")