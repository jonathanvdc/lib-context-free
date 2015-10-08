namespace libcontextfree

open System
open System.IO
open IOHelpers

module GraphvizHandler =
    type GraphvizShape =
        | None
        | Circle
        | DoubleCircle

    [<ReferenceEquality>]
    type GraphvizNode = { Label : string;
                          Shape : GraphvizShape 
                          Edges : GraphvizEdge list; }

    and GraphvizEdge = { Label : string;
                         Directed : bool;
                         Target : GraphvizNode; }
    
    type GraphvizGraph = GraphvizNode list

    let writeGraph (writer : TextWriter) (graph : GraphvizGraph) : unit =
        writer.WriteLine "digraph G {"
        
        let nodeNames : (int * GraphvizNode) list =
            List.mapi (fun i n -> (i, n)) graph
        
        for fromIndex, node in nodeNames do
            let shape =
                match node.Shape with
                | None -> "none"
                | Circle -> "circle"
                | DoubleCircle -> "doublecircle"

            let label = StringHelpers.htmlEscape node.Label
            writer.WriteLine (sprintf "    node%d [label=\"%s\" shape=%s];"
                                  fromIndex label shape)

            for edge in node.Edges do
                let toIndex =
                    List.find (fun (_, n) -> edge.Target = n) nodeNames |> fst
                
                let arrow = if edge.Directed then "->" else "--"
                let label = StringHelpers.htmlEscape edge.Label
                let line = sprintf "    node%d %s node%d [label=\"%s\"];"
                               fromIndex arrow toIndex label

                writer.WriteLine line

        writer.WriteLine("}")
        
    let parseTreeGraph (tree : ParseTree<string, string>) : GraphvizGraph =
        // We'll collect the visited nodes in a mutable list.
        let visitedNodes = new System.Collections.Generic.List<GraphvizNode>()
        
        // Create an edge, which involves visiting its target node.
        let rec makeEdge (target : ParseTree<string, string>) : GraphvizEdge =
            { Label = "";
              Directed = true;
              Target = visit target }

        // Visit a node, which involves creating edges for its children.
        // Resturn the constructed GraphvizNode after adding it to a list.
        and visit (node : ParseTree<string, string>) : GraphvizNode =
            let result =
                { Label = ParseTree.showTreeHead node;
                  Shape = None;
                  Edges = List.map makeEdge (ParseTree.children node) }
            
            visitedNodes.Add result
            result
                
        ignore (visit tree)
        List.ofSeq visitedNodes

    let writeParseTreeGraph (writer : TextWriter) (tree : ParseTree<string, string>) : unit =
        writeGraph writer (parseTreeGraph tree)