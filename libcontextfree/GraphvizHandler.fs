namespace libcontextfree

open System
open System.IO
open IOHelpers

module GraphvizHandler =
    /// Defines a graphviz node's shape.
    type GraphvizShape =
        /// The "none" shape. This is named `NoShape`
        /// to avoid conflicts and confusion with
        /// `Option.None`.
        | NoShape
        /// A circular shape.
        | Circle
        /// A shape that consists of two circles.
        | DoubleCircle

    /// Defines a single node in a graphviz graph.
    /// A node consists of a label, a shape and
    /// a set of edges.
    [<ReferenceEquality>]
    type GraphvizNode = { Label : string;
                          Shape : GraphvizShape 
                          Edges : GraphvizEdge list; }

    /// Defines an edge in a graphviz graph.
    /// Every edge has a target, as well
    /// as a label, which may be empty.
    /// Edges can optionally be directed.
    and GraphvizEdge = { Label : string;
                         Directed : bool;
                         Target : GraphvizNode; }
    
    /// Defines a graphviz graph as a graph name and a 
    /// list of nodes.
    type GraphvizGraph = { Name : string; 
                           Nodes : GraphvizNode list }

    /// Writes the given graph to the given text writer.
    let writeGraph (writer : TextWriter) (graph : GraphvizGraph) : unit =
        writer.WriteLine (sprintf "digraph %s {" graph.Name)

        let namedict = System.Collections.Concurrent.ConcurrentDictionary<GraphvizNode, int>()

        let indexNode node = 
            namedict.GetOrAdd(node, fun _ -> namedict.Count)

        let rec writeNode node =
            let fromIndex = indexNode node
            let shape =
                match node.Shape with
                | NoShape -> "none"
                | Circle -> "circle"
                | DoubleCircle -> "doublecircle"

            let label = StringHelpers.htmlEscape node.Label
            writer.WriteLine (sprintf "    node%d [label=\"%s\" shape=%s];"
                                  fromIndex label shape)
            for edge in node.Edges do
                writeEdge fromIndex edge

        and writeEdge (fromIndex : int) (edge : GraphvizEdge) =
            if not(namedict.ContainsKey edge.Target) then
                writeNode edge.Target

            let toIndex = indexNode edge.Target
            let arrow = if edge.Directed then "->" else "--"
            let label = StringHelpers.htmlEscape edge.Label
            let line = sprintf "    node%d %s node%d [label=\"%s\"];"
                            fromIndex arrow toIndex label

            writer.WriteLine line

        graph.Nodes |> List.iter writeNode

        writer.WriteLine("}")
        
    /// Creates a graphviz graph from the given parse tree.
    let createParseTreeGraph (tree : ParseTree<string, string>) : GraphvizGraph =
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
                  Shape = NoShape;
                  Edges = List.map makeEdge (ParseTree.children node) }
            
            result
                
        { Name = "PTREE"; 
          Nodes = [visit tree] }

    /// Creates a parse tree graph and writes it to the given writer.
    let writeParseTreeGraph (writer : TextWriter) (tree : ParseTree<string, string>) : unit =
        createParseTreeGraph tree |> writeGraph writer