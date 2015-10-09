namespace libcontextfree

open System
open System.IO
open IOHelpers

module GraphvizHandler =
    /// A node in a Graphviz graph.
    [<ReferenceEquality>]
    type GraphvizNode = { Label : string;
                          Shape : GraphvizShape 
                          mutable Edges : GraphvizEdge list; }

    /// The shape of a Graphviz node.
    and GraphvizShape =
        | None
        | Circle
        | DoubleCircle

    /// An edge in a Graphviz graph.
    and GraphvizEdge = { Label : string;
                         Directed : bool;
                         Target : GraphvizNode; }
    
    /// A representation of a Graphviz graph.
    type GraphvizGraph = GraphvizNode list

    /// Writes a GraphvizGraph to a TextWrite in the .dot format.
    let writeGraph (writer : TextWriter) (graph : GraphvizGraph) : unit =
        writer.WriteLine "digraph {"
        
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

        writer.WriteLine "}"
        
    /// Constructs a Graphviz graph from a parse tree.
    let parseTreeGraph (tree : ParseTree<string, string>) : GraphvizGraph =
        // We'll collect the visited nodes in a mutable list.
        let visitedNodes = new System.Collections.Generic.List<GraphvizNode>()
        
        // Creates an edge, which involves visiting its target node.
        let rec makeEdge (target : ParseTree<string, string>) : GraphvizEdge =
            { Label = "";
              Directed = true;
              Target = visit target }

        // Visits a node, which involves creating edges for its children.
        // Resturn the constructed GraphvizNode after adding it to a list.
        and visit (node : ParseTree<string, string>) : GraphvizNode =
            let result =
                { Label = ParseTree.showTreeHead node;
                  Shape = None;
                  Edges = List.map makeEdge (ParseTree.children node) }
            
            visitedNodes.Add result
            result
                
        // We visit the whole tree and throw away the resulting root GraphvizNode...
        ignore (visit tree)

        // ...since all we care about is the list of *all* visited nodes.
        List.ofSeq visitedNodes

    /// Construct a Graphviz graph from a pushdown automaton over characters.
    let pushdownAutomatonGraph (pda : PushdownAutomaton<'Q, char, char>) : GraphvizGraph =
        match PushdownAutomaton.enumerate pda with
        | PDA(Q, Σ, Γ, δ, q0, Z0, F) ->
            // We will memoize nodes to this mutable dictionary.
            let nodeMap = new System.Collections.Generic.Dictionary<int, GraphvizNode>()
            
            // makeNode and makeEdge corecursively search the PDA depth-first, building
            // parts of a GraphvizGraph as they go along.
            let rec makeNode (q : int) : GraphvizNode =
                match nodeMap.TryGetValue(q) with
                | true, node -> node
                | false, _ ->
                    // Before creating the list of edges, add this node to the dictionary
                    // to signify that we're already working on it; otherwise, we will
                    // run into an infinite loop (makeNode -> makeEdge -> makeNode -> ...)
                    let node =
                        { Label = sprintf "q%d" q;
                          Shape = if Set.contains q F then DoubleCircle else Circle;
                          Edges = [] }
                    nodeMap.Add(q, node)

                    // Find all the arrows pointing from this q.
                    // (TODO: group edges by common (q, p) as a shorthand.)
                    node.Edges <-
                        [ for (q', a, Y), v in Map.toSeq δ do
                              if q = q' then
                                  for (p, g) in v do
                                      yield makeEdge (q, a, Y) (p, g) ]
                    node
            
            and makeEdge (q, a, Y) (p, g : char list) =
                // We want to display epsilon symbols and empty strings as ε.
                let a' = defaultArg a 'ε'
                let g' = if List.isEmpty g then "ε" else new string (List.toArray g)
                { Label = sprintf "%c, %c / %s" a' Y g';
                  Directed = true;
                  Target = makeNode p }
               
            // All of the nodes representing states in the PDA.
            let qNodes : GraphvizNode list =
                List.map makeNode (Set.toList Q)
            
            // Find the node corresponding to q0...
            let q0node : GraphvizNode =
                snd (nodeMap.TryGetValue q0)
            
            // And point a "start" arrow at it.
            let startNode : GraphvizNode =
                { Label = "start";
                  Shape = None;
                  Edges = [{ Label = ""; Directed = true; Target = q0node }] }
            
            // Together, these complete the graph.
            startNode :: qNodes

    /// Write the parse tree to the given TextWriter in Graphviz format.
    let writeParseTreeGraph (writer : TextWriter) (tree : ParseTree<string, string>) : unit =
        writeGraph writer (parseTreeGraph tree)

    /// Write the pushdown automaton to the given TextWriter in Graphviz format.
    let writePushdownAutomatonGraph (writer : TextWriter) (pda : PushdownAutomaton<'Q, char, char>) : unit =
        writeGraph writer (pushdownAutomatonGraph pda)

