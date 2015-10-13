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
        | NoneShape
        /// A circular shape.
        | CircleShape
        /// A shape that consists of two circles.
        | DoubleCircleShape

    /// Defines a single node in a graphviz graph.
    /// A node consists of a label, a shape and
    /// a set of edges.
    [<ReferenceEquality>]
    type GraphvizNode = { Label : string;
                          Shape : GraphvizShape
                          Edges : GraphvizEdge list Lazy; }

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
                | NoneShape -> "none"
                | CircleShape -> "circle"
                | DoubleCircleShape -> "doublecircle"

            let label = StringHelpers.dotEscape node.Label
            writer.WriteLine (sprintf "    node%d [label=\"%s\" shape=%s];"
                                  fromIndex label shape)
            for edge in node.Edges.Value do
                writeEdge fromIndex edge

        and writeEdge (fromIndex : int) (edge : GraphvizEdge) =
            // XXX: Why is edge.Target ever null here???
            if not(namedict.ContainsKey edge.Target) then
                writeNode edge.Target

            let toIndex = indexNode edge.Target
            let arrow = if edge.Directed then "->" else "--"
            let label = StringHelpers.dotEscape edge.Label
            let line = sprintf "    node%d %s node%d [label=\"%s\"];"
                            fromIndex arrow toIndex label

            writer.WriteLine line

        graph.Nodes |> List.iter writeNode

        writer.WriteLine "}"

    /// Creates a graphviz graph from the given parse tree.
    let createParseTreeGraph (tree : ParseTree<string, string>) : GraphvizGraph =
        let rec makeEdge (target : ParseTree<string, string>) : GraphvizEdge =
            { Label = "";
              Directed = true;
              Target = visit target }

        // Visits a node, which involves creating edges for its children.
        // Resturn the constructed GraphvizNode after adding it to a list.
        and visit (node : ParseTree<string, string>) : GraphvizNode =
            match node with
            | TerminalLeaf t -> 
                { Label = string t;
                  Shape = NoneShape;
                  Edges = lazy [] }
            | ProductionNode(nt, []) ->
                let epsilonEdge = 
                    { Label = "";
                      Directed = true;
                      Target = { Label = "ε"; Shape = NoneShape; Edges = lazy [] } }
                
                { Label = string nt;
                  Shape = NoneShape;
                  Edges = lazy [epsilonEdge] }
            | ProductionNode(nt, items) ->
                { Label = string nt;
                  Shape = NoneShape;
                  Edges = lazy List.map makeEdge (ParseTree.children node) }

        { Name = "PTREE";
          Nodes = [visit tree] }

    /// Construct a Graphviz graph from a pushdown automaton over characters.
    let createPushdownAutomatonGraph (pda : PushdownAutomaton<'Q, char, char>) : GraphvizGraph =
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
                    // Find all arrows pointing from this q.
                    let arrows =
                        [ for (qSource, a, Y), v in Map.toSeq δ do
                            if q = qSource then
                                for (qTarget, g) in v do
                                    yield (qTarget, (a, Y, g)) ]
                    
                    // Group them by target.
                    let groups : seq<int * (char option * char * char list) list> =
                        MapHelpers.groupFst arrows |> Map.toSeq

                    // Create a new GraphvizNode with one edge for each target.
                    let node =
                        { Label = sprintf "q%d" q;
                          Shape = if Set.contains q F then DoubleCircleShape else CircleShape;
                          Edges = lazy [ for qTarget, g in groups -> makeEdge qTarget g ] }
                    nodeMap.Add(q, node)
                    node

            and makeEdge qTarget (paths : seq<char option * char * char list>) =
                // We want to display epsilon symbols and empty strings as ε.
                let labelLine (a, Y, g) =
                    let a' = defaultArg a 'ε'
                    let g' = if List.isEmpty g then "ε" else new string (List.toArray g)
                    sprintf "%c, %c / %s" a' Y g'

                { Label = String.concat "\n" (Seq.map labelLine paths);
                  Directed = true;
                  Target = makeNode qTarget }

            // Point a "start" arrow at the first node.
            let startNode : GraphvizNode =
                { Label = "start";
                  Shape = NoneShape;
                  Edges = lazy [{ Label = ""; Directed = true; Target = makeNode q0 }] }

            // Together, these complete the graph.
            { Name = "PDA"; Nodes = [startNode] }

    /// Write the parse tree to the given TextWriter in Graphviz format.
    let writeParseTreeGraph (writer : TextWriter) (tree : ParseTree<string, string>) : unit =
        createParseTreeGraph tree |> writeGraph writer

    /// Write the pushdown automaton to the given TextWriter in Graphviz format.
    let writePushdownAutomatonGraph (writer : TextWriter) (pda : PushdownAutomaton<'Q, char, char>) : unit =
        writeGraph writer (createPushdownAutomatonGraph pda)
