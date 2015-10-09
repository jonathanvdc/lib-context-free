﻿namespace libcontextfree

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
                          mutable Edges : GraphvizEdge list; }

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
            let result =
                { Label = ParseTree.showTreeHead node;
                  Shape = NoneShape;
                  Edges = List.map makeEdge (ParseTree.children node) }

            result

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
                    // Before creating the list of edges, add this node to the dictionary
                    // to signify that we're already working on it; otherwise, we will
                    // run into an infinite loop (makeNode -> makeEdge -> makeNode -> ...)
                    let node =
                        { Label = sprintf "q%d" q;
                          Shape = if Set.contains q F then DoubleCircleShape else CircleShape;
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
                  Shape = NoneShape;
                  Edges = [{ Label = ""; Directed = true; Target = q0node }] }

            // Together, these complete the graph.
            { Name = "PDA";
              Nodes = startNode :: qNodes }

    /// Write the parse tree to the given TextWriter in Graphviz format.
    let writeParseTreeGraph (writer : TextWriter) (tree : ParseTree<string, string>) : unit =
        createParseTreeGraph tree |> writeGraph writer

    /// Write the pushdown automaton to the given TextWriter in Graphviz format.
    let writePushdownAutomatonGraph (writer : TextWriter) (pda : PushdownAutomaton<'Q, char, char>) : unit =
        writeGraph writer (createPushdownAutomatonGraph pda)
