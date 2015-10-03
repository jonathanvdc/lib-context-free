namespace libcontextfree

open ListHelpers

module ParseTreeModule =
    /// Defines a parse tree type, based on a nonterminal and a terminal type.
    type ParseTree<'nonterminal, 'terminal> =
        /// Defines a terminal leaf, which is a type of parse tree that 
        /// contains only a single terminal.
        | TerminalLeaf of 'terminal
        /// Defines an epsilon leaf, which is a type of parse tree that
        /// contains an epsilon value.
        | EpsilonLeaf
        /// Defines a production rule node, which is identified by a nonterminal and a 
        /// list of child nodes.
        | ProductionNode of 'nonterminal * ParseTree<'nonterminal, 'terminal> list

    /// Computes the yield of a parse tree.
    let rec treeYield (tree : ParseTree<'nonterminal, 'terminal>) : string =
        match tree with
        | TerminalLeaf x        -> 
            // The yield of a terminal is the terminal's string representation.
            // Note: I'm casting `x` to `obj` here, because the `string` function
            //       requires a concrete type to operate on. Not doing that here causes
            //       fsc to unify `'terminal` and `obj`.
            string(x :> obj)
        | EpsilonLeaf           -> 
            // The yield of an epsilon leaf is the empty string.
            "" 
        | ProductionNode(_, xs) -> 
            // The yield of a production rule node is the concatenation of
            // its children's yields.
            xs |> List.map treeYield 
               |> List.fold (+) ""

    /// Computes a parse tree's "head", i.e. the string representation of
    /// its root node.
    let treeHead (tree : ParseTree<'nonterminal, 'terminal>) : string =
        match tree with
        | TerminalLeaf x       ->
            // The head of a terminal leaf is the terminal's string representation.
            string(x :> obj)
        | EpsilonLeaf          ->
            // The head of an epsilon leaf is the empty string.
            ""
        | ProductionNode(x, _) -> 
            // The head of a production rule node is the associated nonterminal.
            string(x :> obj)

    /// Performs a single derivation: a single nonterminal node (selected from the node list by the `splitAt` function)
    /// is replaced by its children.
    let derive splitAt (nodes : ParseTree<'a, 'b> list) : ParseTree<'a, 'b> list option =
        // `getProductionNodeChildren` is a local helper function that extracts
        // node children from production rule nodes.
        let getProductionNodeChildren (node : ParseTree<'a, 'b>) =
            // Checks if this node is a production rule node.
            // If so, return its items. Otherwise, return None.
            match node with
            | ProductionNode(_, items) -> Some(items)
            | _                        -> None

        // Try to split the list of input nodes at a production node.
        match splitAt getProductionNodeChildren nodes with
        | None -> 
            // Couldn't find a production node. 
            // Derivation cannot be performed.
            None
        | Some(preElements, children, postElements) ->
            // A production node was found, the list was split.
            // Performing a derivation on a parse tree is 
            // equivalent to replacing a production rule node
            // by its children.
            Some(List.concat [preElements; children; postElements])

    /// Creates a list of node lists that is obtained by starting
    /// with a single node list, and then deriving one nonterminal
    /// at a time until only a list of terminal/epsilon leaves remain. 
    /// Said nonterminal is selected by the given split-at function.
    let rec derivation splitAt (nodes : ParseTree<'a, 'b> list) : ParseTree<'a, 'b> list list =
        match derive splitAt nodes with
        | None   -> []
        | Some x -> x :: derivation splitAt x