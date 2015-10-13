namespace libcontextfree

/// Defines a parse tree type, based on a nonterminal and a terminal type.
type ParseTree<'nt, 't> =
    /// Defines a terminal leaf, which is a type of parse tree that 
    /// contains only a single terminal.
    | TerminalLeaf of 't
    /// Defines a production rule node, which is identified by a nonterminal and a 
    /// list of child nodes.
    | ProductionNode of 'nt * ParseTree<'nt, 't> list

module ParseTree =
    /// Applies the given mapping functions to a 
    /// parse tree.
    let rec map (f : 'nt1 -> 'nt2) (g : 't1 -> 't2) : ParseTree<'nt1, 't1> -> ParseTree<'nt2, 't2> =
        function
        | TerminalLeaf t -> TerminalLeaf(g t)
        | ProductionNode(nt, items) -> ProductionNode(f nt, List.map (map f g) items)

    /// Computes the yield of a parse tree, which is a list of terminals.
    let rec treeYield (tree : ParseTree<'nt, 't>) : 't list =
        match tree with
        | TerminalLeaf t -> [t]
        | ProductionNode(_, children) -> 
            // The yield of a production rule node is the concatenation of
            // its children's yields.
            children |> List.collect treeYield

    /// Gets a parse tree's "head", i.e. the terminal or nonterminal
    /// in the root node.
    let treeHead (tree : ParseTree<'nt, 't>) : Symbol<'nt, 't> =
        match tree with
        | TerminalLeaf t -> Terminal t
        | ProductionNode(nt, _) -> Nonterminal nt

    /// Performs a single derivation: a single nonterminal node (selected from the node list by the `splitAt` function)
    /// is replaced by its children.
    let derive splitAt (nodes : ParseTree<'nt, 't> list) : ParseTree<'nt, 't> list option =
        // `getProductionNodeChildren` is a local helper function that extracts
        // node children from production rule nodes.
        let getProductionNodeChildren (node : ParseTree<'nt, 't>) =
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
    /// at a time until only a list of terminal leaves remain. 
    /// Said nonterminal is selected by the given split-at function.
    /// The input node list is not included in the result list.
    let rec derivation splitAt (nodes : ParseTree<'nt, 't> list) : ParseTree<'nt, 't> list list =
        match derive splitAt nodes with
        | None   -> []
        | Some x -> x :: derivation splitAt x

    /// Gets the given tree's entire derivation 
    /// sequence, including the input tree itself. 
    /// Nonterminal nodes are replaced by the given split-at function.
    let derivationSequence splitAt (tree : ParseTree<'nt, 't>) : ParseTree<'nt, 't> list list =
        // The derivation sequence for this parse tree is obtained, 
        // and the parse tree is explicitly prepended.
        [tree] :: derivation splitAt [tree]

    /// Gets the set of production rules that are used in this parse tree.
    let rec productionRules (tree : ParseTree<'nt, 't>) : ProductionRule<'nt, 't> Set =
        match tree with
        | TerminalLeaf _ -> Set.empty
        | ProductionNode(head, children) ->
            let body = children |> List.map treeHead
            let rule = ProductionRule(head, body)
            let childRules = children |> List.map productionRules
            Set.add rule (Set.unionMany childRules)

    /// Gets a string representation for a parse tree's "head", 
    /// i.e. the terminal or nonterminal in the root node.
    let showTreeHead<'nt, 't> : ParseTree<'nt, 't> -> string = 
        treeHead >> string

    /// Get a string representation for the yield of a parse tree.
    let showTreeYield<'nt, 't> : ParseTree<'nt, 't> -> string = 
        treeYield >> Seq.map (fun x -> x.ToString())
                  >> String.concat ""

    /// Gets a string representation for the given tree's entire derivation 
    /// sequence, including the input tree itself. 
    /// Nonterminal nodes are replaced by the given split-at function.
    let showDerivationSequence splitAt (tree : ParseTree<'nt, 't>) : string =
        // First, every step in the derivation sequence is converted to the
        // concatenation of its trees' head strings.
        // Then, the results of that operation are joined with the " => " separator.
        derivationSequence splitAt tree |> List.map (List.map showTreeHead >> String.concat "")
                                        |> String.concat " => "

    /// Gets a string representation for the given tree's entire leftmost derivation 
    /// sequence, including the input tree itself. 
    let showLeftmostDerivationSequence<'nt, 't> : ParseTree<'nt, 't> -> string = 
        showDerivationSequence ListHelpers.splitAtFirst

    /// Gets a string representation for the given tree's entire rightmost derivation 
    /// sequence, including the input tree itself. 
    let showRightmostDerivationSequence<'nt, 't> : ParseTree<'nt, 't> -> string = 
        showDerivationSequence ListHelpers.splitAtLast

    /// Gets a string representation of the set of production rules that are used in this parse tree.
    let showProductionRules (tree : ParseTree<'nt, 't>) : string =
        productionRules tree |> ProductionRules.show

    (*
     *  These functions are for Graphviz output:
     *)

    /// Return a list of all immediate children of this node.
    let children : ParseTree<'nt, 't> -> ParseTree<'nt, 't> list =
        function
        | TerminalLeaf _ -> []
        | ProductionNode(_, ps) -> ps

    /// Return a list of all nodes in the tree.
    let rec allNodes (tree : ParseTree<'nt, 't>) : ParseTree<'nt, 't> list =
        tree :: List.collect allNodes (children tree)