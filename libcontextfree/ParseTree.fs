namespace libcontextfree

/// Defines a parse tree type, based on a nonterminal and a terminal type.
type ParseTree<'Nonterminal, 'Terminal> =
    /// Defines a terminal leaf, which is a type of parse tree that 
    /// contains only a single terminal.
    | TerminalLeaf of 'Terminal
    /// Defines a production rule node, which is identified by a nonterminal and a 
    /// list of child nodes.
    | ProductionNode of 'Nonterminal * ParseTree<'Nonterminal, 'Terminal> list

module ParseTreeHelpers =
    /// Computes the yield of a parse tree, which is a list of terminals.
    let rec treeYield (tree : ParseTree<'a, 'b>) : 'b list =
        match tree with
        | TerminalLeaf x        -> 
            // The yield of a terminal is the terminal itself.
            [x]
        | ProductionNode(_, xs) -> 
            // The yield of a production rule node is the concatenation of
            // its children's yields.
            xs |> List.map treeYield 
               |> List.concat

    /// Gets a parse tree's "head", i.e. the terminal or nonterminal
    /// in the root node.
    let treeHead (tree : ParseTree<'a, 'b>) : Either<'a, 'b> =
        match tree with
        | TerminalLeaf x       ->
            // The head of a terminal leaf is the terminal itself.
            Right x
        | ProductionNode(x, _) -> 
            // The head of a production rule node is the associated nonterminal.
            Left x

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
    /// The input node list is not included in the result list.
    let rec derivation splitAt (nodes : ParseTree<'a, 'b> list) : ParseTree<'a, 'b> list list =
        match derive splitAt nodes with
        | None   -> []
        | Some x -> x :: derivation splitAt x

    /// Gets the given tree's entire derivation 
    /// sequence, including the input tree itself. 
    /// Nonterminal nodes are replaced by the given split-at function.
    let derivationSequence splitAt (tree : ParseTree<'a, 'b>) : ParseTree<'a, 'b> list list =
        // The derivation sequence for this parse tree is obtained, 
        // and the parse tree is explicitly prepended.
        [tree] :: derivation splitAt [tree] 

    /// Gets the set of production rules that are used in this parse tree.
    let rec productionRules (tree : ParseTree<'a, 'b>) : ProductionRule<'a, 'b> Set =
        match tree with
        | TerminalLeaf _ -> Set.empty
        | ProductionNode(head, items) -> 
            let body = items |> List.map treeHead
            let rule = ProductionRule(head, body)
            items |> List.fold (fun result item -> productionRules item |> Set.union result) (Set.singleton rule)

    /// Gets a string representation for a parse tree's "head", 
    /// i.e. the terminal or nonterminal in the root node.
    let showTreeHead<'a, 'b> : ParseTree<'a, 'b> -> string = 
        treeHead >> string

    /// Get a string representation for the yield of a parse tree.
    let showTreeYield<'a, 'b> : ParseTree<'a, 'b> -> string = 
        treeYield >> Seq.map (fun x -> x.ToString()) 
                  >> Seq.fold (+) ""

    /// Gets a string representation for the given tree's entire derivation 
    /// sequence, including the input tree itself. 
    /// Nonterminal nodes are replaced by the given split-at function.
    let showDerivationSequence splitAt (tree : ParseTree<'a, 'b>) : string =
        // First, every step in the derivation sequence is converted to the
        // concatenation of its trees' head strings.
        // Then, the results of that operation are joined with the " => " separator.
        derivationSequence splitAt tree |> List.map (List.map showTreeHead >> List.fold (+) "")
                                        |> String.concat " => "

    /// Gets a string representation for the given tree's entire leftmost derivation 
    /// sequence, including the input tree itself. 
    let showLeftmostDerivationSequence<'a, 'b> : ParseTree<'a, 'b> -> string = 
        showDerivationSequence ListHelpers.splitAtFirst

    /// Gets a string representation for the given tree's entire rightmost derivation 
    /// sequence, including the input tree itself. 
    let showRightmostDerivationSequence<'a, 'b> : ParseTree<'a, 'b> -> string = 
        showDerivationSequence ListHelpers.splitAtLast

    /// Gets a string representation of the set of production rules that are used in this parse tree.
    let showProductionRules (tree : ParseTree<'a, 'b>) : string =
        productionRules tree |> Seq.mapi (fun index item -> (string index) + ". " + (string item))
                             |> String.concat (System.Environment.NewLine)