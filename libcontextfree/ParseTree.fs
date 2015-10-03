namespace libcontextfree

module ParseTreeModule =
    // Defines a parse tree type, based on a nonterminal and a terminal type.
    type ParseTree<'nonterminal, 'terminal> =
        // Defines a terminal leaf, which is a type of parse tree that 
        // contains only a single terminal.
        | TerminalLeaf of 'terminal
        // Defines an epsilon leaf, which is a type of parse tree that
        // contains an epsilon value.
        | EpsilonLeaf
        // Defines a production rule node, which is identified by a nonterminal and a 
        // list of child nodes.
        | ProductionNode of 'nonterminal * ParseTree<'nonterminal, 'terminal> list

    // Computes the yield of a parse tree.
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
            // the its children's yields.
            xs |> List.map treeYield 
               |> List.fold (+) ""