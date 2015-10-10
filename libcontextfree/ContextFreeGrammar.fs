namespace libcontextfree

/// Defines a formal context-free grammar as a quadruple (V, T, P, S) of
/// nonterminals, terminals, production rules, and the starting symbol.
/// 
/// V and T are not stored in the context-free grammar itself. Rather, they
/// are inferred from the production rules and start symbol.
/// They can be accessed either through usage of the `V` and `T` members of
/// the grammar, or by using the `(|CFG|)` active pattern.
type ContextFreeGrammar<'nt, 't when 'nt : comparison and 't : comparison> =
    | ContextFreeGrammar of Set<ProductionRule<'nt, 't>> * 'nt

    /// Gets the set of nonterminals that occur in this context-free grammar.
    member this.V =
        match this with
        | ContextFreeGrammar(rules, start) ->
            rules |> Set.map (fun (ProductionRule(nt, str)) ->
                                  Symbol.nonterminals str
                                  |> Set.ofSeq
                                  |> Set.add nt)
                  |> Set.unionMany
                  |> Set.add start

    /// Gets the set of nonterminals that occur in this context-free grammar.
    member this.T =
        match this with
        | ContextFreeGrammar(rules, start) ->
            rules |> Set.map (fun (ProductionRule(_, str)) ->
                                  Set.ofSeq (Symbol.terminals str))
                  |> Set.unionMany

    /// Gets this context-free grammar's set of production rules.
    member this.P =
        match this with
        | ContextFreeGrammar(rules, _) -> rules

    /// Gets this context-free grammar's start symbol.
    member this.S =
        match this with
        | ContextFreeGrammar(_, start) -> start
    
[<AutoOpen>]
module ContextFreeGrammarOps =
    /// An active pattern that matches context-free grammars,
    /// including their sets of nonterminals and terminals.
    ///
    /// Usage:
    ///     match x with
    ///     | CFG(V, T, P, S) -> ...
    ///
    let (|CFG|) (grammar : ContextFreeGrammar<'nt, 't>) =
        (grammar.V, grammar.T, grammar.P, grammar.S)

module ContextFreeGrammar =
    /// Tries to infer the context-free grammar required to construct
    /// the given parse tree. This can be done iff the head of the parse
    /// tree is a production node. Otherwise, None is returned.
    let ofParseTree (tree : ParseTree<'nt, 't>) : Result<ContextFreeGrammar<'nt, 't>> =
        match tree with
        | ProductionNode(head, _) ->
            Success (ContextFreeGrammar(ParseTree.productionRules tree, head))
        | TerminalLeaf _ ->
            Error "Cannot infer a context-free grammar from a terminal leaf."

    /// Applies the given nonterminal and terminal mapping functions to the given grammar.
    let symbolMap (f : 'nt1 -> 'nt2) (g : 't1 -> 't2) (grammar : ContextFreeGrammar<'nt1, 't1>) : ContextFreeGrammar<'nt2, 't2> =
        match grammar with 
        | ContextFreeGrammar(P1, S1) ->
            // Converts a single production rule.
            let convertRule (ProductionRule(head, body)) : ProductionRule<'nt2, 't2> = 
                ProductionRule(f head, body |> List.map (Symbol.map f g))
            
            let P2 = Set.map convertRule P1
            let S2 = f S1
            ContextFreeGrammar(P2, S2)

    /// Tries to convert the given grammar, which operates on strings, to an
    /// equivalent grammar that operates on characters.
    /// Nonterminals may have their name changed, but terminals will never be modified.
    /// If the given grammar uses a terminal of any length other than one, the algorithm will
    /// fail, and None is returned.
    let toCharacterGrammar (grammar : ContextFreeGrammar<string, string>) : Result<ContextFreeGrammar<char, char>> =
        // Find strings whose length is not equal to one.
        let invalidStrings : seq<string> -> seq<string> = 
            Seq.filter (fun s -> String.length s <> 1)

        match grammar with
        | CFG(V, T, P, S) ->
            // TODO: pattern match and give a decent error message.
            let invalidTerminals = invalidStrings T
            if Seq.isEmpty invalidTerminals then
                let nonterminalMap =
                    if Seq.isEmpty (invalidStrings V) then
                        // If at all possible, we should re-use the old nonterminal names.
                        V |> Seq.map (fun str -> str, Seq.exactlyOne str)
                          |> Map.ofSeq
                    else
                        // We can just rename nonterminals, because nonterminal names
                        // don't have any observable effect on the grammar's language. 
                        V |> Seq.mapi (fun i str -> str, char (i + int 'A'))
                          |> Map.ofSeq
                let convNonterminal (nt : string) : char = nonterminalMap.[nt]
                let convTerminal    (t : string)  : char = Seq.exactlyOne t
                Success (symbolMap convNonterminal convTerminal grammar)
            else
                // If there is a string with any length other than one,
                // we can't perform this operation.
                Error (sprintf "Couldn't convert CFG: it contains invalid terminals %A." invalidTerminals)

    /// Show the nonterminals of a given context-free grammar as a new-line seperated list.
    let showNonterminals (grammar : ContextFreeGrammar<char, char>) : string =
        Seq.map string grammar.V |> String.concat (System.Environment.NewLine)

    /// Show the terminals of a given context-free grammar as a new-line seperated list.
    let showTerminals (grammar : ContextFreeGrammar<char, char>) : string =
        Seq.map string grammar.T |> String.concat (System.Environment.NewLine)

    /// Show the rules of a given context-free grammar as a new-line seperated list.
    let showRules (grammar : ContextFreeGrammar<char, char>) : string =
        grammar.P |> ProductionRules.show

    /// Show the start symbol of a given context-free grammar.
    let showStartSymbol (grammar : ContextFreeGrammar<char, char>) : string =
        string grammar.S
    
    