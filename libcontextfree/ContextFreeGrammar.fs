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
            let getNonterminals (str : Symbol<'nt, 't> list) : Set<'nt> =
                str |> Seq.choose (fun x -> match x with 
                                            | Nonterminal nt -> Some nt 
                                            | _              -> None)
                    |> Set.ofSeq

            rules |> Set.map (fun (ProductionRule(nt, str)) -> getNonterminals str |> Set.add nt)
                  |> Set.unionMany
                  |> Set.add start

    /// Gets the set of nonterminals that occur in this context-free grammar.
    member this.T =
        match this with
        | ContextFreeGrammar(rules, start) ->
            let getTerminals (str : Symbol<'nt, 't> list) : Set<'t> =
                str |> Seq.choose (fun x -> match x with
                                            | Terminal t -> Some t
                                            | _          -> None)
                    |> Set.ofSeq

            rules |> Set.map (fun (ProductionRule(_, str)) -> getTerminals str)
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
    let ofParseTree (tree : ParseTree<'nt, 't>) : ContextFreeGrammar<'nt, 't> option =
        match tree with
        | ProductionNode(head, _) ->
            Some(ContextFreeGrammar(ParseTree.productionRules tree, head))
        | _                       ->
            None

    /// Applies the given nonterminal and terminal mapping functions to the given symbol.
    let mapSymbol (f : 'nt1 -> 'nt2) (g : 't1 -> 't2) (sym : Symbol<'nt1, 't1>) : Symbol<'nt2, 't2> =
        match sym with
        | Nonterminal nt -> Nonterminal (f nt)
        | Terminal t     -> Terminal    (g t)

    /// Applies the given nonterminal and terminal mapping functions to the given grammar.
    let mapGrammar (f : 'nt1 -> 'nt2) (g : 't1 -> 't2) (grammar : ContextFreeGrammar<'nt1, 't1>) : ContextFreeGrammar<'nt2, 't2> =
        match grammar with 
        | ContextFreeGrammar(P1, S1) ->
            let convertRule (ProductionRule(head, body)) : ProductionRule<'nt2, 't2> = 
                ProductionRule(f head, body |> List.map (mapSymbol f g))
            let P2 = Set.map convertRule P1
            let S2 = f S1
            ContextFreeGrammar(P2, S2)

    /// Tries to convert the given grammar, which operates on strings, to an
    /// equivalent grammar that operates on characters.
    /// Nonterminals may have their name changed, but terminals will never be modified.
    /// If the given grammar uses a terminal of any length other than one, the algorithm will
    /// fail, and None is returned.
    let toCharacterGrammar (grammar : ContextFreeGrammar<string, string>) : ContextFreeGrammar<char, char> option =
        // Checks if the given sequence of strings contains one or more
        // strings whose length is not equal to one.
        let containsStrings : seq<string> -> bool = 
            Seq.exists (String.length >> ((<>) 1))

        match grammar with
        | CFG(V, T, P, S) ->
            if containsStrings T then
                None // If there is a string with any length other than one,
                     // we can't perform this operation. Return None.
            else
                let vMap = if containsStrings V then
                               // We can just rename nonterminals, because nonterminal names
                               // don't have any observable effect on the grammar's language. 
                               V |> Seq.mapi (fun i str -> str, char (i + int 'A'))
                                 |> Map.ofSeq
                           else
                               // If at all possible, we should re-use the old nonterminal names.
                               V |> Seq.map (fun str -> str, Seq.exactlyOne str)
                                 |> Map.ofSeq
                let convTerminal : string -> char =
                    Seq.exactlyOne
                let convNonterminal (nt : string) : char = 
                    vMap.[nt]
                Some (mapGrammar convNonterminal Seq.exactlyOne grammar)