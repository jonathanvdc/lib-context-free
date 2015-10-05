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
module ContextFreeGrammar =
    /// An active pattern that matches context-free grammars,
    /// including their sets of nonterminals and terminals.
    ///
    /// Usage:
    ///     match x with
    ///     | CFG(V, T, P, S) -> ...
    ///
    let (|CFG|) (grammar : ContextFreeGrammar<'nt, 't>) =
        (grammar.V, grammar.T, grammar.P, grammar.S)