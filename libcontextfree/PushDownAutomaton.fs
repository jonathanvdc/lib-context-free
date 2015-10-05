namespace libcontextfree

/// The type of the transition map δ in a PushdownAutomaton<'Q, 'Σ, 'Γ>.
type Transition<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    Map<'Q * 'Σ option * 'Γ, Set<'Q * 'Γ list>>

/// Defines a formal Pushdown automaton as a 7-tuple (Q, Σ, Γ, δ, q0, Z0, F), where:
///P
///   * Q is the set of states.            : Set<'Q>
///   * Σ is the set of input symbols.     : Set<'Σ>
///   * Γ is the set of stack symbols.     : Set<'Γ>
///   * δ is the transition map.           : Transition<'Q, 'Σ, 'Γ>
///   * q0 is the initial state.           : 'Q
///   * Z0 is the initial stack symbol.    : 'Γ
///   * F is the set of accepting states.  : Set<'Q>
///
/// However, Q, Σ, and Γ can be derived from the representation of δ as a set. As such,
/// our PDAs only consist of (δ, q0, Z0, F).
type PushdownAutomaton<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    | PushdownAutomaton of Transition<'Q, 'Σ, 'Γ> * 'Q * 'Γ * Set<'Q>
    
    member pdf.Q : Set<'Q> =
        match pdf with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractQ (KeyValue((q, _, _), ps)) =
                Set.add q (Set.map fst ps)
            δ |> Seq.map extractQ |> Set.unionMany

    member pdf.Σ : Set<'Σ> =
        match pdf with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractΣ (KeyValue((_, sym, _), _)) = sym
            δ |> Seq.choose extractΣ |> Set.ofSeq

    member pdf.Γ : Set<'Γ> =
        match pdf with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractΓ (KeyValue((_, _, γ), ps)) =
                let γs = Seq.collect snd ps
                Set.add γ (Set.ofSeq γs)
            δ |> Seq.map extractΓ |> Set.unionMany

module PushdownAutomaton =
    /// Convert a context-free grammar to a pushdown automaton as per slide 75.
    let ofCFG : ContextFreeGrammar<'nt, 't> -> PushdownAutomaton<unit, 't, Symbol<'nt, 't>> =
        function
        | ContextFreeGrammar (V, T, P, S) ->
            // Our only state.
            let q : unit = ()

            let δ : Transition<unit, 't, Symbol<'nt, 't>> =
                // The definition of δ for nonterminals:
                let δ1 = seq [for ProductionRule(A, β) in P ->
                              ((q, None, Nonterminal A), Set.singleton (q, β))]
                // The definition of δ for terminals:
                let δ2 = seq [for t in T ->
                              ((q, Some t, Terminal t), Set.singleton (q, []))]
                // δ is their union.
                Map.ofSeq (Seq.append δ1 δ2)

            PushdownAutomaton (δ, q, Nonterminal S, Set.empty)
