namespace libcontextfree

/// The type of the transition map δ in a PushdownAutomaton<'Q, 'Σ, 'Γ>.
type Transition<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    Map<'Q * 'Σ option * 'Γ, Set<'Q * 'Γ list>>

/// Defines a formal Pushdown automaton as a 7-tuple (Q, Σ, Γ, δ, q0, Z0, F), where:
///
///   * Q is the set of states.            : Set<'Q>
///   * Σ is the set of input symbols.     : Set<'Σ>
///   * Γ is the set of stack symbols.     : Set<'Γ>
///   * δ is the transition map.           : Transition<'Q, 'Σ, 'Γ>
///   * q0 is the initial state.           : 'Q
///   * Z0 is the initial stack symbol.    : 'Γ
///   * F is the set of accepting states.  : Set<'Q>
///
/// However, Q, Σ, and Γ can be derived from the representation of δ as a set. As such,
/// our PDAs only consist of (δ, q0, Z0, F), and the remaining sets are computed.
///
type PushdownAutomaton<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    | PushdownAutomaton of Transition<'Q, 'Σ, 'Γ> * 'Q * 'Γ * Set<'Q>
    
    member pda.Q : Set<'Q> =
        match pda with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractQ (KeyValue((q, _, _), ps)) =
                Set.add q (Set.map fst ps)
            δ |> Seq.map extractQ |> Set.unionMany

    member pda.Σ : Set<'Σ> =
        match pda with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractΣ (KeyValue((_, sym, _), _)) = sym
            δ |> Seq.choose extractΣ |> Set.ofSeq

    member pda.Γ : Set<'Γ> =
        match pda with
        | PushdownAutomaton (δ, q0, Z0, F) ->
            let extractΓ (KeyValue((_, _, γ), ps)) =
                let γs = Seq.collect snd ps
                Set.add γ (Set.ofSeq γs)
            δ |> Seq.map extractΓ |> Set.unionMany

    member pda.δ : Transition<'Q, 'Σ, 'Γ> =
        match pda with
        | PushdownAutomaton(δ, _, _, _) -> δ

    member pda.q0 : 'Q =
        match pda with
        | PushdownAutomaton(_, q0, _, _) -> q0

    member pda.Z0 : 'Γ =
        match pda with
        | PushdownAutomaton(_, _, Z0, _) -> Z0

    member pda.F : Set<'Q> =
        match pda with
        | PushdownAutomaton(_, _, _, F) -> F

module PushdownAutomaton =
    let (|PDA|) (pda : PushdownAutomaton<'Q, 'Σ, 'Γ>) =
        match pda with
        | PushdownAutomaton (δ, q0, Z0, F) -> (pda.Q, pda.Σ, pda.Γ, δ, q0, Z0, F)

    /// Convert a context-free grammar to a pushdown automaton as per slide 75.
    /// TODO: tests!
    let ofCFG : ContextFreeGrammar<'nt, 't> -> PushdownAutomaton<unit, 't, Symbol<'nt, 't>> =
        function
        | CFG (V, T, P, S) ->
            // Our only state.
            let q : unit = ()

            let δ : Transition<unit, 't, Symbol<'nt, 't>> =
                // The definition of δ for nonterminals:
                let δ1 = seq {
                    for ProductionRule(A, β) in P do
                        yield ((q, None, Nonterminal A), Set.singleton (q, β))
                }
                // The definition of δ for terminals:
                let δ2 = seq {
                    for t in T do
                        yield ((q, Some t, Terminal t), Set.singleton (q, []))
                }
                // δ is their union.
                Map.ofSeq (Seq.append δ1 δ2)

            PushdownAutomaton (δ, q, Nonterminal S, Set.empty)

    /// Convert a pushdown automaton that accepts L on an empty stack
    /// to one that accepts L in its final states. (Slide 67)
    /// TODO: tests!
    let emptyStackToFinalState (pda : PushdownAutomaton<'Q, 'Σ, 'Γ>) : PushdownAutomaton<'Q option option, 'Σ, 'Γ option> =
        match pda with
        | PushdownAutomaton (δN, q0, Z0, F) ->
            // New states and stack symbols.
            let p0  : 'Q option option = None
            let pf  : 'Q option option = Some None
            let q0' : 'Q option option = Some (Some q0)
            let X0  : 'Γ option = None
            let Z0' : 'Γ option = Some Z0
        
            // The first transition.
            let δ1 = Seq.singleton ((p0, None, X0), Set.singleton (q0', [Z0'; X0]))

            // The old transitions, wrapped.
            let δ2 = seq {
                for KeyValue((q, a, Y), v) in δN do
                    // Wrap the old state/symbol types.
                    let v' = v |> Set.map (fun (p, γ) ->
                                 (Some (Some p), List.map Some γ))
                    yield ((Some (Some q), a, Some Y), v')
            }

            // Additional arrows from (q, ε, X0) to pf.
            let additional = Map.ofSeq (seq {
                for q in pda.Q ->
                    ((Some (Some q), None, X0), Set.singleton (pf, []))
            })

            // Bundle them all and create a PDA.
            let δF = Map.ofSeq (Seq.append δ1 δ2)
                     |> MapHelpers.mergeWith Set.union additional

            PushdownAutomaton (δF, p0, X0, Set.singleton pf)

    /// Convert a pushdown automaton that accepts L in its final states
    /// to one that accepts L on an empty stack. (Slide 71)
    /// TODO: tests!
    let finalStateToEmptyStack (pda : PushdownAutomaton<'Q, 'Σ, 'Γ>) : PushdownAutomaton<'Q option option, 'Σ, 'Γ option> =
        match pda with
        | PushdownAutomaton (δF, q0, Z0, F) ->
            // New states and stack symbols.
            let p0  : 'Q option option = None
            let p   : 'Q option option = Some None
            let q0' : 'Q option option = Some (Some q0)
            let X0  : 'Γ option = None
            let Z0' : 'Γ option = Some Z0
        
            // The augmented set of stackSymbols: Γ ∪ {X0}.
            let stackSymbols = Set.map Some pda.Γ |> Set.add X0

            // The first transition.
            let δ1 = Seq.singleton ((p0, None, X0), Set.singleton (q0', [Z0'; X0]))

            // The p transitions.
            let δ2 = seq {
                for Y in stackSymbols do
                    yield ((p, None, Y), Set.singleton (p, []))
            }

            // The old transitions, wrapped.
            let δ3 = seq {
                for KeyValue((q, a, Y), v) in δF do
                    // Wrap the old state/symbol types.
                    let v' = v |> Set.map (fun (p, γ) ->
                                 (Some (Some p), List.map Some γ))
                    yield ((Some (Some q), a, Some Y), v')
            }

            // Additional arrows from (q, ε, X0) to pf.
            let additional = Map.ofSeq (seq {
                for q in F do
                    for Y in stackSymbols do
                        yield ((Some (Some q), None, Y), Set.singleton (p, []))
            })

            // Bundle them all and create a PDA.
            let δF = Map.ofSeq (Seq.concat [δ1; δ2; δ3])
                     |> MapHelpers.mergeWith Set.union additional

            PushdownAutomaton (δF, p0, X0, Set.empty)