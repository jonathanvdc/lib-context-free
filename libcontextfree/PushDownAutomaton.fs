namespace libcontextfree

/// The type of the transition map δ in a PushDownAutomaton<'Q, 'Σ, 'Γ>.
type Transition<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    Map<'Q * 'Σ option * 'Γ, Set<'Q * 'Γ list>>

/// Defines a formal pushdown automaton as a 7-tuple (Q, Σ, Γ, δ, q0, Z0, F), where:
///
///   * Q is the set of states.            : Set<'Q>
///   * Σ is the set of input symbols.     : Set<'Σ>
///   * Γ is the set of stack symbols.     : Set<'Γ>
///   * δ is the transition map.           : Transition<'Q, 'Σ, 'Γ>
///   * q0 is the initial state.           : 'Q
///   * Z0 is the initial stack symbol.    : 'Γ
///   * F is the set of accepting states.  : Set<'Q>
///
type PushDownAutomaton<'Q, 'Σ, 'Γ when 'Q : comparison and 'Σ : comparison and 'Γ : comparison> =
    | PushDownAutomaton of Set<'Q> * Set<'Σ> * Set<'Γ> * Transition<'Q, 'Σ, 'Γ> * 'Q * 'Γ * Set<'Q>

exception StackUnderflow

module PushDownAutomaton =
    /// Run a PDA non-deterministically, and return the final set of (state, stack) pairs.
    let runPDA (pda : PushDownAutomaton<'Q, 'Σ, 'Γ>) (input : 'Σ list) : Set<'Q * 'Γ list> =
        // A working state is an ECLOSE'd set of (state, stack) pairs, plus the current word.
        // To advance a step, we munch off a letter and follow transitions, then ECLOSE again.
        match pda with
        | PushDownAutomaton (Q, Σ, Γ, δ, q0, Z0, F) ->
            let step (symbol : 'Σ option) (ps : Set<'Q * 'Γ list>) : Set<'Q * 'Γ list> =
                ps |> Set.map (fun (q, stack) ->
                    match stack with
                    | [] -> raise StackUnderflow
                    | X::β ->
                        let r = Option.get (δ.TryFind (q, symbol, X))
                        Set.map (fun (p, α) -> (p, α @ β)) r
                ) |> Set.unionMany

            let eclose (ps : Set<'Q * 'Γ list>) : Set<'Q * 'Γ list> =
                step None ps

            let rec go (word : 'Σ list) (ps : Set<'Q * 'Γ list>) : Set<'Q * 'Γ list> =
                match word with
                | [] -> ps
                | (a::w) ->
                    go w (step (Some a) ps) |> eclose

            go input (Set.singleton (q0, [Z0]))

    let reachesFinalState (pda : PushDownAutomaton<'Q, 'Σ, 'Γ>) (input : 'Σ list) : bool =
        match pda with
        | PushDownAutomaton (Q, Σ, Γ, δ, q0, Z0, F) ->
            let result = runPDA pda input
            Set.exists (fun (q, _) -> Set.contains q F) result

    let reachesEmptyStack (pda : PushDownAutomaton<'Q, 'Σ, 'Γ>) (input : 'Σ list) : bool =
        match pda with
        | PushDownAutomaton (Q, Σ, Γ, δ, q0, Z0, F) ->
            let result = runPDA pda input
            Set.exists (fun (_, s) -> List.isEmpty s) result