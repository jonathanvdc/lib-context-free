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

module PushDownAutomaton =
    /// Return whether the PDA accepts the given string (via accepting states).
    let accepts (pda : PushDownAutomaton<'Q, 'Σ, 'Γ>) (w : 'Σ list) : bool =
        match pda with
        | PushDownAutomaton (Q, Σ, Γ, δ, q0, Z0, F) ->
            false   // TODO