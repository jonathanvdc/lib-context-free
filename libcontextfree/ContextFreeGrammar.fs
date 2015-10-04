namespace libcontextfree

/// Defines a formal context-free grammar as a quadruple (V, T, P, S) of
/// nonterminals, terminals, production rules, and the starting symbol.
type ContextFreeGrammar<'nt, 't when 'nt : comparison and 't : comparison> =
    | ContextFreeGrammar of Set<'nt> * Set<'t> * Set<ProductionRule<'nt, 't>> * 'nt