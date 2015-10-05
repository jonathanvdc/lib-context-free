namespace libcontextfree

/// A generic union type which combines two types representing nonterminal and terminal symbols.
type Symbol<'nt, 't> =
    /// Defines a nonterminal symbol.
    | Nonterminal of 'nt
    /// Defines a terminal symbol.
    | Terminal of 't

    override this.ToString() =
        match this with
        | Nonterminal nt -> nt.ToString()
        | Terminal    t  -> t.ToString()