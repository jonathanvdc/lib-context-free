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
        | Terminal t     -> t.ToString()

module Symbol =
    let map (f : 'nt1 -> 'nt2) (g : 't1 -> 't2) (sym : Symbol<'nt1, 't1>) : Symbol<'nt2, 't2> =
        match sym with
        | Nonterminal nt -> Nonterminal (f nt)
        | Terminal t     -> Terminal (g t)

    let nonterminals (syms : seq<Symbol<'nt, 't>>) : seq<'nt> =
        Seq.choose (function | Nonterminal nt -> Some nt | _ -> None) syms

    let terminals (syms : seq<Symbol<'nt, 't>>) : seq<'t> =
        Seq.choose (function | Terminal t -> Some t | _ -> None) syms