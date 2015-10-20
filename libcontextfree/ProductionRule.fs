namespace libcontextfree

/// Defines a production rule.
type ProductionRule<'nt, 't> =
    /// A production rule is a tuple of a nonterminal - the rule's head -
    /// and a list of nonterminals and terminals - the rule's body.
    | ProductionRule of 'nt * Symbol<'nt, 't> list

    override this.ToString() =
        match this with
        | ProductionRule(head, body) ->
            let showSymbol (sym : Symbol<'nt, 't>) : string =
                match sym with | Nonterminal nt -> nt.ToString()
                               | Terminal t -> t.ToString()
            
            let bodyString =
                match body with
                | [] -> "ε"
                | _  -> body |> List.map showSymbol
                             |> String.concat ""

            head.ToString() + " -> " + bodyString

module ProductionRules =
    /// Shows the given sequence of production rules as a numbered list.
    let show (rules : seq<ProductionRule<'nt, 't>>) =
        rules |> Seq.mapi (fun index item -> string (index + 1) + ". " + string item)
              |> StringHelpers.concatLines
