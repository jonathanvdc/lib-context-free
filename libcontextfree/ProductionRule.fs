namespace libcontextfree

/// Defines a type that is either a value of the
/// left-hand generic parameter or a value of
/// the right-hand generic parameter.
type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

    override this.ToString() =
        // Define the string representation of an either type
        // as the string representation of its contents.
        match this with
        | Left  x -> x.ToString()
        | Right x -> x.ToString()

/// Defines a production rule.
type ProductionRule<'Nonterminal, 'Terminal> =
    /// A production rule is a tuple of a nonterminal - the rule's head -
    /// and a list of nonterminals and terminals - the rule's body.
    | ProductionRule of 'Nonterminal * Either<'Nonterminal, 'Terminal> list

    override this.ToString() =
        match this with
        | ProductionRule(head, body) ->
            // Production rules with empty bodies
            // are epsilon-rules.
            let bodyString = match body with
                             | [] -> "ε"
                             | _  -> body |> List.map string 
                                          |> List.fold (+) ""
            head.ToString() + " -> " + bodyString
