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
type ProductionRule<'Nonterminal, 'Terminal>(head : 'Nonterminal, body : Either<'Nonterminal, 'Terminal> list) =
    /// Gets this production rule's head, which is the
    /// nonterminal on the production rule's left-hand side.
    member this.Head = head
    /// Gets this production rule's body, which is the
    /// list of terminals and nonterminals on the
    /// production rule's right-hand side.
    member this.Body = body

    override this.ToString() =
        // Production rules with empty bodies
        // are really epsilon-rules.
        let bodyString = match body with
                         | [] -> "ε"
                         | _  -> body |> List.map string 
                                      |> List.fold (+) ""
        head.ToString() + " -> " + bodyString

