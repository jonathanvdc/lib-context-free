namespace libcontextfree

/// A production rule in a fully CNF-converted context-free grammar.
///
/// The right-hand side of a production rule in a CFG in Chomsky normal form
/// may never contain the starting symbol. To enforce this, our alphabet of
/// nonterminals is the type `'nt option`, and the starting symbol S0 is
/// encoded as `None`. The right hand sides are then of the type `('nt * 'nt)`
/// instead of `('nt option * 'nt option).`
///
/// When implementing the algorithms, translate
///
///   * "a nonterminal V"               to     'nt option
///   * "a nonterminal V ... where      
///      V is not the start symbol"     to     'nt
///
type ChomskyNormalRule<'nt, 't> =
    | BinaryRule of 'nt option * ('nt * 'nt)   // A → BC
    | TerminalRule of 'nt option * 't          // A → a
    | StartToEpsilon                           // S → ε

/// A fully CNF-converted context-free grammar.
///
type ChomskyNormalCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | ChomskyNormalCFG of Set<ChomskyNormalRule<'nt, 't>>

module ChomskyNormalForm =
    ///////////////
    // CNF types //
    ///////////////

    /// A production rule, after the START step in the CNF conversion.
    type StartRule<'nt, 't> =
    | SProductionRule of 'nt option * Symbol<'nt, 't> list

    /// A context-free grammar, after the START step in the CNF conversion.
    type StartCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | StartCFG of Set<StartRule<'nt, 't>>

    /// A production rule, after the TERM step in the CNF conversion.
    type TermRule<'nt, 't> =
    | TNonterminalRule of 'nt option * 'nt list
    | TTerminalRule of 'nt * 't

    /// A context-free grammar, after the TERM step in the CNF conversion.
    type TermCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | TermCFG of Set<TermRule<'nt, 't>>

    /// A production rule, after the BIN step in the CNF conversion.
    type BinRule<'nt, 't> =
    | BBinaryRule of 'nt * ('nt * 'nt)
    | BUnitRule of 'nt option * 'nt
    | BTerminalRule of 'nt * 't
    | BEpsilonRule of 'nt option

    /// A context-free grammar, after the BIN step in the CNF conversion.
    type BinCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | BinCFG of Set<BinRule<'nt, 't>>

    /// A production rule, after the DEL step in the CNF conversion.
    type DelRule<'nt, 't> =
    | DBinaryRule of 'nt * ('nt * 'nt)
    | DUnitRule of 'nt option * 'nt
    | DTerminalRule of 'nt * 't
    | DStartToEpsilon

    /// A context-free grammar, after the DEL step in the CNF conversion.
    type DelCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | DelCFG of Set<DelRule<'nt, 't>>

    //////////////////////////////
    // CNF transformation steps //
    //////////////////////////////

    /// Step one in the CNF transformation: add a new start symbol.
    /// (The new start symbol is None; S is no longer the start symbol.)
    let startStep : ContextFreeGrammar<'nt, 't> -> StartCFG<'nt, 't> =
        function
        | ContextFreeGrammar(P, S) ->
            let convertRule (ProductionRule(head, body)) =
                SProductionRule(Some head, body)

            let P' = Set.map convertRule P
                  |> Set.add (SProductionRule (None, [Nonterminal S]))
            StartCFG P'

    /// Step two in the CNF transformation: split into terminal and nonterminal rules.
    /// The resulting nonterminal type is Symbol<'nt, 't>, where
    ///
    ///     * Nonterminal nt   corresponds to a nonterminal from the old 'nt type
    ///     * Terminal t       corresponds to the terminals N_t introduced in this step.
    ///
    let termStep : StartCFG<'nt, 't> -> TermCFG<Symbol<'nt, 't>, 't> =
        function
        | StartCFG P ->
            // Wrap the original rules in a `Nonterminal`.
            let convertRule (SProductionRule(head, body)) =
                TNonterminalRule(Option.map Nonterminal head, body)

            let P1 : Set<TermRule<Symbol<'nt, 't>, 't>> =
                Set.map convertRule P
            
            // Build all the rules N_t → t. (To do this, we must first extract
            // the terminals from the CFG.)
            let terminals : Set<'t> =
                P |> Set.map (fun (SProductionRule(_, str)) ->
                                  Set.ofSeq (Symbol.terminals str))
                  |> Set.unionMany

            let P2 : Set<TermRule<Symbol<'nt, 't>, 't>> =
                Set.map (fun t -> TTerminalRule (Terminal t, t)) terminals

            // Join them to make a TermCFG.
            TermCFG (Set.union P1 P2)

    /// Step three in the CNF transformation: make rules binary.
    /// The new nonterminal type has an additional "index", letting us
    /// turn a nonterminal N into many nonterminals (N,0), (N,1), ...
    let binStep : TermCFG<'nt, 't> -> BinCFG<'nt * int, 't> =
        function
        | TermCFG P ->
            let convertRule : TermRule<'nt, 't> -> Set<BinRule<'nt * int, 't>> =
                function
                | TNonterminalRule(Some H, body) ->
                    // H → X1 X2 X3 ... Xn
                    let rec toBin (i : int) =
                        function
                        | []      -> Set.singleton <| BEpsilonRule (Some (H,0))
                        | [X]     -> Set.singleton <| BUnitRule (Some (H,i), (X,0))
                        | [X; Y]  -> Set.singleton <| BBinaryRule ((H,i), ((X,0), (Y,0)))
                        | X :: Xs -> Set.add (BBinaryRule ((H,i), ((X,0), (H,i+1)))) (toBin (i+1) Xs)
                    toBin 0 body

                | TNonterminalRule(None, [S]) ->
                    // S0 → S
                    Set.singleton <| BUnitRule(None, (S, 0))

                | TTerminalRule(H, t) ->
                    // H → t
                    Set.singleton (BTerminalRule((H, 0), t))

                | TNonterminalRule(None, _) ->
                    // We shouldn't have ever created a rule like this: S0 → S is
                    // the *only* rule on S0.
                    raise (new System.InvalidOperationException())

            let P' = Set.unionMany (Set.map convertRule P)
            BinCFG P'

    /// Step four in the CNF transformation: delete ε-rules.
    /// First, we find all the nullable states using a breadth-first
    /// search. Then, we "inline" the found ε-rules wherever they occur
    /// in other rules' bodies.
    let delStep : BinCFG<'nt, 't> -> DelCFG<'nt, 't> =
        function
        | BinCFG P ->
            let baseNullableFromRule : BinRule<'nt, 't> -> 'nt option =
                function | BEpsilonRule(Some H) -> Some H
                         | _ -> None

            // All nonterminals H for which a rule H → ε exists.
            let baseNullableNonterminals : Set<'nt> =
                SetHelpers.choose baseNullableFromRule P
            
            // Find further nonterminals that are nullable, based on a set of
            // already known nullable nonterminals. Recurses with the resulting
            // set until no more new result are found.
            let rec searchNullables (current : Set<'nt>) =
                let inline isNullable (nt : 'nt) : bool =
                    Set.contains nt current

                // Yield the head of the given rule if we can induce it to be nullable.
                let inducedNullableFromRule : BinRule<'nt, 't> -> 'nt option =
                    function
                    | BBinaryRule(h, (x, y)) when isNullable x && isNullable y -> Some h
                    | BUnitRule(Some h, x) when isNullable x -> Some h
                    | _ -> None

                // Get all *new* nonterminals we can induce from here.
                let newNullables =
                    SetHelpers.choose inducedNullableFromRule P - current
                
                // Recurse until we no longer find new results.
                if Set.isEmpty newNullables
                    then current
                    else searchNullables (Set.union newNullables current)
            
            // Get all nullable nonterminals, using our base case from earlier.
            let nullableNonterminals : Set<'nt> =
                Set.ofSeq (searchNullables baseNullableNonterminals)
            let inline isNullable (nt : 'nt) : bool =
                Set.contains nt nullableNonterminals
                
            // Convert the given rule to a set of DelRules by inlining .
            let convertRule : BinRule<'nt, 't> -> Set<DelRule<'nt, 't>> =
                function
                | BBinaryRule(h, (x, y)) ->
                    Set.singleton (DBinaryRule (h, (x, y)))
                    |> if isNullable x then Set.add (DUnitRule (Some h, y)) else id
                    |> if isNullable y then Set.add (DUnitRule (Some h, x)) else id
                | BEpsilonRule None   -> Set.singleton DStartToEpsilon
                | BEpsilonRule _      -> Set.empty
                | BTerminalRule(x, y) -> Set.singleton (DTerminalRule(x, y))
                | BUnitRule(x, y)     -> Set.singleton (DUnitRule(x, y))
            
            let P' = Set.unionMany (Set.map convertRule P)
            DelCFG P'

    /// Step five in the CNF transformation: delete unit rules.
    let unitStep : DelCFG<'nt, 't> -> ChomskyNormalCFG<'nt, 't> =
        (fun x -> raise (new System.NotImplementedException()))
        // function
        // | DelCFG P ->
        //     let rulesFrom (H : 'nt) =
        //         P |> Set.filter (function | DUnitRule(Some h, _) -> H = h
        //                                   | DBinaryRule(h, _)    -> H = h
        //                                   | DTerminalRule(h, _)  -> H = h
        //                                   | _                    -> false)
        //         
        //     let rec convertRule : DelRule<'nt, 't> -> Set<ChomskyNormalRule<'nt, 't>> =
        //         function
        //         | DUnitRule(h, x)        -> Set.unionMany (Set.map convertRule (rulesFrom x))
        //         | DBinaryRule(h, (x, y)) -> Set.singleton (BinaryRule(h, (x, y)))
        //         | DTerminalRule(h, t)    -> Set.singleton (TerminalRule(h, t))
        //         | DStartToEpsilon        -> Set.singleton (StartToEpsilon)
        // 
        //     let P' = Set.unionMany (Set.map convertRule P)
        //     ChomskyNormalCFG P'

    let chomskyNormalForm<'nt, 't when 'nt : comparison and 't : comparison>
            : ContextFreeGrammar<'nt, 't> -> ChomskyNormalCFG<Symbol<'nt, 't> * int, 't> =
        startStep >> termStep >> binStep >> delStep >> unitStep
