namespace libcontextfree

/// Implements an Earley parser, as defined by:
/// https://en.wikipedia.org/wiki/Earley_parser
module EarleyParser =
    /// Defines a partially parsed production rule.
    type PartialRule<'nt, 't> =
        /// A partially parsed production rule is a tuple of a nonterminal - the rule's head -,
        /// a list of parsed terminal and nonterminal nodes, and a list of
        /// unparsed nonterminals and terminals.
        /// Note: the list of parsed terminal and nonterminal nodes is stored
        ///       *in reverse order* for performance reasons, and must later be
        ///       reversed again.
        | PartialRule of 'nt * ParseTree<'nt, 't> list * Symbol<'nt, 't> list
        

    /// Defines a state in the Earley parsing algorithm
    /// as:
    ///  * the production currently being matched (X → α β)
    ///  * our current position in that production (represented by the dot and encapsulated in the PartialRule type)
    ///  * the position i in the input at which the matching of this production began: 
    ///    the origin position
    type EarleyState<'nt, 't> = PartialRule<'nt, 't> * int

    /// Creates Earley states with partial rules whose left-hand side is 
    /// the given nonterminal, and which do not have any parsed children yet.
    /// Their origin index is equal to the state index.
    let createStates (grammar : ContextFreeGrammar<'nt, 't>) (stateIndex : int) (nonterm : 'nt) : Set<EarleyState<'nt, 't>> =
        grammar.P |> Set.filter (fun (ProductionRule(lhs, _)) -> lhs = nonterm)
                  |> Set.map (fun (ProductionRule(lhs, rhs)) -> PartialRule(lhs, [], rhs), stateIndex)

    /// Prediction: For every state in S(k) of the form (X → α • Y β, j) 
    /// (where j is the origin position as above), add (Y → • γ, k) to S(k) 
    /// for every production in the grammar with Y on the left-hand side (Y → γ).
    let predict (grammar : ContextFreeGrammar<'nt, 't>) (stateIndex : int) : EarleyState<'nt, 't> -> Set<EarleyState<'nt, 't>> option =
        function 
        | PartialRule(_, _, Nonterminal Y :: _), _ -> 
            Some (createStates grammar stateIndex Y)
        | _ -> 
            None

    /// Scanning: If a is the next symbol in the input stream, 
    /// for every state in S(k) of the form (X → α • a β, j), 
    /// add (X → α a • β, j) to S(k + 1).
    let scan (input : 't) : EarleyState<'nt, 't> -> EarleyState<'nt, 't> option =
        function
        | PartialRule(X, α, Terminal a :: β), j when a = input ->
            Some (PartialRule(X, TerminalLeaf a :: α, β), j)
        | _ -> 
            None

    /// Completion: For every state in S(k) of the form (X → γ •, j), 
    /// find states in S(j) of the form (Y → α • X β, i) and 
    /// add (Y → α X • β, i) to S(k).
    let complete (allStates : Set<EarleyState<'nt, 't>>[]) : EarleyState<'nt, 't> -> Set<EarleyState<'nt, 't>> option =
        function
        | PartialRule(X, γ, []), j -> 
            // Tries to complete the given state with the above state.
            let completeState : EarleyState<'nt, 't> -> EarleyState<'nt, 't> option =
                function
                | PartialRule(Y, α, Nonterminal nonterm :: β), i when nonterm = X ->
                    Some (PartialRule(Y, ProductionNode(X, List.rev γ) :: α, β), i)
                    // Note:                               ^~~~~~~~~~
                    // The list of parsed child nodes is reversed here, so we need only 
                    // explicitly reverse top-level node's children explicitly.
                | _ ->
                    None
                
            allStates.[j] |> Seq.choose completeState
                          |> Set.ofSeq
                          |> Some
        | _ -> 
            None

    /// Executes a "worklist" algorithm that operates on the given
    /// seed set. A single item is transformed into a set of new values by a function.
    /// Said function is never applied to the same value twice.
    let worklistSet (f : 'a -> Set<'a>) (seed : Set<'a>) : Set<'a> =
        let rec processWorklist (results : Set<'a>) (worklist : Set<'a>) : Set<'a> =
            if Set.isEmpty worklist then
                results
            else
                let firstElem = Seq.head worklist
                let newElems = f firstElem
                let newResults = Set.add firstElem results
                // TODO: maybe computing the set difference here
                //       is a little... slow. Perhaps there's a better way?
                let newWorklist = Set.difference (Set.union worklist newElems) newResults
                processWorklist newResults newWorklist

        processWorklist Set.empty seed

    /// Parses the given terminal string with the given grammar.
    /// All possible parse trees are returned.
    let parse (grammar : ContextFreeGrammar<'nt, 't>) (input : seq<'t>) : Set<ParseTree<'nt, 't>> =
        let inputSize = Seq.length input
        let setArray = Array.create (inputSize + 1) Set.empty
        // The parser is seeded with S(0) consisting of only the top-level rules.
        setArray.[0] <- createStates grammar 0 grammar.S

        for (i, c) in Seq.zip [0..inputSize] input do
            let processState state =
                match complete setArray state with
                | Some completed -> completed
                | None ->
                    match predict grammar i state with
                    | Some predicted -> predicted
                    | None ->
                        match scan c state with
                        | Some scanned ->
                            setArray.[i + 1] <- Set.add scanned setArray.[i + 1]
                        | None ->
                            raise (new System.InvalidOperationException("Something went wrong here."))
                        Set.empty

            setArray.[i] <- worklistSet processState setArray.[i]
        
        // Tries to interpret the given Earley state as a well-formed
        // parse tree in the language of the grammar. If that can't be
        // done, None is returned.
        let getResult : EarleyState<'nt, 't> -> ParseTree<'nt, 't> option =
            function
            | PartialRule(Y, α, []), 0 when Y = grammar.S ->
                // We have a winner!
                Some (ProductionNode(Y, List.rev α))
            | _ ->
                None

        setArray.[inputSize] |> Seq.choose getResult
                             |> Set.ofSeq