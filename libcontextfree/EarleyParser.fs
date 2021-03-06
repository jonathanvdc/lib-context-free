﻿namespace libcontextfree

/// Implements an Earley parser, as defined by:
/// https://en.wikipedia.org/wiki/Earley_parser
module EarleyParser =
    /// Defines a partially parsed production rule.
    type PartialRule<'nt, 'token, 'terminal> =
        /// A partially parsed production rule is a tuple of a nonterminal - the rule's head -,
        /// a list of parsed token and nonterminal nodes, and a list of
        /// unparsed nonterminals and terminals.
        /// Note: the list of parsed terminal and nonterminal nodes is stored
        ///       *in reverse order* for performance reasons, and must later be
        ///       reversed again.
        | PartialRule of 'nt * ParseTree<'nt, 'token> list * Symbol<'nt, 'terminal> list
        

    /// Defines a state in the Earley parsing algorithm
    /// as:
    ///  * the production currently being matched (X → α β)
    ///  * our current position in that production (represented by the dot and encapsulated in the PartialRule type)
    ///  * the position i in the input at which the matching of this production began: 
    ///    the origin position
    type EarleyState<'nt, 'token, 'terminal> = PartialRule<'nt, 'token, 'terminal> * int

    /// Creates Earley states with partial rules whose left-hand side is 
    /// the given nonterminal, and which do not have any parsed children yet.
    /// Their origin index is equal to the state index.
    let createStates (grammar : ContextFreeGrammar<'nt, 'terminal>) 
                     (stateIndex : int) 
                     (nonterm : 'nt) 
                     : Set<EarleyState<'nt, 'token, 'terminal>> =
        grammar.P |> Set.filter (fun (ProductionRule(lhs, _)) -> lhs = nonterm)
                  |> Set.map (fun (ProductionRule(lhs, rhs)) -> PartialRule(lhs, [], rhs), stateIndex)

    /// Prediction: For every state in S(k) of the form (X → α • Y β, j) 
    /// (where j is the origin position as above), add (Y → • γ, k) to S(k) 
    /// for every production in the grammar with Y on the left-hand side (Y → γ).
    let predict (grammar : ContextFreeGrammar<'nt, 'terminal>) 
                (stateIndex : int) 
                : EarleyState<'nt, 'token, 'terminal> -> Set<EarleyState<'nt, 'token, 'terminal>> option =
        function 
        | PartialRule(_, _, Nonterminal Y :: _), _ -> 
            Some (createStates grammar stateIndex Y)
        | _ -> 
            None

    /// Scanning: If a is the next symbol in the input stream, 
    /// for every state in S(k) of the form (X → α • a β, j), 
    /// add (X → α a • β, j) to S(k + 1).
    let scan (input : 'token, inputTokenType : 'terminal) : EarleyState<'nt, 'token, 'terminal> -> EarleyState<'nt, 'token, 'terminal> option =
        function
        | PartialRule(X, α, Terminal a :: β), j when a = inputTokenType ->
            Some (PartialRule(X, TerminalLeaf input :: α, β), j)
        | _ -> 
            None

    /// Completion: For every state in S(k) of the form (X → γ •, j), 
    /// find states in S(j) of the form (Y → α • X β, i) and 
    /// add (Y → α X • β, i) to S(k).
    let complete (getState : int -> Set<EarleyState<'nt, 'token, 'terminal>>) 
                 : EarleyState<'nt, 'token, 'terminal> -> Set<EarleyState<'nt, 'token, 'terminal>> option =
        function
        | PartialRule(X, γ, []), j -> 
            // Tries to complete the given state with the above state.
            let completeState : EarleyState<'nt, 'token, 'terminal> -> EarleyState<'nt, 'token, 'terminal> option =
                function
                | PartialRule(Y, α, Nonterminal nonterm :: β), i when nonterm = X ->
                    Some (PartialRule(Y, ProductionNode(X, List.rev γ) :: α, β), i)
                    // Note:                               ^~~~~~~~~~
                    // The list of parsed child nodes is reversed here, so we need only 
                    // explicitly reverse top-level node's children explicitly.
                | _ ->
                    None
                
            getState j |> Seq.choose completeState
                       |> Set.ofSeq
                       |> Some
        | _ -> 
            None

    /// Executes a "worklist" algorithm that operates on the given
    /// seed set. A single item is transformed into a set of new values by a function.
    /// Said function is never applied to the same value twice.
    let worklistSet (f : (unit -> Set<'a>) -> 'a -> Set<'a>) (seed : Set<'a>) : Set<'a> =
        let rec processWorklist (results : Set<'a>) (worklist : Set<'a>) : Set<'a> =
            if Set.isEmpty worklist then
                results
            else
                let firstElem = Seq.head worklist
                let newElems = f (fun () -> Set.union results worklist) firstElem
                let newResults = Set.add firstElem results
                // TODO: maybe computing the set difference here
                //       is a little... slow. Perhaps there's a better way?
                let newWorklist = Set.difference (Set.union worklist newElems) newResults
                processWorklist newResults newWorklist

        processWorklist Set.empty seed

    /// Parses the given terminal string with the given grammar.
    /// All possible parse trees are returned.
    let parse (tokenType : 'token -> 'terminal) 
              (grammar : ContextFreeGrammar<'nt, 'terminal>) 
              (input : seq<'token>) 
              : Set<ParseTree<'nt, 'token>> =
        let inputSize = Seq.length input
        let setArray = Array.create (inputSize + 1) Set.empty
        // The parser is seeded with S(0) consisting of only the top-level rules.
        setArray.[0] <- createStates grammar 0 grammar.S

        // Performs a single complete/predict step on the ith set.
        let performStep i getCurrentSet state =
            let getSet j = 
                if j = i then
                    getCurrentSet()
                else
                    setArray.[j]

            match complete getSet state with
            | Some completed -> Some completed
            | None ->
                match predict grammar i state with
                | Some predicted -> Some predicted
                | None -> None

        for (i, c) in Seq.mapi (fun a b -> a, b) input do
            let processState getCurrentSet state =
                match performStep i getCurrentSet state with
                | Some results -> results
                | None ->
                    match scan (c, tokenType c) state with
                    | Some scanned ->
                        setArray.[i + 1] <- Set.add scanned setArray.[i + 1]
                    | None ->
                        ()
                    Set.empty

            setArray.[i] <- worklistSet processState setArray.[i]


        // Performs the last step.
        let performFinalStep getCurrentSet state =
            match performStep inputSize getCurrentSet state with
            | Some results -> results
            | None -> Set.empty

        setArray.[inputSize] <- worklistSet performFinalStep setArray.[inputSize]
        
        // Tries to interpret the given Earley state as a well-formed
        // parse tree in the language of the grammar. If that can't be
        // done, None is returned.
        let getResult : EarleyState<'nt, 'token, 'terminal> -> ParseTree<'nt, 'token> option =
            function
            | PartialRule(Y, α, []), 0 when Y = grammar.S ->
                // We have a winner!
                Some (ProductionNode(Y, List.rev α))
            | _ ->
                None

        setArray.[inputSize] |> Seq.choose getResult
                             |> Set.ofSeq