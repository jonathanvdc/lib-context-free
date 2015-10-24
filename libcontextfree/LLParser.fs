namespace libcontextfree

/// A terminal type for parsers that use lookahead,
/// such as LL(k) and LR(k) parsers.
type LTerminal<'t> = 
| LTerminal of 't
| EndOfInput

    override this.ToString() =
        match this with
        | LTerminal t -> t.ToString()
        | EndOfInput  -> "eof"

module LLParser =
    /// Peeks a terminal from the given token list.
    let peekTerminal (tokenType : 'token -> 'terminal) : 'token list -> LTerminal<'terminal> = function
    | t :: _ -> LTerminal (tokenType t)
    | []     -> EndOfInput

    /// Applies the LL(1) parsing algorithm to the given 
    /// expected symbol and list of tokens.
    let rec parse (tokenType : 'token -> 'terminal)
                  (parseTable : 'nt -> LTerminal<'terminal> -> Symbol<'nt, 'terminal> list option)
                  (currentSymbol : Symbol<'nt, 'terminal>)
                  (tokens : 'token list)
                  : ('token list * ParseTree<'nt, 'token>) option =
        let peek = peekTerminal tokenType tokens
        match currentSymbol with
        | Nonterminal nonterm -> 
            match parseTable nonterm peek with
            | Some body -> 
                let foldSymbol (state : ('token list * ParseTree<'nt, 'token> list) option) 
                               (sym : Symbol<'nt, 'terminal>) 
                               : ('token list * ParseTree<'nt, 'token> list) option =
                    match state with
                    | Some(tokens, trees) ->
                        match parse tokenType parseTable sym tokens with
                        | Some(ts, node) ->
                            Some (ts, node :: trees)
                        | None ->
                            None
                    | None -> None

                match List.fold foldSymbol (Some (tokens, [])) body with
                | Some(tokens, trees) -> Some(tokens, ProductionNode(nonterm, List.rev trees))
                | None -> None
            | None -> 
                // Unexpected nonterminal/terminal combination.
                None
        | Terminal term -> 
            match tokens with
            | t :: ts when tokenType t = term ->
                Some (ts, TerminalLeaf t)
            | _ ->
                // Unexpected terminal type.
                None

    /// Computes the FIRST set for the given terminal/nonterminal string.
    let rec first (firstMap : Map<'nt, Set<'t option>>) : Symbol<'nt, 't> list -> Set<'t option> = function
    | Terminal a :: _ -> Set.singleton (Some a)
    | [] -> Set.singleton None
    | Nonterminal A :: rest ->
        let followA = Map.find A firstMap
        if Set.contains None followA then
            Set.union (Set.remove None followA) (first firstMap rest)
        else
            followA

    /// Computes a map that maps every nonterminal A in the given grammar
    /// to their FIRST(A) set.
    /// FIRST(A) is the set of terminals which can appear as the first element
    /// of any chain of rules matching nonterminal A.
    ///
    /// FIRST(A) is used when building LL(1) and LR(1) tables.
    let firstSets (grammar : ContextFreeGrammar<'nt, 't>) : Map<'nt, Set<'t option>> =
        let addOne k v map =
            Map.add k (Set.add v (Map.find k map)) map

        let addMany k vs map =
            Map.add k (Set.union vs (Map.find k map)) map

        let innerFirst (results : Map<'nt, Set<'t option>>) =
            let foldRule results = function
            | ProductionRule(head, body) ->
                addMany head (first results body) results

            grammar.P |> Set.fold foldRule results

        MapHelpers.emptySetMap grammar.V |> FunctionHelpers.fix innerFirst

    /// Computes a map that maps every nonterminal A in the given grammar to
    /// its follow set FOLLOW(A).
    let followSets (first : Symbol<'nt, 't> list -> Set<'t option>) (grammar : ContextFreeGrammar<'nt, 't>) : Map<'nt, Set<LTerminal<'t>>> =
        let pickNonterminals = function
        | Nonterminal x, xs -> Some (x, xs)
        | _ -> None

        let innerFollow (results : Map<'nt, Set<LTerminal<'t>>>) =
            let foldRule results (ProductionRule(head, body)) =
                let updateMap (results : Map<'nt, Set<LTerminal<'t>>>) (nt, w') = 
                    let bodyFirst = first w'
                    let outputSet = Set.union (bodyFirst |> SetHelpers.choose id |> Set.map LTerminal) (Map.find nt results)
                    let outputSet = 
                        if Set.contains None bodyFirst || List.isEmpty w' then
                            Set.union (Map.find head results) outputSet
                        else
                            outputSet
                    Map.add nt outputSet results
                    
                body |> ListHelpers.suffixes
                     |> List.choose pickNonterminals
                     |> List.fold updateMap results
            Set.fold foldRule results grammar.P

        MapHelpers.emptySetMap grammar.V |> Map.add grammar.S (Set.singleton EndOfInput)
                                         |> FunctionHelpers.fix innerFollow


    /// Creates an LL(1) table from the given grammar.
    let createLLTable (grammar : ContextFreeGrammar<'nt, 't>)
                      : Result<Map<'nt * LTerminal<'t>, Symbol<'nt, 't> list>> =

        let first = FunctionHelpers.memoize (first (firstSets grammar))
        let follow = FunctionHelpers.ofMapWithDefault Set.empty (followSets first grammar)

        let foldCell (state : Result<Map<'nt * LTerminal<'t>, Symbol<'nt, 't> list>>) 
                     (a : LTerminal<'t>, (ProductionRule(A, ruleBody) as rule))
                     : Result<Map<'nt * LTerminal<'t>, Symbol<'nt, 't> list>> =
            match state with
            | Success map ->
                let firstSet = first ruleBody

                match a with 
                | LTerminal a ->
                    match Set.contains (Some a) firstSet, Set.contains None firstSet with
                    | true, true -> 
                        Error (sprintf "FIRST/FOLLOW conflict: caused by rule '%s' in cell ('%s', '%s')." (string rule) (A.ToString()) (a.ToString()))
                    | true, false -> 
                        // FIRST
                        match Map.tryFind (A, LTerminal a) map with
                        | Some oldRule -> Error (sprintf "FIRST/FIRST conflict: caused by rules '%s' and '%s' in cell ('%s', '%s')." (string rule) (string (ProductionRule(A, oldRule))) (A.ToString()) (a.ToString()))
                        | None -> Success (Map.add (A, LTerminal a) ruleBody map)
                    | false, true when Set.contains (LTerminal a) (follow A) ->
                        // FOLLOW
                        match Map.tryFind (A, LTerminal a) map with
                        | Some oldRule -> Error (sprintf "FIRST/FOLLOW conflict: caused by rules '%s' and '%s' in cell ('%s', '%s')." (string rule) (string (ProductionRule(A, oldRule))) (A.ToString()) (a.ToString()))
                        | None -> Success (Map.add (A, LTerminal a) ruleBody map)
                    | _ ->
                        // Do nothing
                        Success map
                | EndOfInput ->
                    if Set.contains None firstSet && Set.contains EndOfInput (follow A) then
                        // FOLLOW
                        match Map.tryFind (A, EndOfInput) map with
                        | Some oldRule -> Error (sprintf "FIRST/FOLLOW conflict: caused by rules '%s' and '%s' in cell ('%s', '%s')." (string rule) (string (ProductionRule(A, oldRule))) (A.ToString()) (string a))
                        | None -> Success (Map.add (A, EndOfInput) ruleBody map)
                    else
                        // Do nothing
                        Success map
            | Error e -> Error e

        let allTerminals = grammar.T |> Set.map LTerminal
                                     |> Set.add EndOfInput

        ListHelpers.cartesianProduct allTerminals grammar.P |> Seq.fold foldCell (Success Map.empty)

    /// Creates a string representation for the given LL(1) table.
    let showLL (table : Map<'nt * LTerminal<'t>, Symbol<'nt, 't> list>) =
        let allTerms = table |> Seq.map (fun (KeyValue((_, t), body)) -> Symbol.terminals body |> Set.ofSeq
                                                                                               |> Set.map LTerminal
                                                                                               |> Set.add t)
                             |> Set.unionMany
        let allNonterms = table |> Seq.map (fun (KeyValue((nt, _), body)) -> Symbol.nonterminals body |> Set.ofSeq 
                                                                                                      |> Set.add nt)
                                |> Set.unionMany

        let printTerm = string
        let printNonterm x = x.ToString()
        let printRuleBody body = body |> Seq.map string 
                                      |> String.concat ""

        MapHelpers.showTable printNonterm printTerm printRuleBody allNonterms allTerms table