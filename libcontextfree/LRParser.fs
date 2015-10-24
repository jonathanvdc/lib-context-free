namespace libcontextfree

module LRParser =

    type LRAction<'nt, 't> =
    /// Indicates that the next state is n.
    | Shift of int
    /// Indicates that a reduction with the given grammar rule should be performed.
    | Reduce of ProductionRule<'nt, 't>
    /// Indicates that the LR parser has successfully parsed the input string.
    | Accept
    /// Indicates that the LR parser could not parse the input string.
    | Fail

    type ParserState<'nt, 't> = 't list * (ParseTree<'nt, LTerminal<'t>> * int) list

    /// Shift the matched terminal t onto the parse stack and scan the next input symbol into the lookahead buffer.
    /// Push next state n onto the parse stack as the new current state.
    let shift (n : int) (state : ParserState<'nt, 'token>) : ParserState<'nt, 'token> =
        match state with
        | t :: ts, stack -> 
            ts, (TerminalLeaf (LTerminal t), n) :: stack
        | [], stack ->
            [], (TerminalLeaf EndOfInput, n) :: stack

    /// Peeks a single item from the stack.
    let peekStack (startState : 'a) (stack : ('b * 'a) list) =
        match stack with
        | x :: _ -> snd x
        | []     -> startState

    /// Remove the matched topmost L symbols (and parse trees and associated state numbers) from the parse stack.
    /// This exposes a prior state p that was expecting an instance of the Lhs symbol.
    /// Join the L parse trees together as one parse tree with new root symbol Lhs.
    /// Lookup the next state n from row p and column Lhs of the LHS Goto table.
    /// Push the symbol and tree for Lhs onto the parse stack.
    /// Push next state n onto the parse stack as the new current state.
    /// The lookahead and input stream remain unchanged.
    let reduce (gotoTable : int -> 'nt -> int) 
               (startState : int)
               (rule : ProductionRule<'nt, 'terminal>) 
               (state : ParserState<'nt, 'token>) : ParserState<'nt, 'token> = 
        match rule, state with
        | ProductionRule(lhs, body), (ts, stack) ->
            let elems, remStack = ListHelpers.splitAtIndex (List.length body) stack
            let newTree = ProductionNode(lhs, List.rev (List.map fst elems))
            let p = peekStack startState remStack
            let n = gotoTable p lhs
            ts, (newTree, n) :: remStack

    /// "Normalizes" the given LR parser tree: end-of-input leaves are discarded recursively.
    let rec normalizeLRTree : ParseTree<'nt, LTerminal<'t>> -> ParseTree<'nt, 't> option = function
    | TerminalLeaf (LTerminal t) -> Some (TerminalLeaf t)
    | TerminalLeaf EndOfInput     -> None
    | ProductionNode(lhs, body)   ->
        Some (ProductionNode(lhs, List.choose normalizeLRTree body))
    
    /// Applies the LR parsing algorithm recursively to the given action table, goto table and state.
    /// A parse tree is returned if the parsing algorithm was successful. Otherwise,
    /// the list of remaining input terminals is returned (this may be useful for diagnostic purposes).
    let rec parseLR (tokenType : 'token -> 'terminal)
                    (actionTable : int -> LTerminal<'terminal> -> LRAction<'nt, 'terminal>)
                    (gotoTable : int -> 'nt -> int)
                    (startSymbol : 'nt, startState : int)
                    : ParserState<'nt, 'token> -> Choice<ParseTree<'nt, 'token>, 'token list> = function
    | input, stack ->
        let peek = LLParser.peekTerminal tokenType input
        let state = peekStack startState stack

        match actionTable state peek with
        | Shift n ->
            (input, stack) |> shift n 
                           |> parseLR tokenType actionTable gotoTable (startSymbol, startState)
        | Reduce rule ->
            (input, stack) |> reduce gotoTable startState rule
                           |> parseLR tokenType actionTable gotoTable (startSymbol, startState)
        | Accept ->
            let items = stack |> List.map fst
                              |> List.choose normalizeLRTree
                              |> List.rev

            Choice1Of2 (ProductionNode(startSymbol, items))
        | Fail ->
            Choice2Of2 input

    /// Parses a terminal string based on the given LR table components.
    let parse (tokenType : 'token -> 'terminal)
              (actionTable : int -> LTerminal<'terminal> -> LRAction<'nt, 'terminal>) 
              (gotoTable : int -> 'nt -> int)
              (startState : 'nt * int) 
              (input : 'token list) =
        parseLR tokenType actionTable gotoTable startState (input, [])


    /// Defines LR(0) items, which are grammar rules with a special dot added somewhere in the right-hand side. 
    /// The 'dot' is represented by storing the rule's body in two lists.
    ///
    /// For example the rule E → E + B has the following four corresponding items:
    ///     E → • E + B
    ///     E → E • + B
    ///     E → E + • B
    ///     E → E + B •
    ///
    /// Note: because of how lists work in F#, this body on the left-hand
    ///       side of the dot is stored in reverse order.
    type LRItem<'nt, 't> = 
        | LRItem of 'nt * Symbol<'nt, 't> list * Symbol<'nt, 't> list 

        /// Gets the production rule associated with this LR item.
        member this.Rule =
            match this with
            | LRItem(head, left, right) -> ProductionRule(head, List.append (List.rev left) right)

        /// Gets the production rule's head for this LR item's
        /// associated production rule.
        member this.Head =
            match this with
            | LRItem(x, _, _) -> x

        /// Gets the symbol directly after the dot.
        member this.NextSymbol =
            match this with
            | LRItem(_, _, x :: _) -> Some x
            | _                    -> None

        /// Gets the item obtained by moving
        /// the dot one symbol toward the right.
        member this.NextItem = 
            match this with
            | LRItem(head, ls, r :: rs) -> Some(LRItem(head, r :: ls, rs))
            | _                         -> None

        override this.ToString() =
            match this with
            | LRItem(head, left, right) ->
                let showList xs = 
                    xs |> Seq.map string
                       |> String.concat ""
                sprintf "%s -> %s . %s" (head.ToString()) (left |> List.rev |> showList) (showList right)

    /// An LR(0) item is defined as a pair of a a generic LR item
    /// and zero terminals of lookahead.
    type LR0Item<'nt, 't> = LRItem<'nt, 't> * unit

    /// An LR(1) item is defined as a pair of a generic LR item
    /// and a single terminal of lookahead.
    type LR1Item<'nt, 't> = LRItem<'nt, 't> * 't

    /// Any set of items can be extended by recursively adding all 
    /// the appropriate items until all nonterminals preceded by dots are accounted for. 
    /// The minimal extension is called the closure of an item set and written as closure(I) where I is an item set. 
    /// It is these closed item sets that are taken as the states of the parser, 
    /// although only the ones that are actually reachable from the begin state will be included in the tables.
    ///
    /// In this closure implementation, a context-free grammar and an item-creating function are passes as arguments:
    /// said function takes an old item and a rule for the next item, and uses them to generate a 
    /// set of new items.
    let rec closure (getLookahead : Set<LRItem<'nt, 't>> -> LRItem<'nt, 't> -> Set<'a>) 
                    (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LRItem<'nt, 't> * 'a>)
                    : Set<LRItem<'nt, 't> * 'a> = 
        let groupRules : ProductionRule<'nt, 't> -> 'nt * ProductionRule<'nt, 't> = function
        | ProductionRule(lhs, _) as rule -> lhs, rule

        let groupedRules = grammar.P |> Seq.map groupRules
                                     |> MapHelpers.groupFstSet

        let induction (lrItem : LRItem<'nt, 't>) =
            match lrItem.NextSymbol with
            | Some (Nonterminal nt) ->
                match Map.tryFind nt groupedRules with
                | Some nts ->
                    nts |> Set.map (fun (ProductionRule(head, body)) -> LRItem(head, [], body))
                | None ->
                    Set.empty
            | _ -> Set.empty

        let lookaheadMap = MapHelpers.groupFstSet basis
        let setClosure = basis |> Set.map fst 
                               |> SetHelpers.closure induction 

        let getOrDefault def = function
        | Some x -> x
        | None   -> def

        setClosure |> Set.map (fun x -> getLookahead setClosure x |> Set.union (Map.tryFind x lookaheadMap |> getOrDefault Set.empty) 
                                                                  |> Set.map (fun l -> x, l))
                   |> Set.unionMany

    /// Computes the set of states, given a closure and goto function and 
    /// an initial item.
    /// A state is nothing more than a set of items.
    let states (closure : Set<LRItem<'nt, 't> * 'a> -> Set<LRItem<'nt, 't> * 'a>)
               (goto : Set<LRItem<'nt, 't> * 'a> -> Symbol<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>)
               (initState : Set<LRItem<'nt, 't> * 'a>)
               : Set<Set<LRItem<'nt, 't> * 'a>> = 

        let induction (state : Set<LRItem<'nt, 't> * 'a>) : Set<Set<LRItem<'nt, 't> * 'a>> =
            let stateClosure = closure state

            stateClosure |> SetHelpers.choose (fun (s, _) -> s.NextSymbol)
                         |> Set.map (goto stateClosure)
                         |> Set.filter (not << Set.isEmpty)
            
        SetHelpers.closure induction (Set.singleton initState)

    /// Creates an action table from the given closure, goto and follow functions,
    /// the grammar's starting symbol, and the set of states.
    let actionTable (closure : Set<LRItem<'nt, 't> * 'a> -> Set<LRItem<'nt, 't> * 'a>)
                    (goto : Set<LRItem<'nt, 't> * 'a> -> Symbol<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>)
                    (follow : 'a -> Set<LTerminal<'t>>)
                    (startSymbol : 'nt)
                    (states : Map<Set<LRItem<'nt, 't> * 'a>, int>)
                    : Result<Map<int * LTerminal<'t>, LRAction<'nt, 't>>> =
        let mutable results = Success Map.empty
        for KeyValue(state, stateIndex) in states do
            let state = closure state
            for (item, lookahead) in state do
                match item.NextSymbol with
                | Some(Terminal t) -> 
                    // Try to shift.
                    let addShift dict =
                        let toIndex = Map.find (goto state (Terminal t)) states
                        let key = stateIndex, LTerminal t
                        match Map.tryFind key dict with
                        | None -> Success (Map.add key (Shift toIndex) dict)
                        | Some (Shift shiftTarget) when shiftTarget = toIndex -> Success dict
                        | Some (Reduce reduceTarget) -> Error (sprintf "Shift/reduce conflict: %A and %s on terminal '%O'." (state |> Set.map (fst >> string) |> Set.toList) (string reduceTarget) t)
                        | Some (Shift shiftTarget) ->
                            let otherState = Map.findKey (fun key value -> value = shiftTarget) states |> closure
                            Error (sprintf "Shift/shift conflict: %A and %A." (state |> Set.map (fst >> string) |> Set.toList) (otherState |> Set.map (fst >> string) |> Set.toList))
                        | Some _ -> Error (sprintf "Shift conflict: %A." (state |> Set.map (fst >> string) |> Set.toList))
                    results <- Result.bind addShift results
                | None ->
                    let addReduce symbol dict =
                        let key = stateIndex, symbol
                        match Map.tryFind key dict with
                        | None -> Success (Map.add key (Reduce item.Rule) dict)
                        | Some (Reduce reduceTarget) when reduceTarget = item.Rule -> Success dict
                        | Some Accept -> Success dict
                        | Some (Reduce reduceTarget) -> 
                            Error (sprintf "Reduce/reduce conflict: %s and %s on terminal '%O'." (string item.Rule) (string reduceTarget) symbol)
                        | Some (Shift shiftTarget) -> 
                            let state = Map.findKey (fun key value -> value = shiftTarget) states |> closure
                            Error (sprintf "Shift/reduce conflict: %A and %s on terminal '%O'." (state |> Set.map (fst >> string) |> Set.toList) (string item.Rule) symbol)
                        | _ -> 
                            // This shouldn't happen, as it would indicate a Reduce/Fail conflict, 
                            // which just doesn't make sense, because we're not generating any
                            // Fail actions here.
                            Error "Unexpected reduce conflict. Fascinating."

                    // Try to reduce.
                    for t in follow lookahead do
                        results <- Result.bind (addReduce t) results
                    if item.Head = startSymbol then
                        // Starting symbol. Our work here is done.
                        results <- Result.map (Map.add (stateIndex, EndOfInput) Accept) results
                | _ -> 
                    // Nothing to do here, really.
                    ()

        results

    /// Gets the action at the given state/terminal cell in the LR table.
    let getAction (table : Map<int * LTerminal<'t>, LRAction<'nt, 't>>) (state : int) (terminal : LTerminal<'t>) : LRAction<'nt, 't> =
        match Map.tryFind (state, terminal) table with
        | Some action -> action
        | None        -> Fail

    /// Creates a goto table from the given closure and goto functions, set of nonterminals,
    /// and set of states.
    let gotoTable (closure : Set<LRItem<'nt, 't> * 'a> -> Set<LRItem<'nt, 't> * 'a>)
                  (goto : Set<LRItem<'nt, 't> * 'a> -> Symbol<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>)
                  (nonterminals : Set<'nt>)
                  (states : Map<Set<LRItem<'nt, 't> * 'a>, int>)
                  : Map<int * 'nt, int> =

        let mutable results = Map.empty

        for KeyValue(state, stateIndex) in states do
            let stateClosure = closure state
            for nt in nonterminals do
                let nextState = goto stateClosure (Nonterminal nt)
                if not (Set.isEmpty nextState) then
                    let nextStateIndex = Map.find nextState states
                    results <- Map.add (stateIndex, nt) nextStateIndex results
        results

    /// Defines a goto function for LR(k) items: all items in the specified set whose
    /// next symbol is the given label are taken, and their dot is shifted one position
    /// to the right.
    let goto (from : Set<LRItem<'nt, 't> * 'a>) 
             (label : Symbol<'nt, 't>) 
             : Set<LRItem<'nt, 't> * 'a> =
        let snd (_, x, _) = x

        from |> SetHelpers.choose (fun (x, l) -> Option.map (fun y -> x, y, l) x.NextSymbol)
             |> Set.filter (snd >> ((=) label))
             |> SetHelpers.choose (fun (x, _, l) -> Option.map (fun x -> x, l) x.NextItem)

    /// A specialization of the closure function for LR(0) items.
    let closureLR0 (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LR0Item<'nt, 't>>) =
        let getLookahead _ _ = Set.singleton ()

        closure getLookahead grammar basis

    /// Given the precomputed map of FIRST sets, computes FOLLOW(k, B), where 
    /// k is a set of LR items, and B is a nonterminal.
    ///
    /// FOLLOW(I) of an Item I [A → α • B β, x] is the set of terminals that can appear 
    /// immediately after nonterminal B, where α, β are arbitrary symbol strings, 
    /// and x is an arbitrary lookahead terminal.
    ///
    /// FOLLOW(k, B) of an item set k and a nonterminal B is the union of the 
    /// follow sets of all items in k where '•' is followed by B.
    let follow (followMap : Map<'nt, Set<LTerminal<'t>>>) (k : Set<LRItem<'nt, 't>>) (B : 'nt) : Set<LTerminal<'t>> =
        let mapping = function
        | LRItem(_, _, Nonterminal nt :: _) when nt = B -> 
            Map.find B followMap
        | _ -> 
            Set.empty
        
        k |> Set.map mapping
          |> Set.unionMany

    /// A specialization of the closure function for LR(1) items.
    let closureLR1 (firstMap : Map<'nt, Set<'t option>>) (followMap : Map<'nt, Set<LTerminal<'t>>>) (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LRItem<'nt, 't> * LTerminal<'t>>) =
        let getLookahead (closureSet : Set<LRItem<'nt, 't>>) =
            let getFollowers = FunctionHelpers.memoize (follow followMap closureSet)
            fun (item : LRItem<'nt, 't>) -> 
                getFollowers item.Head

        closure getLookahead grammar basis

    /// Creates an LR(k) parser, which is a triple of an action table, a goto table,
    /// and an initial state. If this cannot be done, an error message is returned.
    let createLR (closure : Set<LRItem<'nt, 't> * 'a> -> Set<LRItem<'nt, 't> * 'a>)
                 (goto : Set<LRItem<'nt, 't> * 'a> -> Symbol<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>)
                 (follow : 'a -> Set<LTerminal<'t>>)
                 (initialLookahead : 'a)
                 (grammar : ContextFreeGrammar<'nt, 't>) =
        // Memoize these two
        let closure = FunctionHelpers.memoize closure
        let goto = FunctionHelpers.memoize2 goto

        let initState = grammar.P |> Set.filter (fun (ProductionRule(head, _)) -> head = grammar.S)
                                  |> Set.map (fun (ProductionRule(head, body)) -> let item = LRItem(head, [], body) in item, initialLookahead)
        let stateMap = states closure goto initState |> SetHelpers.toIndexedMap
        let actions = actionTable closure goto follow grammar.S stateMap
        let gotos = gotoTable closure goto grammar.V stateMap
        let initStateIndex = Map.find initState stateMap

        Result.map (fun actionMap -> actionMap, gotos, (grammar.S, initStateIndex)) actions

    /// Creates an LR(0) parser from the given grammar.
    /// If this cannot be done, an error message is returned.
    let createLR0 (grammar : ContextFreeGrammar<'nt, 't>) =
        let closure = closureLR0 grammar
        let follow _ = grammar.T |> Set.map LTerminal
                                 |> Set.add EndOfInput
        
        createLR closure goto follow () grammar

    /// Creates an LR(1) parser from the given grammar.
    /// If this cannot be done, an error message is returned.
    let createLR1 (grammar : ContextFreeGrammar<'nt, 't>) =
        let firstMap = LLParser.firstSets grammar
        let followMap = LLParser.followSets (LLParser.first firstMap) grammar

        let closure = closureLR1 firstMap followMap grammar
        
        createLR closure goto Set.singleton EndOfInput grammar

    type LRMapParser<'nt, 't when 'nt : comparison and 't : comparison> = 
        Map<int * LTerminal<'t>, LRAction<'nt, 't>> * Map<int * 'nt, int> * ('nt * int)

    type LRFunctionalParser<'nt, 't> = 
        (int -> LTerminal<'t> -> LRAction<'nt, 't>) * (int -> 'nt -> int) * ('nt * int)

    /// Converts the given parser, which has a
    /// map-based action and goto table, to a parser
    /// that uses a function-based action and goto table.
    let toFunctionalParser (actionTable : Map<int * LTerminal<'t>, LRAction<'nt, 't>>, gotoTable : Map<int * 'nt, int>, startState : 'nt * int) 
                           : LRFunctionalParser<'nt, 't> = 
        getAction actionTable, (fun i nt -> Map.find (i, nt) gotoTable), startState

    /// Prints an LR parser triple.
    let printLR (actionTable : Map<int * LTerminal<'t>, LRAction<'nt, 't>>, gotoTable : Map<int * 'nt, int>, (_ : 'nt, startState : int))
                : string =
        let actionStates = actionTable |> Seq.map (fun (KeyValue((i, _), _)) -> i)
                                       |> Set.ofSeq

        let gotoStates = gotoTable |> Seq.map (fun (KeyValue((i, _), _)) -> i)
                                   |> Set.ofSeq

        let allStates = Set.union actionStates gotoStates
        let allTerminals = actionTable |> Seq.map (fun (KeyValue((_, t), _)) -> t) 
                                       |> Set.ofSeq
        let allNonterminals = gotoTable |> Seq.map (fun (KeyValue((_, nt), _)) -> nt) 
                                        |> Set.ofSeq

        let getRule = function
        | Reduce rule -> Some rule
        | _           -> None

        let allRules = actionTable |> Seq.choose (fun (KeyValue((_, _), a)) -> getRule a)
                                   |> Set.ofSeq
                                   |> SetHelpers.toIndexedMap

        let printedRules = allRules |> Seq.sortBy (fun (KeyValue(_, i)) -> i)
                                    |> Seq.map (fun (KeyValue(r, i)) -> sprintf "%d. %s" i (string r))
                                    |> String.concat System.Environment.NewLine

        let showAction = function
        | Shift state -> string state
        | Reduce rule -> allRules |> Map.find rule
                                  |> string
                                  |> (+) "r"
        | Accept      -> "acc"
        | Fail        -> ""

        let actionTable = MapHelpers.showTable string string showAction allStates allTerminals actionTable
        let gotoTable = MapHelpers.showTable string (fun nt -> nt.ToString()) string allStates allNonterminals gotoTable

        ["Start state: " + string startState;
         "Rules:"; printedRules; "";
         "Action table:"; actionTable; "";
         "Goto table:"; gotoTable] 
         |> String.concat System.Environment.NewLine