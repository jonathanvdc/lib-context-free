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

    type LRTerminal<'t> = 
    | LRTerminal of 't
    | EndOfInput

        override this.ToString() =
            match this with
            | LRTerminal t -> t.ToString()
            | EndOfInput   -> "eof"

    type ParserState<'nt, 't> = 't list * (ParseTree<'nt, LRTerminal<'t>> * int) list

    /// Peeks a terminal from the given terminal list.
    let peekTerminal : 't list -> LRTerminal<'t> = function
    | t :: _ -> LRTerminal t
    | []     -> EndOfInput

    /// Shift the matched terminal t onto the parse stack and scan the next input symbol into the lookahead buffer.
    /// Push next state n onto the parse stack as the new current state.
    let shift (n : int) (state : ParserState<'nt, 't>) : ParserState<'nt, 't> =
        match state with
        | t :: ts, stack -> 
            ts, (TerminalLeaf (LRTerminal t), n) :: stack
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
               (rule : ProductionRule<'nt, 't>) 
               (state : ParserState<'nt, 't>) : ParserState<'nt, 't> = 
        match rule, state with
        | ProductionRule(lhs, body), (ts, stack) ->
            let elems, remStack = ListHelpers.splitAtIndex (List.length body) stack
            let newTree = ProductionNode(lhs, List.map fst elems)
            let p = peekStack startState remStack
            let n = gotoTable p lhs
            ts, (newTree, n) :: remStack

    /// "Normalizes" the given LR parser tree: end-of-input leaves are discarded recursively.
    let rec normalizeLRTree : ParseTree<'nt, LRTerminal<'t>> -> ParseTree<'nt, 't> option = function
    | TerminalLeaf (LRTerminal t) -> Some (TerminalLeaf t)
    | TerminalLeaf EndOfInput     -> None
    | ProductionNode(lhs, body)   ->
        Some (ProductionNode(lhs, List.choose normalizeLRTree body))
    
    /// Applies the LR parsing algorithm recursively to the given action table, goto table and state.
    /// A parse tree is returned if the parsing algorithm was successful. Otherwise,
    /// the list of remaining input terminals is returned (this may be useful for diagnostic purposes).
    let rec parseLR (actionTable : int -> LRTerminal<'t> -> LRAction<'nt, 't>)
                    (gotoTable : int -> 'nt -> int)
                    (startState : int)
                    : ParserState<'nt, 't> -> Choice<ParseTree<'nt, 't>, 't list> = function
    | input, stack ->
        let peek = peekTerminal input
        let state = peekStack startState stack

        match actionTable state peek with
        | Shift n ->
            (input, stack) |> shift n 
                           |> parseLR actionTable gotoTable startState
        | Reduce rule ->
            (input, stack) |> reduce gotoTable startState rule
                           |> parseLR actionTable gotoTable startState
        | Accept ->
            match normalizeLRTree (stack |> List.head |> fst) with
            | Some tree -> Choice1Of2 tree
            | None      -> Choice2Of2 input
        | Fail ->
            Choice2Of2 input

    /// Parses a terminal string based on the given LR table components.
    let parse (actionTable : int -> LRTerminal<'t> -> LRAction<'nt, 't>) 
              (gotoTable : int -> 'nt -> int)
              (startState : int) 
              (input : 't list) =
        parseLR actionTable gotoTable startState (input, [])


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
    let rec closure (createItem : LRItem<'nt, 't> * 'a -> ProductionRule<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>) 
                    (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LRItem<'nt, 't> * 'a>)
                    : Set<LRItem<'nt, 't> * 'a> = 
        let groupRules : ProductionRule<'nt, 't> -> 'nt * ProductionRule<'nt, 't> = function
        | ProductionRule(lhs, _) as rule -> lhs, rule

        let groupedRules = grammar.P |> Seq.map groupRules
                                     |> MapHelpers.groupFstSet

        let induction ((lrItem : LRItem<'nt, 't>, _) as input) =
            match lrItem.NextSymbol with
            | Some (Nonterminal nt) ->
                match Map.tryFind nt groupedRules with
                | Some nts ->
                    nts |> Set.map (createItem input)
                        |> Set.unionMany
                | None ->
                    Set.empty
            | _ -> Set.empty

        SetHelpers.closure induction basis

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
                    (follow : 'nt -> Set<'t>)
                    (startSymbol : 'nt)
                    (states : Map<Set<LRItem<'nt, 't> * 'a>, int>)
                    : Result<Map<int * LRTerminal<'t>, LRAction<'nt, 't>>> =
        let mutable results = Success Map.empty
        for KeyValue(state, stateIndex) in states do
            let closureState = closure state
            for (item, lookahead) in closureState do
                match item.NextSymbol with
                | Some(Terminal t) -> 
                    // Try to shift.
                    let addShift dict =
                        let toIndex = Map.find (goto closureState (Terminal t)) states
                        let key = stateIndex, LRTerminal t
                        match Map.tryFind key dict with
                        | None -> Success (Map.add key (Shift toIndex) dict)
                        | Some (Reduce reduceTarget) -> Error (sprintf "Shift/reduce conflict: %A and %s." (state |> Set.map (fst >> string) |> Set.toList) (string reduceTarget))
                        | Some _ -> Error (sprintf "Shift conflict: %A." (state |> Set.map (fst >> string) |> Set.toList))
                    results <- Result.bind addShift results
                | None ->
                    if item.Head = startSymbol then
                        // Starting symbol. Our work here is done.
                        results <- Result.map (Map.add (stateIndex, EndOfInput) Accept) results
                    else
                        let addReduce symbol dict =
                            let key = stateIndex, LRTerminal symbol
                            match Map.tryFind key dict with
                            | None -> Success (Map.add key (Reduce item.Rule) dict)
                            | Some (Reduce reduceTarget) -> 
                                Error (sprintf "Reduce/reduce conflict: %s and %s." (string item.Rule) (string reduceTarget))
                            | Some (Shift shiftTarget) -> 
                                let state = Map.findKey (fun key value -> value = shiftTarget) states
                                Error (sprintf "Shift/reduce conflict: %A and %s." (state |> Set.map (fst >> string) |> Set.toList) (string item.Rule))
                            | _ -> 
                                // This shouldn't happen, as it would indicate a Reduce/Accept conflict, 
                                // which just doesn't make sense.
                                Error "Unexpected reduce conflict. Fascinating."

                        // Try to reduce.
                        for t in follow item.Head do
                            results <- Result.bind (addReduce t) results
                | _ -> 
                    // Nothing to do here, really.
                    ()

        results

    /// Gets the action at the given state/terminal cell in the LR table.
    let getAction (table : Map<int * LRTerminal<'t>, LRAction<'nt, 't>>) (state : int) (terminal : LRTerminal<'t>) : LRAction<'nt, 't> =
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

    /// Defines a goto function for LR(0) items: all items in the specified set whose
    /// next symbol is the given label are taken, and their dot is shifted one position
    /// to the right.
    let gotoLR0 (from : Set<LR0Item<'nt, 't>>) (label : Symbol<'nt, 't>) : Set<LR0Item<'nt, 't>> =
        from |> SetHelpers.choose (fun (x, ()) -> Option.map (fun y -> x, y) x.NextSymbol)
             |> Set.filter (snd >> ((=) label))
             |> SetHelpers.choose (fun (x, _) -> x.NextItem)
             |> Set.map (fun x -> x, ())

    /// A specialization of the closure function for LR(0) items.
    let closureLR0 (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LR0Item<'nt, 't>>) =
        let createItem (oldItem, _) = function
        | ProductionRule(head, body) -> Set.singleton (LRItem(head, [], body), ())

        closure createItem grammar basis

    /// Computes the FIRST set for the given first symbol map and nonterminal.
    /// FIRST(A) is the set of terminals which can appear as the first element
    /// of any chain of rules matching nonterminal A.
    ///
    /// The given map associates every nonterminal with
    /// all first symbols that occur in rules that have said 
    /// nonterminal on their left-hand side.
    ///
    /// FIRST(A) is used when building LR(1) tables.
    let first (firstSymbols : Map<'nt, Set<Symbol<'nt, 't>>>) (sym : 'nt) : Set<'t> =
        let induction : Symbol<'nt, 't> -> Set<Symbol<'nt, 't>> = function
        | Nonterminal sym -> firstSymbols.[sym]
        | Terminal    _   -> Set.empty

        Nonterminal sym |> Set.singleton 
                        |> SetHelpers.closure induction
                        |> Symbol.terminals
                        |> Set.ofSeq

    /// Computes a map that maps every nonterminal A in the given grammar
    /// to their FIRST(A) set.
    let firstSets (grammar : ContextFreeGrammar<'nt, 't>) : Map<'nt, Set<'t>> =
        let getFirstSymbol : ProductionRule<'nt, 't> -> ('nt * Symbol<'nt, 't>) option = function
        | ProductionRule(lhs, sym :: _) -> Some (lhs, sym)
        | _ -> None

        let firstSyms = grammar.P |> Seq.choose getFirstSymbol
                                  |> MapHelpers.groupFstSet

        grammar.V |> Seq.map (fun sym -> sym, first firstSyms sym)
                  |> Map.ofSeq

    /// Creates an LR(k) parser, which is a triple of an action table, a goto table,
    /// and an initial state. If this cannot be done, an error message is returned.
    let createLR (closure : Set<LRItem<'nt, 't> * 'a> -> Set<LRItem<'nt, 't> * 'a>)
                 (goto : Set<LRItem<'nt, 't> * 'a> -> Symbol<'nt, 't> -> Set<LRItem<'nt, 't> * 'a>)
                 (follow : 'nt -> Set<'t>)
                 (initialLookahead : LRItem<'nt, 't> -> 'a)
                 (grammar : ContextFreeGrammar<'nt, 't>) =
        let initState = grammar.P |> Set.filter (fun (ProductionRule(head, _)) -> head = grammar.S)
                                  |> Set.map (fun (ProductionRule(head, body)) -> let item = LRItem(head, [], body) in item, initialLookahead item)
        let stateMap = states closure goto initState |> SetHelpers.toIndexedMap
        let actions = actionTable closure goto follow grammar.S stateMap
        let gotos = gotoTable closure goto grammar.V stateMap
        let initStateIndex = Map.find initState stateMap

        Result.map (fun actionMap -> actionMap, gotos, initStateIndex) actions

    /// Creates an LR(0) parser from the given grammar.
    /// If this cannot be done, an error message is returned.
    let createLR0 (grammar : ContextFreeGrammar<'nt, 't>) =
        let closure = closureLR0 grammar
        let follow _ = grammar.T
        
        createLR closure gotoLR0 follow ignore grammar

    /// Converts the given parser, which has a
    /// map-based action and goto table, to a parser
    /// that uses a function-based action and goto table.
    let toFunctionalParser (actionTable : Map<int * LRTerminal<'t>, LRAction<'nt, 't>>, gotoTable : Map<int * 'nt, int>, startState : int) 
                           : (int -> LRTerminal<'t> -> LRAction<'nt, 't>) * (int -> 'nt -> int) * int = 
        getAction actionTable, (fun i nt -> Map.find (i, nt) gotoTable), startState

    let private showTable (show : 'c -> string) (cellWidth : int) (rows : seq<'a>) (columns : seq<'b>) (table : Map<'a * 'b, 'c>) =
        let actualWidth = cellWidth + 2
        let colCount = Seq.length columns
        let horizSep = String.replicate ((actualWidth + 1) * (colCount + 1) - 1) "-"

        let showCellContents text =
            let text = " " + text
            text + String.replicate (actualWidth - String.length text) " "

        let getCellValue row column =
            match Map.tryFind (row, column) table with
            | Some x -> showCellContents (show x)
            | None   -> showCellContents ""

        let cells = rows |> Seq.map (fun r -> Seq.append (seq [showCellContents (r.ToString())]) (Seq.map (fun c -> getCellValue r c) columns))
        let cells = Seq.append [Seq.append [showCellContents ""] (columns |> Seq.map (fun c -> showCellContents(c.ToString())))] cells

        cells |> Seq.map (String.concat "|")
              |> String.concat (System.Environment.NewLine + horizSep + System.Environment.NewLine)

    /// Prints an LR parser triple.
    let printLR (actionTable : Map<int * LRTerminal<'t>, LRAction<'nt, 't>>, gotoTable : Map<int * 'nt, int>, startState : int)
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

        let termStrings = allTerminals |> Seq.map string
        let nontermStrings = allNonterminals |> Seq.map (fun x -> x.ToString())

        let getRule = function
        | Reduce rule -> Some rule
        | _           -> None

        let allRules = actionTable |> Seq.choose (fun (KeyValue((_, _), a)) -> getRule a)
                                   |> Set.ofSeq
                                   |> SetHelpers.toIndexedMap

        let printedRules = allRules |> Seq.sortBy (fun (KeyValue(_, i)) -> i)
                                    |> Seq.map (fun (KeyValue(r, i)) -> sprintf "%d. %s" i (string r))
                                    |> String.concat System.Environment.NewLine

        let cellWidth = Seq.concat [termStrings |> Seq.map String.length; 
                                    nontermStrings |> Seq.map String.length;
                                    allRules |> Seq.map (fun (KeyValue(_, i)) -> String.length (string i) + 1);
                                    allStates |> Seq.map (string >> String.length);
                                    seq [ String.length "eof" ]]
                        |> Seq.max

        let showAction = function
        | Shift state -> string state
        | Reduce rule -> allRules |> Map.find rule
                                  |> string
                                  |> (+) "r"
        | Accept      -> "acc"
        | Fail        -> ""

        let actionTable = showTable showAction cellWidth allStates allTerminals actionTable
        let gotoTable = showTable string cellWidth allStates allNonterminals gotoTable

        ["Rules:"; printedRules; "";
         "Action table:"; actionTable; "";
         "Goto table:"; gotoTable] 
         |> String.concat System.Environment.NewLine