namespace libcontextfree

module LRParser =

    type LRAction<'nt, 't> =
    /// Indicates that the next state is n.
    | Shift of int
    /// Indicates that a reduction with the given grammar rule should be performed.
    | Reduce of ProductionRule<'nt, 't>
    /// Indicates that the LR parser has successfully parsed the input string.
    | Done
    /// Indicates that the LR parser could not parse the input string.
    | Error

    type LRTerminal<'t> = 
    | LRTerminal of 't
    | EndOfInput

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

    /// Remove the matched topmost L symbols (and parse trees and associated state numbers) from the parse stack.
    /// This exposes a prior state p that was expecting an instance of the Lhs symbol.
    /// Join the L parse trees together as one parse tree with new root symbol Lhs.
    /// Lookup the next state n from row p and column Lhs of the LHS Goto table.
    /// Push the symbol and tree for Lhs onto the parse stack.
    /// Push next state n onto the parse stack as the new current state.
    /// The lookahead and input stream remain unchanged.
    let reduce (gotoTable : int -> 'nt -> int) 
               (rule : ProductionRule<'nt, 't>) 
               (state : ParserState<'nt, 't>) : ParserState<'nt, 't> = 
        match rule, state with
        | ProductionRule(lhs, body), (ts, stack) ->
            let elems, remStack = ListHelpers.splitAtIndex (List.length body) stack
            let newTree = ProductionNode(lhs, List.map fst elems)
            let p = remStack |> List.head
                             |> snd
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
                    : ParserState<'nt, 't> -> Choice<ParseTree<'nt, 't>, 't list> = function
    | input, stack ->
        let peek = peekTerminal input
        let state = 
            match stack with
            | x :: _ -> snd x
            | []     -> 0

        match actionTable state peek with
        | Shift n ->
            (input, stack) |> shift n 
                           |> parseLR actionTable gotoTable
        | Reduce rule ->
            (input, stack) |> reduce gotoTable rule
                           |> parseLR actionTable gotoTable
        | Done ->
            match normalizeLRTree (stack |> List.head |> fst) with
            | Some tree -> Choice1Of2 tree
            | None      -> Choice2Of2 input
        | Error ->
            Choice2Of2 input

    /// Defines LR(0) items, which are grammar rules with a special dot added somewhere in the right-hand side. 
    /// The 'dot' is represented by storing the rule's body in two lists.
    ///
    /// For example the rule E → E + B has the following four corresponding items:
    ///     E → • E + B
    ///     E → E • + B
    ///     E → E + • B
    ///     E → E + B •
    type LRItem<'nt, 't> = 
        | LRItem of 'nt * Symbol<'nt, 't> list * Symbol<'nt, 't> list 

        /// Gets the symbol directly after the dot.
        member this.NextSymbol =
            match this with
            | LRItem(_, _, x :: _) -> Some x
            | _                     -> None

        override this.ToString() =
            match this with
            | LRItem(head, left, right) ->
                sprintf "%O → %O • %O" head left right

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
                    (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LRItem<'nt, 't> * 'a>) = 
        let getPairedNonterminals : ProductionRule<'nt, 't> -> ('nt * ProductionRule<'nt, 't>) option = function
        | ProductionRule(lhs, Nonterminal _ :: _) as rule -> Some (lhs, rule)
        | _ -> None

        let pairedNonterms = grammar.P |> Seq.choose getPairedNonterminals
                                       |> MapHelpers.groupFstSet

        let induction ((lrItem : LRItem<'nt, 't>, _) as input) =
            match lrItem.NextSymbol with
            | Some (Nonterminal nt) ->
                pairedNonterms.[nt] |> Set.map (createItem input)
                                    |> Set.unionMany
            | _ -> Set.empty

        SetHelpers.closure induction basis

    /// A specialization of the closure function for LR(0) items.
    let closureLR0 (grammar : ContextFreeGrammar<'nt, 't>) (basis : Set<LR0Item<'nt, 't>>) =
        let createItem (oldItem, _) = function
        | ProductionRule(head, body) -> Set.singleton (LRItem(head, [], body), ())

        closure createItem grammar basis

    /// Computes the FIRST set for the given nonterminal map and nonterminal.
    /// FIRST(A) is the set of terminals which can appear as the first element
    /// of any chain of rules matching nonterminal A.
    ///
    /// The given nonterminal map associates every nonterminal with
    /// all first symbols that occur in rules that have said 
    /// nonterminal on their left-hand side.
    ///
    /// FIRST(A) is used when building LR(1) tables.
    let first (nonterminalMap : Map<'nt, Set<Symbol<'nt, 't>>>) (sym : 'nt) : Set<'t> =
        let induction : Symbol<'nt, 't> -> Set<Symbol<'nt, 't>> = function
        | Nonterminal sym -> nonterminalMap.[sym]
        | Terminal    _   -> Set.empty

        Nonterminal sym |> Set.singleton 
                        |> SetHelpers.closure induction
                        |> Symbol.terminals
                        |> Set.ofSeq

    