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

    /// "Normalizes" the given LR parser tree: end-of-input leaves are removed recursively.
    let rec normalizeLRTree : ParseTree<'nt, LRTerminal<'t>> -> ParseTree<'nt, 't> option = function
    | TerminalLeaf (LRTerminal t) -> Some (TerminalLeaf t)
    | TerminalLeaf EndOfInput     -> None
    | ProductionNode(lhs, body)   ->
        Some (ProductionNode(lhs, List.choose normalizeLRTree body))

    /// Applies the LR parsing algorithm recursively to the given action table, goto table and state.
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

