namespace libcontextfree

// The right-hand side of a production rule in a CFG in Chomsky normal form
// may never contain the starting symbol. To enforce this, our alphabet of
// nonterminals is the type `'nt option`, and the starting symbol S0 is
// encoded as `None`. The right hand sides are then of the type `('nt * 'nt)`
// instead of `('nt option * 'nt option).`

// When implementing the algorithms, translate
//
//   * "a nonterminal V"               to     'nt option
//   * "a nonterminal V ... where      
//      V is not the start symbol"     to     'nt

type ChomskyNormalRule<'nt, 't> =
    | BinaryRule of 'nt option * ('nt * 'nt)   // A → BC
    | TerminalRule of 'nt option * 't          // A → a
    | StartToEpsilon                           // S → ε

type ChomskyNormalCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | ChomskyNormalCFG of Set<ChomskyNormalRule<'nt, 't>>

module ChomskyNormalForm =
    type StartRule<'nt, 't> =
    | SProductionRule of 'nt option * Symbol<'nt, 't> list

    type StartCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | StartCFG of Set<StartRule<'nt, 't>>

    type TermRule<'nt, 't> =
    | TNonterminalRule of 'nt option * 'nt list
    | TTerminalRule of 'nt option * 't

    type TermCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | TermCFG of Set<TermRule<'nt, 't>>

    type BinRule<'nt, 't> =
    | BBinaryRule of 'nt option * ('nt * 'nt)
    | BUnitRule of 'nt option * 'nt
    | BEpsilonRule of 'nt option
    | BTerminalRule of 'nt option * 't

    type BinCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | BinCFG of Set<BinRule<'nt, 't>>

    type DelRule<'nt, 't> =
    | DBinaryRule of 'nt option * ('nt * 'nt)
    | DUnitRule of 'nt option * 'nt
    | DTerminalRule of 'nt option * 't
    | DStartToEpsilon

    type DelCFG<'nt, 't when 'nt : comparison and 't : comparison> =
    | DelCFG of Set<DelRule<'nt, 't>>

    let startStep : ContextFreeGrammar<'nt, 't> -> StartCFG<'nt, 't> =
        fun x -> raise (new System.NotImplementedException())

    let termStep : StartCFG<'nt, 't> -> TermCFG<'nt, 't> =
        fun x -> raise (new System.NotImplementedException())

    let binStep : TermCFG<'nt, 't> -> BinCFG<'nt, 't> =
        fun x -> raise (new System.NotImplementedException())

    let delStep : BinCFG<'nt, 't> -> DelCFG<'nt, 't> =
        fun x -> raise (new System.NotImplementedException())

    let unitStep : DelCFG<'nt, 't> -> ChomskyNormalCFG<'nt, 't> =
        fun x -> raise (new System.NotImplementedException())

    let chomskyNormalForm<'nt, 't when 'nt : comparison and 't : comparison>
            : ContextFreeGrammar<'nt, 't> -> ChomskyNormalCFG<'nt, 't> =
        startStep >> termStep >> binStep >> delStep >> unitStep
