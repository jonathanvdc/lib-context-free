open libcontextfree
open ParseTreeModule

/// Gets a list of string representations for the given tree's entire derivation 
/// sequence, including the input tree itself. 
/// Nonterminal nodes are replaced by the given split-at function.
let showDerivationSequence splitAt (tree : ParseTree<'a, 'b>) =
    // First, the derivation sequence for this parse tree is obtained, 
    // and the parse tree is explicitly prepended.
    // Then, every step in the derivation sequence is converted to the
    // concatenation of its trees' head strings.
    [tree] :: derivation splitAt [tree] |> List.map (List.map treeHead >> List.fold (+) "")

/// Gets a list of string representations for the given tree's entire leftmost derivation 
/// sequence, including the input tree itself. 
let showLeftmostDerivationSequence<'a, 'b> : ParseTree<'a, 'b> -> string list = 
    showDerivationSequence ListHelpers.splitAtFirst

/// Gets a list of string representations for the given tree's entire rightmost derivation 
/// sequence, including the input tree itself. 
let showRightmostDerivationSequence<'a, 'b> : ParseTree<'a, 'b> -> string list = 
    showDerivationSequence ListHelpers.splitAtLast

[<EntryPoint>]
let main argv = 
    let testTree = ProductionNode('S', [ ProductionNode('S', [TerminalLeaf 'h']); EpsilonLeaf; ProductionNode('S', [TerminalLeaf 'i']) ])

    printfn "%A" testTree
    treeYield testTree |> printfn "%s"
    treeHead testTree |> printfn "%s"
    showLeftmostDerivationSequence testTree |> printfn "%A"
    showRightmostDerivationSequence testTree |> printfn "%A"
    0 // return an integer exit code
