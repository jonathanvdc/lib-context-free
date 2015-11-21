namespace FsUnit.Test
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open NHamcrest.Core
open libcontextfree

[<TestClass>]
type ParserTests () =
    let LLTest (grammar : ContextFreeGrammar<'a, char>) (testInput : string, testOutput : ParseTree<'a, char>) : unit =
        match LLParser.createLLTable grammar with
        | Success table -> 
            let tableFunc = fun x y -> Map.tryFind (x, y) table
            match LLParser.parse id tableFunc (Nonterminal grammar.S) (List.ofSeq testInput) with
            | Some ([], tree) -> ()
            | Some (_, _)
            | None            -> raise (System.Exception(sprintf "'%s' could not be parsed." testInput))
        | Error e -> raise (System.InvalidOperationException(e))

    let LRTest createTable (grammar : ContextFreeGrammar<'a, char>) (testInput : string, testOutput : ParseTree<'a, char>) : unit =
        match createTable grammar with
        | Success parser -> 
            let parse = LRParser.toFunctionalParser parser |||> LRParser.parse id
            match parse (List.ofSeq testInput) with
            | Choice1Of2 result when result = testOutput -> ()
            | Choice1Of2 result -> raise (System.Exception(sprintf "'%s' was parsed incorrectly. Expected '%A', got '%A'." testInput testOutput result))
            | Choice2Of2 _ -> raise (System.Exception(sprintf "'%s' could not be parsed." testInput))
        | Error e -> raise (System.InvalidOperationException(e))

    let LR0Test (grammar : ContextFreeGrammar<'a, char>) (testInput : string, testOutput : ParseTree<'a, char>) : unit =
        LRTest LRParser.createLR0 grammar (testInput, testOutput)

    let LR1Test (grammar : ContextFreeGrammar<'a, char>) (testInput : string, testOutput : ParseTree<'a, char>) : unit =
        LRTest LRParser.createLR1 grammar (testInput, testOutput)

    let grammar1 = 
        ContextFreeGrammar(
            set [
                    ProductionRule("S", [Terminal 'x'; Nonterminal "S"; Terminal 'z'])
                    ProductionRule("S", [Terminal 'y'; Nonterminal "S"; Terminal 'z'])
                    ProductionRule("S", [])
                ], "S")

    let grammar1TestCase1 = ("xz", ProductionNode("S", [TerminalLeaf 'x'; ProductionNode("S", []); TerminalLeaf 'z']))
    let grammar1TestCase2 = ("", ProductionNode("S", []))
    let grammar1TestCase3 = ("yz", ProductionNode("S", [TerminalLeaf 'y'; ProductionNode("S", []); TerminalLeaf 'z']))
    let grammar1TestCase4 = ("xyzz", ProductionNode("S", [TerminalLeaf 'x'; ProductionNode("S", [TerminalLeaf 'y'; ProductionNode("S", []); TerminalLeaf 'z']); TerminalLeaf 'z']))

    let grammar2 = 
        ContextFreeGrammar(
            set [
                    ProductionRule("S", [Nonterminal "F"; Nonterminal "S"])
                    ProductionRule("S", [])
                    ProductionRule("F", [Terminal 'x'])
                ], "S")

    let grammar2TestCase1 = ("", ProductionNode("S", []))
    let grammar2TestCase2 = ("x", ProductionNode("S", [ProductionNode("F", [TerminalLeaf 'x']); ProductionNode("S", [])]))

    [<TestMethod>]
    member this.LLGrammar1Test1 () = LLTest grammar1 grammar1TestCase1
    [<TestMethod>]
    member this.LLGrammar1Test2 () = LLTest grammar1 grammar1TestCase2
    [<TestMethod>]
    member this.LLGrammar1Test3 () = LLTest grammar1 grammar1TestCase3
    [<TestMethod>]
    member this.LLGrammar1Test4 () = LLTest grammar1 grammar1TestCase4

    [<TestMethod>]
    member this.LR1Grammar1Test1 () = LR1Test grammar1 grammar1TestCase1
    [<TestMethod>]
    member this.LR1Grammar1Test2 () = LR1Test grammar1 grammar1TestCase2
    [<TestMethod>]
    member this.LR1Grammar1Test3 () = LR1Test grammar1 grammar1TestCase3
    [<TestMethod>]
    member this.LR1Grammar1Test4 () = LR1Test grammar1 grammar1TestCase4

    [<TestMethod>]
    member this.LLGrammar2Test1 () = LLTest grammar2 grammar2TestCase1
    [<TestMethod>]
    member this.LLGrammar2Test2 () = LLTest grammar2 grammar2TestCase2

    [<TestMethod>]
    member this.LR1Grammar2Test1 () = LR1Test grammar2 grammar2TestCase1
    [<TestMethod>]
    member this.LR1Grammar2Test2 () = LR1Test grammar2 grammar2TestCase2
