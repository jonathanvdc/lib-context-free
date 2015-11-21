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

    let parseSExpr (expr : string) : ParseTree<string, char> =
        use reader = new System.IO.StringReader(expr)
        match TreeHandler.readNode reader with
        | Success x -> ParseTree.map id (fun s -> Seq.exactlyOne s) x
        | Error e -> raise (System.FormatException(e))

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

    let grammar3 =
        ContextFreeGrammar(
            set [
                    ProductionRule("S", [Terminal 'i'; Nonterminal "S"])
                    ProductionRule("S", [Nonterminal "F"])
                    ProductionRule("F", [Terminal 'i'; Nonterminal "F"; Terminal 'e'])
                    ProductionRule("F", [])
                ], "S")

    let grammar3TestCase1 = ("", ProductionNode("S", [ProductionNode("F", [])]))
    let grammar3TestCase2 = ("i", ProductionNode("S", [TerminalLeaf 'i'; ProductionNode("F", [])]))
    let grammar3TestCase3 = ("ie", ProductionNode("S", [ProductionNode("F", [TerminalLeaf 'i'; ProductionNode("F", []); TerminalLeaf 'e'])]))
    let grammar3TestCase4 = ("iie", ProductionNode("S", [TerminalLeaf 'i'; ProductionNode("F", [TerminalLeaf 'i'; ProductionNode("F", []); TerminalLeaf 'e'])]))

    let grammar4 = 
        ContextFreeGrammar(
            set [
                    ProductionRule("S", [Nonterminal "S"; Nonterminal "F"])
                    ProductionRule("S", [])
                    ProductionRule("F", [Terminal 'x'])
                ], "S")

    let grammar4TestCase1 = ("", ProductionNode("S", []))
    let grammar4TestCase2 = ("x", ProductionNode("S", [ProductionNode("S", []); ProductionNode("F", [TerminalLeaf 'x'])]))

    let grammar5 = 
        ContextFreeGrammar(
            set [
                    ProductionRule("S", [Nonterminal "F"; Terminal '+'; Nonterminal "S"])
                    ProductionRule("S", [Nonterminal "F"])
                    ProductionRule("F", [Nonterminal "I"; Nonterminal "F"])
                    ProductionRule("F", [Nonterminal "I"])
                    ProductionRule("I", [Terminal '('; Nonterminal "S"; Terminal ')'])
                    ProductionRule("I", [Terminal '0'])
                    ProductionRule("I", [Terminal '1'])
                    ProductionRule("I", [Nonterminal "I"; Terminal '*'])
                ], "S")

    let grammar5TestCase1 = ("0", parseSExpr "(S (F (I 0)))")
    let grammar5TestCase2 = ("01", parseSExpr "(S (F (I 0) (F (I 1))))")
    let grammar5TestCase3 = ("0*", parseSExpr "(S (F (I (I 0) *)))")
    let grammar5TestCase4SExpr = "(S (F (I (I 0) *)) + (S (F (I 1))))"
    let grammar5TestCase4 = ("0*+1", parseSExpr grammar5TestCase4SExpr)
    let grammar5TestCase5 = (sprintf "(%s)" (fst grammar5TestCase4), parseSExpr (sprintf "(S (F (I \( %s \) )))" grammar5TestCase4SExpr))

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

    [<TestMethod>]
    member this.LR1Grammar3Test1 () = LR1Test grammar3 grammar3TestCase1
    [<TestMethod>]
    member this.LR1Grammar3Test2 () = LR1Test grammar3 grammar3TestCase2
    [<TestMethod>]
    member this.LR1Grammar3Test3 () = LR1Test grammar3 grammar3TestCase3
    [<TestMethod>]
    member this.LR1Grammar3Test4 () = LR1Test grammar3 grammar3TestCase4

    [<TestMethod>]
    member this.LR0Grammar4Test1 () = LR0Test grammar4 grammar4TestCase1
    [<TestMethod>]
    member this.LR0Grammar4Test2 () = LR0Test grammar4 grammar4TestCase2

    [<TestMethod>]
    member this.LR1Grammar4Test1 () = LR1Test grammar4 grammar4TestCase1
    [<TestMethod>]
    member this.LR1Grammar4Test2 () = LR1Test grammar4 grammar4TestCase2

    [<TestMethod>]
    member this.LR1Grammar5Test1 () = LR1Test grammar5 grammar5TestCase1
    [<TestMethod>]
    member this.LR1Grammar5Test2 () = LR1Test grammar5 grammar5TestCase2    
    [<TestMethod>]
    member this.LR1Grammar5Test3 () = LR1Test grammar5 grammar5TestCase3
    [<TestMethod>]
    member this.LR1Grammar5Test4 () = LR1Test grammar5 grammar5TestCase4
    [<TestMethod>]
    member this.LR1Grammar5Test5 () = LR1Test grammar5 grammar5TestCase5