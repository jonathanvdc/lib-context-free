namespace libcontextfree

open FSharp.Data
open System.Xml.Linq

module XmlHandler =
    [<Literal>]
    let private CfgXmlSample = 
        """<?xml version="1.0"?>
        <CFG>
            <Variables>
                SR
                <StartSymbol>S</StartSymbol>
            </Variables>
            <Terminals> ab </Terminals>
            <Productions>
                <Rule RHS="aSb|b" LHS="S"/>
                <Rule RHS="R" LHS="S"/>
                <Rule RHS="a" LHS="R"/>
            </Productions>
        </CFG>
        """

    type CFGFile = XmlProvider<CfgXmlSample>

    let ofCfg (cfg : ContextFreeGrammar<char, char>) : CFGFile.Cfg =
        let setToString (xs : Set<char>) =
            xs |> Seq.map string 
               |> String.concat ""

        let terminals = setToString cfg.T
        let nonterminals = setToString cfg.V
        let startSymbol = string cfg.S
        let variablesNode = XElement(XName.Get("Variables"))
        variablesNode.Add(nonterminals)
        variablesNode.Add(XElement(XName.Get("StartSymbol"), startSymbol))
        let variables = CFGFile.Variables(variablesNode)
        let rules = cfg.P |> Seq.map (fun (ProductionRule(head, body)) -> 
                                      CFGFile.Rule(body |> Seq.map string |> String.concat "", string head))
                          |> Array.ofSeq
        let productions = CFGFile.Productions(rules)
        CFGFile.Cfg(variables, terminals, productions)

    let toCfg (input : CFGFile.Cfg) : ContextFreeGrammar<char, char> option =
        let terminals = input.Terminals |> Set.ofSeq

        let toSymbol c =
            if terminals.Contains c then
                Terminal c
            else
                Nonterminal c

        match input.Variables.StartSymbol |> Seq.toList with
        | [startSymbol] ->
            let startSymbol = input.Variables.StartSymbol |> Seq.exactlyOne
            let toRule (inputRule : CFGFile.Rule) : Set<ProductionRule<char, char>> option =
                match inputRule.Lhs |> Seq.toList with
                | [ruleHead] -> 
                    inputRule.Rhs.Split('|') |> Seq.map (Seq.map toSymbol >> List.ofSeq)
                                             |> Seq.map (fun body -> ProductionRule(ruleHead, body))
                                             |> Set.ofSeq
                                             |> Some
                | _          ->
                    // There can be only one head symbol per production rule.
                    // Quit if this invariant is not respected.
                    None 

            let foldRule (state : Set<ProductionRule<char, char>> option) (inputRule : CFGFile.Rule) =
                match state with
                | Some result -> 
                    match toRule inputRule with
                    | Some newRules -> Some(Set.union newRules result)
                    | None          -> None
                | None        -> None

            match input.Productions.Rules |> Seq.fold foldRule (Some Set.empty) with
            | Some rules ->
                Some(ContextFreeGrammar(rules, startSymbol))
            | None       ->
                None
        | _ -> 
            // There can be only one start symbol. 
            // Quit if this invariant is not respected.
            None 