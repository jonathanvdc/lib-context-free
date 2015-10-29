namespace libcontextfree

/// Defines a number of string helper functions.
module StringHelpers =
    /// Concatenate a list of strings using newlines.
    let concatLines : seq<string> -> string =
        String.concat System.Environment.NewLine

    /// Escape a string for output in a Graphviz .dot file.
    let dotEscape : string -> string =
        let shouldEscape (c : char) : bool =
            c = '"' || c = '&' || c = '<' || c = '>' || c = '\\' || c > '~'

        let escapeChar : char -> string =
            function
            | '\n' -> "\\n"
            | c when shouldEscape c -> sprintf "&#%d;" (int c)
            | c -> string c

        String.collect escapeChar

    /// A collection of meaning-free letters that can be used for remapping
    /// nonterminal names to single characters. Letters like S and ε, which
    /// usually have some special meaning in a grammar, are avoided.
    let letters : string =
        "ABCDEFGHIJKLMNOPQRTUVWXYZ\
         abcdefghijklmnopqrstuvwxyz\
         αβγδζηθικλμνξπρστυφχψως\
         ΓΔΘΛΞΠΣΦΨΩджилшыюя"