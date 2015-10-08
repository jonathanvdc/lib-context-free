namespace libcontextfree

/// Defines a number of string helper functions.
module StringHelpers =
    /// Escape a string for HTML output in a Graphviz .dot file.
    let htmlEscape : string -> string =
        let shouldEscape (c : char) : bool =
            c = '"' || c = '&' || c = '<' || c = '>' || c = '\\' || c > '~'

        let escapeChar : char -> string =
            function
            | '\n' -> "<br/>"
            | c when shouldEscape c -> sprintf "&#%d;" (int c)
            | c -> string c

        String.collect escapeChar