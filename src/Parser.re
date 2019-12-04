open Helpers;
open Circuits;
open Lattices;

let functions = [Function("F", 2, 2, (x,y) => y),Function("id", 1, 1, (x,y) => y)]

let functionLookup = (func) => {
    List.find((x) => switch(x){
                        | Function(id,ins,outs,f) => (id == func)
                        }, functions)
}

let stringToChars = (s) => {List.map((i => String.get(s,i)), range(String.length(s) - 1))}

/* Tokenise an input string into terms and brackets */
let rec tokenise = (string) => { (List.map((x) => String.init(List.length(x), (i) => (List.nth(x, i))), tokenise'(stringToChars(string), [])))}
and tokenise' = (chars, current) => {
    switch(chars){
    | []           => List.length(current) == 0 ? [] : [List.rev(current)]
    | [' ', ...xs] => List.length(current) == 0 ? tokenise'(xs, []) : [List.rev(current), ...tokenise'(xs, [])] 
    | ['(', ...xs] => List.length(current) == 0 ? [['('], ...tokenise'(xs, [])] : [List.rev(current), ['('], ...tokenise'(xs, [])]
    | [')', ...xs] => List.length(current) == 0 ? [[')'], ...tokenise'(xs, [])] : [List.rev(current), [')'], ...tokenise'(xs, [])]
    | ['[', ...xs] => List.length(current) == 0 ? [['['], ...tokenise'(xs, [])] : [List.rev(current), ['['], ...tokenise'(xs, [])]
    | [']', ...xs] => List.length(current) == 0 ? [[']'], ...tokenise'(xs, [])] : [List.rev(current), [']'], ...tokenise'(xs, [])]
    | [x, ...xs]   => tokenise'(xs, [x, ...current]) 
    }
}

let rec scanForClosingBracket = (xs, i, bracket) => scanForClosingBracket'(xs,i,0,bracket,[bracket])
and scanForClosingBracket' = (xs, i, j, bracket, brackstack) => {
    switch(xs) {
    | []              => failwith("Parse error, char " ++ string_of_int(i) ++ ": " ++ bracket ++ " expected")
    | [bracket,...xs] => List.length(brackstack) == 1 && List.hd(brackstack) == bracket ? j : scanForClosingBracket'(xs,i+1,j+1,bracket,List.tl(brackstack))
    | ["(",...xs]     => scanForClosingBracket'(xs,i+1,j+1,bracket,[")",...brackstack])
    | ["[",...xs]     => scanForClosingBracket'(xs,i+1,j+1,bracket,["]",...brackstack])
    | [x,...xs]       => scanForClosingBracket'(xs,i+1,j+1,bracket,brackstack)
    }
}

let rec parse = (v, tokens) => { parse'(v, 0, tokens, []) }
and parse' = (v, i, tokens, stack) => {
    switch(tokens){
        | [] => failwith("erm...") 
        | [x, ...xs] => switch(x){
                                | "(" => let j = scanForClosingBracket(xs, i, ")"); 
                                                    let parsedSubterm = parse(v,slice(xs,i,j));
                                                    parse'(v, j, trim(xs,j), stack @ [parsedSubterm])
                                | "(" => let j = scanForClosingBracket(xs, i, "]"); 
                                                    let parsedSubterm = parse(v,slice(xs,i,j));
                                                    parse'(v, j, trim(xs,j), stack @ [parsedSubterm])
                                | ")" => failwith("parse error, char " ++ string_of_int(i) ++ " unexpected ) encountered")
                                | "]" => failwith("parse error, char " ++ string_of_int(i) ++ " unexpected ] encountered")
                                | "*" => failwith("todo composition")
                                | "." => failwith("todo tensor")
                                | a   => let sym = v.parse(a); fst(sym) ? Value(snd(sym)) : functionLookup(a)
                                }
    }
}
 

/*let rec parse = (v, tokens) => parse'(v, tokens, [], false, [])
and parse' = (v, tokens, bracks, scanningForSubterm, subterm, endOfSubterm) => {
    switch(tokens){
    | [] => failwith("erm...") 
    | [x, ...xs] => scanningForSubterm 
                        ? (x == endOfSubterm ? parse(v, List.rev(subterm), [], false, []) : parse'(v, xs, bracks, scanningForSubterm, [x,...subterm], endOfSubterm)) 
                        : switch(x){
                            | "(" => parse'(v, xs, ["(", ...bracks], true, [], ")")
                            | "[" => parse'(v, xs, ["(", ...bracks], true, [], "]")
                            | ")" => List.hd(bracks) == "(" ? failwith("todo") : failwith("parse error")
                            | "]" => List.hd(bracks) == "[" ? failwith("todo") : failwith("parse error")
                            | "*" => failwith("todo tensor")
                            | "." => parse(v, Composition(List.hd(stack), parse'(v, xs, bracks, [])))
                            | a   => let sym = v.parse(a); fst(sym) ? Value(snd(sym)) : functionLookup(a) 
                            }
    }
}*/