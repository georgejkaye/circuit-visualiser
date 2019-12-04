open Helpers;
open Circuits;
open Lattices;

let functions = [Function("F", 2, 2, (x,y) => y)]

let rec functionLookup = (func) => {
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

let rec parse = (v, tokens) => parse'(v, tokens, [], [])
and parse' = (v, tokens, bracks, stack) => {
    switch(tokens){
    | [] => failwith("erm...") 
    | [x, ...xs] => switch(x){
                    | "(" => parse'(v, xs, ["(", ...bracks], stack)
                    | "[" => parse'(v, xs, ["[", ...bracks], stack)
                    | ")" => List.hd(bracks) == "(" ? failwith("todo") : failwith("parse error")
                    | "]" => List.hd(bracks) == "[" ? failwith("todo") : failwith("parse error")
                    | "*" => failwith("todo tensor")
                    | "." => Composition(List.hd(stack), parse'(v, xs, bracks, []))
                    | a   => let sym = v.parse(a); fst(sym) ? Value(snd(sym)) : functionLookup(a) 
                    }
    }
}