open Helpers;
open Circuits;

let stringToChars = (s) => {List.map((i => String.get(s,i)), range(String.length(s) - 1))}

/* Tokenise an input string into terms and brackets */
let rec tokenise = (string) => { (List.map((x) => String.init(List.length(x), (i) => (List.nth(x, i))), tokenise'(stringToChars(string), [])))}
and tokenise' = (chars, current) => {
    switch(chars){
    | []           => [List.rev(current)]
    | [' ', ...xs] => [List.rev(current), ...tokenise'(xs, [])] 
    | ['(', ...xs] => [List.rev(current), ['('], ...tokenise'(xs, [])]
    | [')', ...xs] => [List.rev(current), [')'], ...tokenise'(xs, [])]
    | ['[', ...xs] => [List.rev(current), ['['], ...tokenise'(xs, [])]
    | [']', ...xs] => [List.rev(current), [']'], ...tokenise'(xs, [])]
    | [x, ...xs]   => tokenise'(xs, [x, ...current]) 
    }
}