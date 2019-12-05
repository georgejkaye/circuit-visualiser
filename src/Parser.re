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
    | []             => failwith("Parse error, char " ++ string_of_int(i) ++ ": " ++ bracket ++ " expected")
    | ["(",...xs]    => scanForClosingBracket'(xs,i+1,j+1,bracket,[")",...brackstack])
    | ["[",...xs]    => scanForClosingBracket'(xs,i+1,j+1,bracket,["]",...brackstack])
    | [x,...xs]      => if(x == bracket) { 
                            if(List.length(brackstack) == 1 && List.hd(brackstack) == bracket){ 
                                j
                            } else { 
                                List.length(brackstack) == 0 ? failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected " ++ bracket ++ " encountered") 
                                                             : scanForClosingBracket'(xs,i+1,j+1,bracket,List.tl(brackstack))
                            }
                        } else {
                            scanForClosingBracket'(xs,i+1,j+1,bracket,brackstack)
                        }
    }
}

/* Find the index of the next composition 'dot', to determine the end of the tensor */
let rec scanForNextComposition = (xs, i) => scanForNextComposition'(xs, i, 0)
and scanForNextComposition' = (xs, i, bracks) => {
    switch(xs){
    | [] => i
    | ["(",...xs] => scanForNextComposition'(xs, i+1, bracks+1)
    | ["[",...xs] => scanForNextComposition'(xs, i+1, bracks+1)
    | [")",...xs] => scanForNextComposition'(xs, i+1, bracks-1)
    | ["]",...xs] => scanForNextComposition'(xs, i+1, bracks-1)
    | [".",...xs] => bracks == 0 ? i : scanForNextComposition'(xs, i+1, bracks)
    | [x,...xs]   => scanForNextComposition'(xs, i+1, bracks)
    }
}

let rec parse = (v, tokens) => parse'(v, 0, tokens, [], false)
and parse' = (v, i, tokens, stack, tensor) => {
    Js.log("stack: " ++ printList(stack, (x) => printComponent(v,x)))
    switch(tokens){
        | [] => tensor ? Tensor(stack) : (List.length(stack) == 1 ? List.hd(stack) : failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected end of term"))
        | [x, ...xs] => switch(x){
                                | "(" => let j = scanForClosingBracket(xs, 1, ")"); 
                                                    let parsedSubterm = parse'(v, i+1, slice(xs, 0, j-1), [], false);
                                                    parse'(v, j+1, trim(xs,j+1), stack @ [parsedSubterm], tensor)
                                | "(" => let j = scanForClosingBracket(xs, 1, "]"); 
                                                    let parsedSubterm = parse'(v, i+1, slice(xs, 0, j-1), [], false);
                                                    parse'(v, j+1, trim(xs,j+1), stack @ [parsedSubterm], tensor)
                                | ")" => failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected ) encountered")
                                | "]" => failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected ] encountered")
                                | "." => List.length(stack) == 0 ? failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected * encountered")
                                                                 : Composition(List.hd(stack), parse'(v, i+1, xs, List.tl(stack), tensor))
                                | "*" => if(tensor){
                                            parse'(v, i+1, xs, stack, tensor)
                                         } else {
                                            let j = scanForNextComposition(xs, 0);
                                                    let parsedTensor = parse'(v, i+1, slice(xs,0,j-1),stack, true);
                                                    parse'(v, j, trim(xs,j), drop(stack,1) @ [parsedTensor], false);
                                         }
                                | a   => if(!tensor && List.length(stack) > 0 ){
                                                 failwith("unexpected term encountered, did you forget a composition or tensor?") 
                                             } else {
                                                 let sym = v.parse(a); 
                                                 let subterm = fst(sym) ? Value(snd(sym)) : functionLookup(a);
                                                 parse'(v, i+1, xs, stack @ [subterm], tensor)
                                             }
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