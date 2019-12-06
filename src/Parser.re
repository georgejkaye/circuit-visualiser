open Helpers;
open Circuits;
open Lattices;

let functionLookup = (func,funcs) => {
    switch(List.find((x) => switch(x){
                        | Function(id,ins,outs,f) => (id == func)
                        }, funcs)){
                            | item => item
                            | exception Not_found => failwith("Function not in library")
                        }
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

let match = (i, regex, string) =>{
    let matches = Js.String.match(regex, string)
    switch(matches){
    | None =>  failwith("Parse error, char " ++ string_of_int(i) ++ ": bad syntax")
    | Some(m) => m 
    }
}

let rec parse = (v, funcs, tokens) => parse'(v, funcs, 0, tokens, [], false)
and parse' = (v, funcs, i, tokens, stack, tensor) => {
    switch(tokens){
        | [] => tensor ? Tensor(stack) : (List.length(stack) == 1 ? List.hd(stack) : failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected end of term"))
        | [x, ...xs] => switch(x){
                                | "("   => let j = scanForClosingBracket(xs, 1, ")"); 
                                                    let parsedSubterm = parse'(v, funcs, i+1, slice(xs, 0, j-1), [], false);
                                                    parse'(v, funcs, j+1, trim(xs,j+1), stack @ [parsedSubterm], tensor)
                                | "("   => let j = scanForClosingBracket(xs, 1, "]"); 
                                                    let parsedSubterm = parse'(v, funcs, i+1, slice(xs, 0, j-1), [], false);
                                                    parse'(v, funcs, j+1, trim(xs,j+1), stack @ [parsedSubterm], tensor)
                                | ")"   => failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected ) encountered")
                                | "]"   => failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected ] encountered")
                                | "."   => List.length(stack) == 0 ? failwith("parse error, char " ++ string_of_int(i) ++ ": unexpected * encountered")
                                                                 : compose'(v, List.hd(stack), parse'(v, funcs, i+1, xs, List.tl(stack), tensor))
                                | "*"   => if(tensor){
                                            parse'(v, funcs, i+1, xs, stack, tensor)
                                         } else {
                                            let j = scanForNextComposition(xs, 0);
                                                    let parsedTensor = parse'(v, funcs, i+1, slice(xs,0,j-1),stack, true);
                                                    parse'(v, funcs, j, trim(xs,j), drop(stack,1) @ [parsedTensor], false);
                                         }
                                | "/\\" => parse'(v, funcs, i, [{js|⋏|js},...xs], stack, tensor)
                                | "\\/" => parse'(v, funcs, i, [{js|⋎|js},...xs], stack, tensor)
                                | a     => switch(a.[0]){
                                            | 'x'    => let m = match(i, swapRegEx, a);
                                                        let x = int_of_string(m[1]);
                                                        let y = int_of_string(m[2]);
                                                        parse'(v, funcs, i+1, xs, stack @ [swap(v,x,y).c], tensor) 
                                            | '/'    => let m = match(i, dforkRegEx, a);
                                                        let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [dfork(v,x).c], tensor) 
                                            | '\\'   => let m = match(i, djoinRegEx, a);
                                                        let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [djoin(v,x).c], tensor)      
                                            | 'o'    => let m = match(i, delayRegEx, a);
                                                        let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [delay(v,x).c], tensor)          
                                            | _      => if(!tensor && List.length(stack) > 0 ){
                                                            failwith("unexpected term encountered, did you forget a composition or tensor?") 
                                                        } else {
                                                            let sym = v.parse(a); 
                                                            let subterm = fst(sym) ? Value(snd(sym)) : functionLookup(a,funcs);
                                                            parse'(v, funcs, i+1, xs, stack @ [subterm], tensor)
                                                     }
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