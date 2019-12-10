/**
 * File containing functions for tokenising and
 * subsquently parsing user input into circuits
 */

open Helpers;
open Circuits;
open Lattices;

/* Produce a parse error at character i */
let parseError = (i, message) => failwith("parse error, char " ++ string_of_int(i) ++ ": " ++ message)

/* Convert a string to characters */
let stringToChars = (s) => {List.map((i => String.get(s,i)), range(String.length(s) - 1))}

/***********************/
/* Regular expressions */
/***********************/

let openingBracketsRegEx = [%bs.re "/(\(|\[])/"]
let closingBracketsRegEx = [%bs.re "/(\)|\])(\^[0-9]+)?/"]
let roundBracketsClosingRegEx   = [%bs.re "/(\))(\^[0-9]+)?/"]
let squareBracketsClosingRegEx   = [%bs.re "/(\])(\^[0-9]+)?/"]

let match = (regex, string) =>{
    Js.String.match(regex, string)
}


let rec makeRegExChecks = (i, checks, token) => {
    switch(checks){
    | []        =>  (-1, [||])
    | [x,...xs] =>  switch(match(x,token)){
                        | None    => makeRegExChecks(i+1,xs,token)
                        | Some(x) => (i,x) 
                    }
    }
}

let checkForMatches = (token) => makeRegExChecks(0, constructRegExes, token)

/**********************************************************/
/* Tokenisation                                           */
/* First a string is split into characters and tokenised, */
/* breaking into different tokens at spaces or brackets   */
/**********************************************************/

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
    | ['^', ...xs] => List.length(current) == 0 ? tokenise'(xs, ['^']) : [List.rev(current), ...tokenise'(xs, ['^'])]
    | [x, ...xs]   => tokenise'(xs, [x, ...current]) 
    }
}

/**************************************************************/
/* Parsing                                                    */
/* The tokens are parsed and turned into an actual circuit.   */
/* Typechecking is also performed using the safe constructors */ 
/**************************************************************/

let rec scanForClosingBracket = (xs, i, bracket) => scanForClosingBracket'(xs,i,0,bracket,[bracket])
and scanForClosingBracket' = (xs, i, j, bracket, brackstack) => {
    switch(xs) {
    | []             => parseError(i, bracket ++ " expected")
    | ["(",...xs]    => scanForClosingBracket'(xs,i+1,j+1,bracket,[")",...brackstack])
    | ["[",...xs]    => scanForClosingBracket'(xs,i+1,j+1,bracket,["]",...brackstack])
    | [x,...xs]      => switch(match(closingBracketsRegEx, x)){
                        | None => scanForClosingBracket'(xs,i+1,j+1,bracket,brackstack)
                        | Some(x) => let a = x[1];                            
                                     if(List.length(brackstack) == 0){
                                        parseError(i, "unexpected " ++ a ++ " encountered") 
                                     } else {
                                        if(List.hd(brackstack) == a){
                                            if(List.length(brackstack) == 1){
                                                if(a == bracket){
                                                    j
                                                } else {
                                                    parseError(i, "bracket " ++ bracket ++ " expected but not found")
                                                }
                                            } else {
                                                scanForClosingBracket'(xs,i+1,j+1,bracket,List.tl(brackstack))
                                            }
                                        } else {
                                            parseError(i, "bracket " ++ List.hd(brackstack) ++ " expected but " ++ a ++ " found")
                                        }
                                    }
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
    | [".",...xs] => bracks == 0 ? i : scanForNextComposition'(xs, i+1, bracks)
    | [x,...xs]   => switch(match(closingBracketsRegEx, x)){
                     | Some(a) => scanForNextComposition'(xs, i+1, bracks-1)
                     | None    => scanForNextComposition'(xs, i+1, bracks)
                     }
    }
}

/* Looks up a string in the function library to see if it is a function or not */ 
let functionLookup = (func,funcs) => {
    switch(List.find((x) => switch(x){
                        | Function(id,_,_,_) => (id == func)
                        | _ => failwith("Unexpected non-function found in function library")
                        }, funcs)){
                            | item => item
                            | exception Not_found => failwith("Function " ++ func ++ " not in library")
                        }
                        
}

/* Parses a set of tokens into a circuit. 
 * v: lattice
 * funcs: function library
 * tokens: the list of tokens 
 */
let rec parse = (v, funcs, tokens) => parse'(v, funcs, 1, tokens, [], [], false)
and parse' = (v, funcs, i, tokens, stack, lastterm, tensor) => {
    Js.log("parsing " ++ printStringList(tokens));
    switch(tokens){
        | [] => tensor ? 
                    Tensor(stack) : 
                    (List.length(stack) == 1 ? 
                        List.hd(stack) : 
                        parseError(i, "unexpected end of term")
                    )
        | [x, ...xs] => switch(x){
                                | "("   => parseBrackets(")", v, funcs, i, xs, stack, tensor)
                                | "["   => parseBrackets("]", v, funcs, i, xs, stack, tensor)
                                | ")"   => parseError(i, "unexpected ) encountered")
                                | "]"   => parseError(i, "unexpected ] encountered")
                                | "."   => List.length(stack) == 0 ? parseError(i, "unexpected * encountered")
                                                                   : compose'(v, List.hd(stack), parse'(v, funcs, i+1, xs, List.tl(stack), [], tensor))
                                | "*"   => parseTensor(v, funcs, i, xs, stack, tensor)
                                | "/\\" => parse'(v, funcs, i, [{js|⋏|js},...xs], stack, [], tensor)
                                | "\\/" => parse'(v, funcs, i, [{js|⋎|js},...xs], stack, [], tensor)
                                | a     =>  let matches = checkForMatches(a);
                                            let m = snd(matches);

                                            switch(fst(matches)){
                                                | 0  => let x = int_of_string(m[1]);
                                                        let y = int_of_string(m[2]);
                                                        parse'(v, funcs, i+1, xs, stack @ [swap(v,x,y).c], [swap(v,x,y).c], tensor) 
                                                | 1  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [dfork(v,x).c], [dfork(v,x).c], tensor) 
                                                | 2  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [djoin(v,x).c], [djoin(v,x).c], tensor) 
                                                | 3  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i+1, xs, stack @ [delay(v,x).c], [delay(v,x).c], tensor)  
                                                | 4  => parseTrace(m, v, funcs, i, xs, stack, tensor)
                                                | 5  => parseIteration(m, v, funcs, i, xs, stack, tensor)
                                                | 6  => parseIteration(m, v, funcs, i, xs, stack, tensor)
                                                | 7  => parseExponential(m, v, funcs, i, xs, stack, lastterm, tensor)
                                                | -1 => parseTerm(a, v, funcs, i, xs, stack, tensor)
                                            }
                                        }
    }
} and parseBrackets = (close, v, funcs, i, xs, stack, tensor) => {
    
    let j = scanForClosingBracket(xs, i, close);
    let parsedSubterm = parse'(v, funcs, i+1, slice(xs, 0, j-1), [], [], false);

    parse'(v, funcs, j+1, trim(xs,j+1), stack @ [parsedSubterm], [parsedSubterm], tensor)


} and parseTensor = (v, funcs, i, xs, stack, tensor) => {
  
    if(tensor){
        parse'(v, funcs, i+1, xs, stack, [], tensor)
     } else {
        let j = scanForNextComposition(xs, 0);
                let parsedTensor = parse'(v, funcs, i+1, slice(xs,0,j-1), stack,[],  true);
                parse'(v, funcs, j, trim(xs,j), drop(stack,1) @ [parsedTensor], [], false);
     }

} and parseTrace = (m, v, funcs, i, xs, stack, tensor) => {
    
    let x = int_of_string(m[1]);
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "trace expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i+6, ")")
        let parsedTrace = parse'(v, funcs, i+6, slice(xs,1,j), [], [], false)
        let actualTrace = trace'(v,x,parsedTrace);
        parse'(v, funcs, i+6+j+1, trim(xs,j+2), stack @ [actualTrace], [actualTrace], tensor)
    }

} and parseIteration = (m, v, funcs, i, xs, stack, tensor) => {
    
    let x = int_of_string(m[1]);
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "iteration expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i+6, ")");
        let parsedIteration = parse'(v, funcs, i+6, slice(xs,1,j-1), [], [], false);
        let actualIteration = iter'(v,parsedIteration)
        parse'(v, funcs, i+6+j+1, trim(xs,j+2), stack @ [actualIteration], [actualIteration], tensor)
    }

} and parseTerm = (a, v, funcs, i, xs, stack, tensor) => {

    if(!tensor && List.length(stack) > 0 ){
        parseError(i, "unexpected term encountered, did you forget a composition or tensor?") 
    } else {
        let sym = v.parse(a); 
        let subterm = fst(sym) ? 
                        Value(snd(sym)) : 
                        switch(int_of_string(a)){
                        | item        => Identity(item)
                        | exception _ => functionLookup(a,funcs)
                        };
        parse'(v, funcs, i+1, xs, stack @ [subterm], [subterm], tensor)
    }  
} and parseExponential = (m, v, funcs, i, xs, stack, lastterm, tensor) => {

    let n = int_of_string(m[1]);

    if(List.length(lastterm) != 1){
        parseError(i, "exponential used without a valid term")
    } else {
        let newstack = processExponential(n,List.hd(lastterm),tensor);
        parse'(v, funcs, i+1, xs, drop(stack,1) @ newstack, lastterm, tensor)
    }

} and processExponential = (n, term, tensor) => {
    switch(n){ 
    | 0 => [term]
    | 1 => [term]
    | n => tensor ?
           [term, ...processExponential(n-1, term, tensor)] :
           [Tensor([term,...processExponential(n-1,term,true)])]
    }
}