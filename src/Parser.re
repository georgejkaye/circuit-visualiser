/**
 * File containing functions for tokenising and
 * subsquently parsing user input into circuits
 */

open Helpers;
open Circuits;
open Lattices;

exception ParseError(string);

/* Produce a parse error at character i */
let parseError = (i, message) => raise(ParseError("parse error, char " ++ string_of_int(i) ++ ": " ++ message));

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

let rec lengthOfTokens = (tokens) => {
    switch(tokens){
    | [] => 0
    | [x,...xs] => String.length(x) + lengthOfTokens(xs)
    }
}

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
    | ['.', ...xs] => List.length(current) == 0 ? tokenise'(xs, ['.']) : [List.rev(['.',...current]), ...tokenise'(xs, [])]
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
    | []             => bracket == "" ? j : parseError(i, bracket ++ " expected")
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
                        | Function(id,_,_,_,_) => (id == func)
                        | _ => failwith("Unexpected non-function found in function library")
                        }, funcs)){
                            | item => Some(item)
                            | exception Not_found => None
                        }
                        
}

let linkLookup = (link,links) => {
    switch(List.find((x) => fst(fst(x)) == link, links)){
    | item => Some(Output(snd(fst(item))))
    | exception Not_found => switch(List.find((x) => fst(snd(x)) == link, links)){
                             | item => Some(Input(snd(snd(item))))
                             | exception Not_found => None 
                             }
    }
}

/* Parses a set of tokens into a circuit. 
 * v: lattice
 * funcs: function library
 * tokens: the list of tokens 
 */
let rec parse = (v, funcs, tokens) => parse'(v, funcs, 1, tokens, [], [], false, 0, [])
and parse' = (v, funcs, i, tokens, stack, lastterm, tensor, nextlink, links) => {
    Js.log("parsing " ++ printStringList(tokens));
    switch(tokens){
        | [] => tensor ? 
                    (Tensor(stack), (nextlink, links)) : 
                    (List.length(stack) == 1 ? 
                        (List.hd(stack), (nextlink, links)) : 
                        parseError(i, "unexpected end of term")
                    )
        | [x, ...xs] => switch(x){
                                | "("   => parseBrackets(")", v, funcs, i, xs, stack, tensor, nextlink, links)
                                | "["   => parseBrackets("]", v, funcs, i, xs, stack, tensor, nextlink, links)
                                | ")"   => parseError(i, "unexpected ) encountered")
                                | "]"   => parseError(i, "unexpected ] encountered")
                                | "."   => parseComposition(v, funcs, i, xs, stack, tensor, nextlink, links)
                                | "*"   => parseTensor(v, funcs, i, xs, stack, tensor, nextlink, links)
                                | "/\\" => parse'(v, funcs, i, [{js|⋏|js},...xs], stack, [], tensor, nextlink, links)
                                | "\\/" => parse'(v, funcs, i, [{js|⋎|js},...xs], stack, [], tensor, nextlink, links)
                                | a     =>  let matches = checkForMatches(a);
                                            let m = snd(matches);
                                            let len = String.length(a);
                                            switch(fst(matches)){
                                                | 0  => let x = int_of_string(m[1]);
                                                        let y = int_of_string(m[2]);
                                                        parse'(v, funcs, i + 1 + len, xs, stack @ [swap(v,x,y).c], [swap(v,x,y).c], tensor, nextlink, links)
                                                | 1  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i + 1 + len, xs, stack @ [dfork(v,x).c], [dfork(v,x).c], tensor, nextlink, links)
                                                | 2  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i + 1 + len, xs, stack @ [djoin(v,x).c], [djoin(v,x).c], tensor, nextlink, links)
                                                | 3  => let x = int_of_string(m[1]);
                                                        parse'(v, funcs, i + 1 + len, xs, stack @ [delay(v,x).c], [delay(v,x).c], tensor, nextlink, links) 
                                                | 4  => parseTrace(m, v, funcs, i, xs, stack, tensor, nextlink, links)
                                                | 5  => parseIteration(m, v, funcs, i, xs, stack, tensor, nextlink, links)
                                                | 6  => parseIteration(m, v, funcs, i, xs, stack, tensor, nextlink, links)
                                                | 7  => parseExponential(m, v, funcs, i, xs, stack, lastterm, tensor, nextlink, links)
                                                | 8  => parseLink(m, v, funcs, i, xs, stack, tensor, nextlink, links)
                                                | 9  => parseLink(m, v, funcs, i, xs, stack, tensor, nextlink, links)
                                                | -1 => parseTerm(a, v, funcs, i, xs, stack, tensor, nextlink, links)
                                            }
                                        }
    }
} and parseBrackets = (close, v, funcs, i, xs, stack, tensor, nextlink, links) => {
    
    let j = scanForClosingBracket(xs, i, close);

    let subtermTokens = slice(xs, 0, j-1);

    let parsedSubterm = parse'(v, funcs, i+1, subtermTokens, [], [], false, nextlink, links);

    let actualSubterm = fst(parsedSubterm)
    let nextlink = fst(snd(parsedSubterm))
    let links = snd(snd(parsedSubterm))

    parse'(v, funcs, i + 1 + lengthOfTokens(subtermTokens), trim(xs,j+1), stack @ [actualSubterm], [actualSubterm], tensor, nextlink, links)

} and parseComposition = (v, funcs, i, xs, stack, tensor, nextlink, links) => {

    if(List.length(stack) == 0){
        parseError(i, "unexpected * encountered")
    } else {
        let parsedArgument = parse'(v, funcs, i+2, xs, List.tl(stack), [], tensor, nextlink, links);
        
        let actualArgument = fst(parsedArgument);
        let nextlink = fst(snd(parsedArgument));
        let links = snd(snd(parsedArgument));

        (compose'(v, List.hd(stack), actualArgument), (nextlink, links))
    }

} and parseTensor = (v, funcs, i, xs, stack, tensor, nextlink, links) => {
  
    if(tensor){
        parse'(v, funcs, i+2, xs, stack, [], tensor, nextlink, links)
     } else {
        let j = scanForNextComposition(xs, 0);
                let tensorTokens = slice(xs,0,j-1);
                let parsedTensor = parse'(v, funcs, i+2, tensorTokens, stack,[],  true, nextlink, links);

                let actualTensor = fst(parsedTensor);
                let nextlink = fst(snd(parsedTensor));
                let links = snd(snd(parsedTensor));

                parse'(v, funcs, i + 2 + lengthOfTokens(tensorTokens), trim(xs,j), drop(stack,1) @ [actualTensor], [], false, nextlink, links);
     }

} and parseTrace = (m, v, funcs, i, xs, stack, tensor, nextlink, links) => {
    
    let x = int_of_string(m[1]);
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "trace expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i + 2 + String.length(m[0]), ")")

        let traceTokens = slice(xs,1,j);

        let parsedTrace = parse'(v, funcs, i + 1 + 1 + String.length(m[0]) , traceTokens, [], [], false, nextlink, links)
        
        let actualTrace = trace'(v,x,fst(parsedTrace));
        let nextlink = fst(snd(parsedTrace));
        let links = snd(snd(parsedTrace));
        
        parse'(v, funcs, i + 1 + 1 + String.length(m[0]) + lengthOfTokens(traceTokens) + 1, trim(xs,j+2), stack @ [actualTrace], [actualTrace], tensor, nextlink, links)
    }

} and parseIteration = (m, v, funcs, i, xs, stack, tensor, nextlink, links) => {
    
    let x = int_of_string(m[1]);
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "iteration expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i+6, ")");
        let iterationTokens = slice(xs,1,j-1);
        let parsedIteration = parse'(v, funcs, i + 1 + 1 + String.length(m[0]), iterationTokens, [], [], false, nextlink, links);
        
        let actualIteration = iter'(v,fst(parsedIteration));
        let nextlink = fst(snd(parsedIteration));
        let links = snd(snd(parsedIteration));

        parse'(v, funcs, i + 1 + 1 + String.length(m[0]) + lengthOfTokens(iterationTokens) + 1, trim(xs,j+2), stack @ [actualIteration], [actualIteration], tensor, nextlink, links)
    }

} and parseTerm = (a, v, funcs, i, xs, stack, tensor, nextlink, links) => {

    if(!tensor && List.length(stack) > 0 ){
        parseError(i, "unexpected term encountered, did you forget a composition or tensor?") 
    } else {
        let sym = v.parse(a); 
        let subterm = fst(sym) ? 
                        Value(snd(sym)) : 
                        switch(int_of_string(a)){
                        | item        => Identity(item)
                        | exception _ => switch(functionLookup(a,funcs)){
                                        | Some(x) => x
                                        | None    => switch(linkLookup(a, links)){
                                                     | Some(x) => x
                                                     | None    => parseError(i,"unable to parse " ++ a)
                                                     }
                                        }
                        };
        parse'(v, funcs, i+String.length(a)+1, xs, stack @ [subterm], [subterm], tensor, nextlink, links)
    }  
} and parseExponential = (m, v, funcs, i, xs, stack, lastterm, tensor, nextlink, links) => {

    let n = int_of_string(m[1]);

    if(List.length(lastterm) != 1){
        parseError(i, "exponential used without a valid term")
    } else {
        let newstack = processExponential(n,List.hd(lastterm),tensor);
        parse'(v, funcs, i+String.length(m[0]) + 1, xs, drop(stack,1) @ newstack, lastterm, tensor, nextlink, links)
    }

} and processExponential = (n, term, tensor) => {
    switch(n){ 
    | 0 => [term]
    | 1 => [term]
    | n => tensor ?
           [term, ...processExponential(n-1, term, tensor)] :
           [Tensor([term,...processExponential(n-1,term,true)])]
    }
} and parseLink = (m, v, funcs, i, xs, stack, tensor, nextlink, links) => {

    let oup = m[1];
    let inp = m[2];

    let oux = nextlink;
    let inx = nextlink + 1;

    let newLinks = [((oup, oux), (inp, inx)),...links]
    let nextlink = nextlink + 2

    let j = scanForClosingBracket(List.tl(xs), i+4, "")
    let parsedScope = parse'(v, funcs, i+6, slice(xs,0,j), [], [], false, nextlink, newLinks)

    let actualScope = fst(parsedScope);
    let nextlink = fst(snd(parsedScope));
    let links = snd(snd(parsedScope));

    let finalLink = Link(oux, inx, actualScope)

    parse'(v, funcs, i+1, trim(xs, j+1), stack @ [finalLink], [finalLink], tensor, nextlink, links)

}

let parseFromString = (v, funcs, string) => {
    let tokenisedString = tokenise(string);
    let parsedString = parse(v, funcs, tokenisedString);

    {v:v, c:fst(parsedString)}

}