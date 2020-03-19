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

let openingBracketsRegEx = [%bs.re "/(\\(|\\[])/"]
let closingBracketsRegEx = [%bs.re "/(\\)|\\])(\\^[0-9]+)?/"]
let roundBracketsClosingRegEx   = [%bs.re "/(\\))(\\^[0-9]+)?/"]
let squareBracketsClosingRegEx   = [%bs.re "/(\\])(\\^[0-9]+)?/"]

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
                     | Some(_) => scanForNextComposition'(xs, i+1, bracks-1)
                     | None    => scanForNextComposition'(xs, i+1, bracks)
                     }
    }
}

/* Looks up a string in the function library to see if it is a function or not */ 
let functionLookup = (func,funcs) => {
    switch(List.find((x) => switch(x.c){
                        | Function(id,_,_,_,_) => (id == func)
                        | _ => failwith("Unexpected non-function found in function library")
                        }, funcs)){
                            | item => Some(item.c)
                            | exception Not_found => None
                        }
                        
}

let macroLookup = (macro,macros) => {
    switch(List.find((x) => switch(x.c){
                        | Macro(id,_,_) => (id == macro)
                        | _ => failwith("Unexpected non-macro found in function library")
                        }, macros)){
                            | item => Some(item.c)
                            | exception Not_found => None
                        }
                        
}

let linkLookup = (link,links) => {
    switch(List.find((x) => fst(fst(x)) == link, links)){
    | item => Some(Outlink(snd(fst(item))))
    | exception Not_found => switch(List.find((x) => fst(snd(x)) == link, links)){
                             | item => Some(Inlink(snd(snd(item))))
                             | exception Not_found => None 
                             }
    }
}

/* Parses a set of tokens into a circuit. 
 * v: lattice
 * funcs: function library
 * macros: macro library
 * tokens: the list of tokens 
 */
let rec parse = (v, funcs, macros, tokens) => parse'(v, 1, tokens, [], [], false, 0, (funcs,macros), [])
and parse' = (v, i, tokens, stack, lastterm, tensor, nextlink, defs, links) => {
    switch(tokens){
        | [] => tensor ? 
                    (circ(v,Tensor(stack), links), nextlink) : 
                    (List.length(stack) == 1 ? 
                        (List.hd(stack), nextlink) : 
                        parseError(i, "unexpected end of term")
                    )
        | [x, ...xs] => switch(x){
                                | "("   => parseBrackets(")", v, i, xs, stack, tensor, nextlink, defs, links)
                                | "["   => parseBrackets("]", v, i, xs, stack, tensor, nextlink, defs, links)
                                | ")"   => parseError(i, "unexpected ) encountered")
                                | "]"   => parseError(i, "unexpected ] encountered")
                                | "."   => parseComposition(v, i, xs, stack, tensor, nextlink, defs, links)
                                | "*"   => parseTensor(v, i, xs, stack, tensor, nextlink, defs, links)
                                | "/\\" => parse'(v, i, [{js|⋏|js},...xs], stack, [], tensor, nextlink, defs, links)
                                | "\\/" => parse'(v, i, [{js|⋎|js},...xs], stack, [], tensor, nextlink, defs, links)
                                | a     =>  let matches = checkForMatches(a);
                                            let m = snd(matches);
                                            let len = String.length(a);
                                            switch(fst(matches)){
                                                | 0  => let x = int_of_string(m[1]);
                                                        let y = int_of_string(m[2]);
                                                        parse'(v,  i + 1 + len, xs, stack @ [swap(v,x,y)], [swap(v,x,y)], tensor, nextlink, defs, links)
                                                | 1  => let x = int_of_string(m[1]);
                                                        parse'(v, i + 1 + len, xs, stack @ [dfork(v,x)], [dfork(v,x)], tensor, nextlink, defs, links)
                                                | 2  => let x = int_of_string(m[1]);
                                                        parse'(v, i + 1 + len, xs, stack @ [djoin(v,x)], [djoin(v,x)], tensor, nextlink, defs, links)
                                                | 3  => let x = int_of_string(m[1]);
                                                        parse'(v, i + 1 + len, xs, stack @ [delay(v,x)], [delay(v,x)], tensor, nextlink, defs, links) 
                                                | 4  => parseTrace(m, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | 5  => parseIteration(m, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | 6  => parseIteration(m, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | 7  => parseExponential(m, v, i, xs, stack, lastterm, tensor, nextlink, defs, links)
                                                | 8  => parseLink(m, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | 9  => parseLink(m, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | -1 => parseTerm(a, v, i, xs, stack, tensor, nextlink, defs, links)
                                                | _ => failwith("Bad regex match code")
                                            }
                                        }
    }
} and parseBrackets = (close, v, i, xs, stack, tensor, nextlink, defs, links) => {
    
    let j = scanForClosingBracket(xs, i, close);

    let subtermTokens = slice(xs, 0, j-1);

    let parsedSubterm = parse'(v, i+1, subtermTokens, [], [], false, nextlink, defs, links);

    let actualSubterm = fst(parsedSubterm)
    let nextlink = snd(parsedSubterm)

    parse'(v, i + 1 + lengthOfTokens(subtermTokens), trim(xs,j+1), stack @ [actualSubterm], [actualSubterm], tensor, nextlink, defs, links)

} and parseComposition = (v, i, xs, stack, tensor, nextlink, defs, links) => {

    if(List.length(stack) == 0){
        parseError(i, "Unexpected . encountered")
    } else if(List.length(xs) == 0){
            parseError(i, "Unexpected end of term encountered after .")
    };
    
    let parsedArgument = parse'(v, i+2, xs, List.tl(stack), [], tensor, nextlink, defs, links);
    
    let actualArgument = fst(parsedArgument);
    let nextlink = snd(parsedArgument);

    (compose(List.hd(stack), actualArgument), nextlink)

} and parseTensor = (v, i, xs, stack, tensor, nextlink, defs, links) => {
  
    if(List.length(stack) == 0){
        parseError(i, "Unexpected * encountered")
    } else if(List.length(xs) == 0){
        parseError(i, "Unexpected end of term encountered after *")
    };

    if(tensor){
        parse'(v, i+2, xs, stack, [], tensor, nextlink, defs, links)
     } else {
        let j = scanForNextComposition(xs, 0);
                let tensorTokens = slice(xs,0,j-1);
                let parsedTensor = parse'(v, i+2, tensorTokens, stack,[],  true, nextlink, defs, links);

                let actualTensor = fst(parsedTensor);
                let nextlink = snd(parsedTensor);

                parse'(v, i + 2 + lengthOfTokens(tensorTokens), trim(xs,j), drop(stack,1) @ [actualTensor], [], false, nextlink, defs, links);
     }

} and parseTrace = (m, v, i, xs, stack, tensor, nextlink, defs, links) => {
    
    let x = int_of_string(m[1]);
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "trace expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i + 2 + String.length(m[0]), ")")

        let traceTokens = slice(xs,1,j);

        let parsedTrace = parse'(v, i + 1 + 1 + String.length(m[0]) , traceTokens, [], [], false, nextlink, defs, links)
        
        let nextlink = snd(parsedTrace);
        let actualTrace = trace(x,fst(parsedTrace));
        
        parse'(v, i + 1 + 1 + String.length(m[0]) + lengthOfTokens(traceTokens) + 1, trim(xs,j+2), stack @ [actualTrace], [actualTrace], tensor, nextlink, defs, links)
    }

} and parseIteration = (m, v, i, xs, stack, tensor, nextlink, defs, links) => {
    
    /*let x = int_of_string(m[1]);*/
    if(List.length(xs) == 0 || List.hd(xs) != "("){ 
        parseError(i, "iteration expected, no expression found")
    } else {
        let j = scanForClosingBracket(List.tl(xs), i+6, ")");
        let iterationTokens = slice(xs,1,j);
        let parsedIteration = parse'(v, i + 1 + 1 + String.length(m[0]), iterationTokens, [], [], false, nextlink, defs, links);
        
        let nextlink = snd(parsedIteration)
        let actualIteration = iter(fst(parsedIteration));
        
        parse'(v, i + 1 + 1 + String.length(m[0]) + lengthOfTokens(iterationTokens) + 1, trim(xs,j+2), stack @ [actualIteration], [actualIteration], tensor, nextlink, defs, links)
    }

} and parseTerm = (a, v, i, xs, stack, tensor, nextlink, defs, links) => {

    if(!tensor && List.length(stack) > 0 ){
        parseError(i, "unexpected term encountered, did you forget a composition or tensor?") 
    } else {
        let sym = v.parse(a); 
        let subterm = fst(sym) ? 
                        Value(snd(sym)) : 
                        switch(int_of_string(a)){
                        | item        => Identity(item)
                        | exception _ => switch(functionLookup(a,fst(defs))){
                                        | Some(x) => x
                                        | None    => switch(macroLookup(a, snd(defs))){
                                                        | Some(x) => x
                                                        | None    => switch(linkLookup(a, links)){
                                                                        | Some(x) => x
                                                                        | None    => parseError(i,"unable to parse " ++ a)
                                                                        }
                                                        }
                                        }
                        }
        let subterm = circ(v,subterm,links);
        parse'(v, i+String.length(a)+1, xs, stack @ [subterm], [subterm], tensor, nextlink, defs, links)
    }  
} and parseExponential = (m, v, i, xs, stack, lastterm, tensor, nextlink, defs, links) => {

    let n = int_of_string(m[1]);

    if(List.length(lastterm) != 1){
        parseError(i, "exponential used without a valid term")
    } else {
        let newstack = processExponential(v,links,n,List.hd(lastterm),tensor);
        parse'(v, i+String.length(m[0]) + 1, xs, drop(stack,1) @ newstack, lastterm, tensor, nextlink, defs, links)
    }

} and processExponential = (v, links, n, term, tensor) => {
    switch(n){ 
    | 0 => [term]
    | 1 => [term]
    | n => tensor ?
           [term, ...processExponential(v,links,n-1, term, tensor)] :
           [circ(v, Tensor([term,...processExponential(v,links,n-1,term,true)]), links)]
    }
} and parseLink = (m, v, i, xs, stack, tensor, nextlink, defs, links) => {

    /* Strings representing the links */
    let oup = m[1];
    let inp = m[2];

    /* Check that we don't have duplicate links */
    if(oup == inp) {
        semanticsError("Outlink and Inlink cannot be the same string!")
    } else if(doesLinkStringExist(oup,links)){
        semanticsError("Link " ++ oup ++ " already exists!")
    } else if (doesLinkStringExist(inp, links)){
        semanticsError("Link " ++ inp ++ " already exists!")
    }

    /* Numbers representing the links - how links are resolved */
    let oux = nextlink;
    let inx = nextlink + 1;

    let newLinks = [((oup, oux), (inp, inx)),...links]
    let nextlink = nextlink + 2

    let parsedScope = parse'(v, i+6, xs, [], [], false, nextlink, defs, newLinks)

    let actualScope = fst(parsedScope);
    let nextlink = snd(parsedScope);

    let finalLink = link(v, oux, inx, actualScope, newLinks)

    parse'(v, i+1, [], stack @ [finalLink], [finalLink], tensor, nextlink, defs, links)
}

let parseFromString = (v, funcs, macros, string) => {
    let tokenisedString = tokenise(string);
    let parsedString = parse(v, funcs, macros, tokenisedString);

    fst(parsedString)
}