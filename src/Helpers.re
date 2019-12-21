/**
 * File containing helper functions for use 
 * throughout the project
 */

/* Assert but with a specific message */
let assert' = (condition, error, message) =>
    condition ? () : error(message);

let generateUnicodeSubscript = (n) => {
    let s1 = string_of_int(n);
    let s2 = ref("");
    for(i in 0 to String.length(s1) - 1){
        s2 := s2^ ++ "&#832" ++ String.make(1,s1.[i]) ++ ";"
    }
    s2^
}

/******************/
/* List functions */
/******************/

/* Helper function for split */
let rec split' = (n, xs, ys) => {
    switch(ys){
    | []         => (xs, [])
    | [y, ...yss] => n == 0 ? (xs, [y, ...yss]) : split'(n-1, List.concat([xs, [y]]), yss)
    }
}

/* Split a list into two lists at position n, with n in the second list */
let split = (n, xs) => split'(n, [], xs);

/* Print a list of elements
 *  xs: list('a)
 *  print : 'a -> string
 */ 
let rec printList = (xs, print) => { 
    let string = printList'(xs, print); 
    string == "" ? "[]" : "[ " ++ String.sub(string, 0, String.length(string) - 1) ++ " ]"
} and printList' = (xs, print) => {
    switch(xs){
    | [] => ""
    | [x,...xs] => let elem = print(x) == "" ? "_" : print(x); elem ++ " :: " ++ printList'(xs,print) 
    }
}

/* Print a list of elements with commas and no surrounding brackets
 *  xs: list('a)
 *  print : 'a -> string
 */ 
let rec printListCommas = (xs, print) => { 
    let string = printListCommas'(xs, print); 
    string == "" ? "" : String.sub(string, 0, String.length(string) - 2)
} and printListCommas' = (xs, print) => {
    switch(xs){
    | [] => ""
    | [x,...xs] => let elem = print(x) == "" ? "_" : print(x); elem ++ ", " ++ printListCommas'(xs,print) 
    }
}

let printStringList = (xs) => printList(xs, (x) => x);
let printIntList = (xs) => printList(xs, (x) => string_of_int(x));
let printCharList = (xs) => printList(xs, (x) => String.make(1,x));

/* Generate a list ranging from 0 to a number (non-inclusive) */
let rec range = (n) => List.rev(range'(n))
and range' = (n) => {
    switch(n){
    | 0 => [0]
    | n => [n, ...range'(n-1)]
    };
}

/* Add an element to the end of a list */
let addToEnd = (n, xs) => {
    List.concat([xs, [n]]);
}

/* Take a slice of a list between elements a (inclusive) and b (exclusive) */ 
let rec slice = (xs, a, b) => slice'(xs, a, b, 0)
and slice' = (xs, a, b, n) => {
    switch(xs){
    | [] => failwith("not enough list!")
    | [x,...xs] => n < a ? slice'(xs, a, b, n+1) : ((n < b) ? [x,...slice'(xs, a, b, n+1)] : [x])               
    }
}

/* Drop the last b elements of a list */
let rec drop = (xs, b) => drop'(xs, List.length(xs) - b, 0)
and drop' = (xs, b, n) => {
    switch(xs){
    | [] => failwith("not enough list!")
    | [x,...xs] => n == b ? [] : [x,...drop'(xs, b, n + 1)]
    }
}

/* Trim the first b elements of a list */
let rec trim = (xs, b) => trim'(xs, b-1, 0)
and trim' = (xs, b, n) => {
    switch(xs){
    | [] => failwith("not enough list!")
    | [x,...xs] => n == b ? xs : trim'(xs, b, n + 1)      
    }
}

/*******************/
/* Array functions */
/*******************/

let printArray = (a, f) => {
    let string = ref("{");
    for(i in 0 to Array.length(a) - 1) {
        string := string^ ++ ", " ++ f(a[i]);
    }
    string^ ++ "}"
}