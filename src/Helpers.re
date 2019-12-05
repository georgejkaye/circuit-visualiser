let assert' = (condition, message) =>
    condition ? () : failwith(message);

let rec slice = (string, n) => slice'(string, n, 0)
and slice' = (string, n, x) => {
    switch(string){
    | "" => ""
    | st => switch(n){
            | 0 => (String.make(1, st.[n])) ++ slice'(string, 0, x+1) 
            | n => slice'(string, n-1, x+1)
            }
    }
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

let rec printList = (xs, print) => { 
    let string = printList'(xs, print); 
    string == "" ? "[]" : "[" ++ String.sub(string, 0, String.length(string) - 1) ++ "]]"
} and printList' = (xs, print) => {
    switch(xs){
    | [] => ""
    | [x,...xs] => let elem = print(x) == "" ? "_" : print(x); elem ++ " :: " ++ printList(xs,print) 
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

let addToEnd = (n, xs) => {
    List.concat([xs, [n]]);
}

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