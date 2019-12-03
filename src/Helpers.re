let assert' = (condition, message) =>
    condition ? () : failwith(message);

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
    switch(xs){
    | [] => ""
    | [x,...xs] => print(x) ++ ", " ++ printList(xs,print) 
    }
}

/* Generate a list ranging from 0 to a number (non-inclusive) */
let rec range = (n) => {
    let xs = switch(n){
    | 0 => []
    | n => [n, ...range(n-1)]
    };
    List.rev(xs)
}