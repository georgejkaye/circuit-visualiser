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