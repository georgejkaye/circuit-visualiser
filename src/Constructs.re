/**
 * File containing several useful constructs,
 * such as common logic gates and multiplexers
 */

open Lattices;
open Circuits;

let v = simpleLattice;
let t = value(v,t);
let f = value(v,f);
let bot = value(v,bot);
let top = value(v,top);

let andGate = (v) => {v:v, c:Function({js|AND|js}, "\\wedge", 2, 1, (v, c) => 
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(a,b)) 
                                            | Tensor([a, b])                => Function(printComponent(v,a) ++ " AND " ++ printComponent(v,b),
                                                                                        printComponentLatex(v,a) ++ " \\wedge " ++ printComponent(v,b),
                                                                                        inputs'(a) + inputs'(b), 
                                                                                        1, 
                                                                                        (_,_) => failwith("not implemented")
                                                                             ) 
                                            | _ => failwith("Bad input")
                                            }
                                     )
                     }

let orGate = (v) => {v:v, c:Function({js|OR|js}, "\\vee", 2, 1, (v, c) => 
                                          switch(c){
                                          | Tensor([Value(a),Value(b)]) => Value(v.orOp(a,b)) 
                                          | Tensor([a, b])                => Function(printComponent(v,a) ++ " OR " ++ printComponent(v,b),
                                                                                           printComponentLatex(v,a) ++ " \\vee " ++ printComponentLatex(v,b),  
                                                                                           inputs'(a) + inputs'(b), 
                                                                                           1, 
                                                                                           (_,_) => failwith("not implemented")
                                                                             ) 
                                          | _ => failwith("Bad input")
                                          }
                                   )
                     }

let xorGate = (v) => {v:v, c:Function({js|XOR|js}, "\\veebar", 2, 1, (v, c) =>     
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(v.notOp(v.andOp(a, b)), v.orOp(a,b)))
                                            | Tensor([a, b])                => Function(printComponent(v,a) ++ " XOR " ++ printComponent(v,b),
                                                                                           printComponentLatex(v,a) ++ " \\veebar " ++ printComponentLatex(v,b),  
                                                                                           inputs'(a) + inputs'(b), 
                                                                                           1, 
                                                                                           (_,_) => failwith("not implemented")
                                                                             ) 
                                            | _ => failwith("Bad input")
                                            }
                                     )
                     }

let notGate = (v) => {v:v, c:Function({js|NOT|js}, "\\neg", 1, 1, (v, c) =>
                                                 switch(c){
                                                 | Value(a) => Value(v.notOp(a))
                                                 | a        => Function("¬" ++ printComponent(v,a),
                                                                             "\\neg " ++ printComponentLatex(v,a),
                                                                             inputs'(a), 1, (_,_) => failwith("not implemented"))
                                                 })}

let id = (v, n) => func(v,"id{" ++ string_of_int(n) ++ "}", "\\text{id}_" ++ string_of_int(n), n,n, (_,c) => c)

let first = (v) => func(v, "fst", "\\text{fst}", 2, 1, (_,c) => switch(c){
                                          | Tensor([a,b]) => a
                                          | _ => failwith("Bad input")
                                          })

let second = (v) => func(v, "snd", "\\text{snd}", 2, 1, (_,c) => switch(c){
                                                        | Tensor([a,b]) => b
                                                        | _ => failwith("Bad input")
                                                  }
       )

let multiplexer = (v) => func(v, "m", "\\text{m}", 3, 1, (_,c) => switch(c){
                                                        | Tensor([Value(c),a,b]) => List.exists(((x) => x == c), v.highValues) ? a : b 
                                                        | _ => failwith("Bad input")
                                                     }
)