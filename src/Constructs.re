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

let andGate = (v) => {v:v, c:Function({js|AND|js}, 2, 1, (v, c) => 
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(a,b)) 
                                            | Tensor([a, b])                => Function(printComponent(v,a) ++ " AND " ++ printComponent(v,b), 
                                                                                        inputs'(a) + inputs'(b), 
                                                                                        1, 
                                                                                        (_,_) => failwith("not implemented")
                                                                             ) 
                                            | _ => failwith("Bad input")
                                            }
                                     )
                     }

let orGate = (v) => {v:v, c:Function({js|OR|js}, 2, 1, (v, c) => 
                                          switch(c){
                                          | Tensor([Value(a),Value(b)]) => Value(v.orOp(a,b)) 
                                          | Tensor([a, b])                => Function(printComponent(v,a) ++ " OR " ++ printComponent(v,b), 
                                                                                           inputs'(a) + inputs'(b), 
                                                                                           1, 
                                                                                           (_,_) => failwith("not implemented")
                                                                             ) 
                                          | _ => failwith("Bad input")
                                          }
                                   )
                     }

let xorGate = (v) => {v:v, c:Function({js|XOR|js}, 2, 1, (v, c) =>     
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(v.notOp(v.andOp(a, b)), v.orOp(a,b)))
                                            | _ => failwith("Bad input");
                                            }
                                     )
                     }

let notGate = (v) => {v:v, c:Function({js|NOT|js}, 1, 1, (v, c) =>
                                                 switch(c){
                                                 | Value(a) => Value(v.notOp(a))
                                                 })}

let id = (v, n) => func(v,"id{" ++ string_of_int(n) ++ "}",n,n, (_,c) => c)