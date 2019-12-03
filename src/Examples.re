open Lattices;
open Circuits;

let v = simpleLattice;

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

let xorGate = (v) => {v:v, c:Function({js|XOR|js}, 2, 1, (v, c) =>     
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(v.notOp(v.andOp(a, b)), v.orOp(a,b)))
                                            | _ => failwith("Bad input");
                                            }
                                     )
                     }

let id = (v, n) => func(v,"id",n,n, (_,c) => c)