open Lattices;
open Circuits;

let v = simpleLattice;

let andGate = Function({js|∧|js}, 2, 1, (v, c) => 
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(a,b))
                                            | _ => failwith("Bad input")
                                            }
                      )

let xorGate = Function({js|xor|js}, 2, 1, (v, c) =>     
                                            switch(c){
                                            | Tensor([Value(a),Value(b)]) => Value(v.andOp(v.notOp(v.andOp(a, b)), v.orOp(a,b)))
                                            | _ => failwith("Bad input");
                                            }
                        )

let halfAdder = (a, b) => {
    composemany(v,[
        tensor(v,[a, b]),
        dfork(v,2),
        tensor(v,[andGate, xorGate])
    ])
}