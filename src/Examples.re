open Lattices;
open Circuits;

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
    composemany([
        Tensor([a, b]),
        dfork(2),
        Tensor([andGate, xorGate])
    ])
}