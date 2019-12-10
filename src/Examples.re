open Circuits;
open Rewrites;
open Constructs;
open Parser;
open Helpers;

/* v = Bottom, (False, True), Top */

let halfAdder = {
    composemany([
        dfork(v,2),
        tensor([xorGate(v), andGate(v)])
    ])
}

let halfAdderApplied = composemany([
                            tensor([t, f]),
                            halfAdder
                       ])

let halfAdderReduced_1 = evaluateOneStep(halfAdderApplied);
let halfAdderReduced_2 = evaluateOneStep(halfAdderReduced_1);
let halfAdderReduced_3 = evaluateOneStep(halfAdderReduced_2);
let halfAdderReduced_4 = evaluateOneStep(halfAdderReduced_3);
let halfAdderReduced_5 = evaluateOneStep(halfAdderReduced_4);
let halfAdderReduced_6 = evaluateOneStep(halfAdderReduced_5);
let halfAdderReduced_7 = evaluateOneStep(halfAdderReduced_6);
let halfAdderReduced_8 = evaluateOneStep(halfAdderReduced_7);
let halfAdderReduced_9 = evaluateOneStep(halfAdderReduced_8);
let halfAdderReduced_10 = evaluateOneStep(halfAdderReduced_9);
let halfAdderReduced_11 = evaluateOneStep(halfAdderReduced_10);
let halfAdderReduced_12 = evaluateOneStep(halfAdderReduced_11);

let fullAdder = {
    composemany([
        tensor([dfork(v,2), identity(v,1)]),
        swap(v,2,1),
        tensor([xorGate(v), identity(v,3)]),
        tensor([dfork(v,2), identity(v,2)]),
        tensor([xorGate(v), andGate(v), andGate(v)]),
        tensor([identity(v,1), orGate(v)])
    ])
}

let fullAdderApplied = composemany([
                            tensor([t,f,f]),
                            fullAdder
                        ])

let fullAdderReduced = evaluate(fullAdderApplied)

let multiplexer = func'("m", 3, 1, (v,c) => switch(c){
                                                | Tensor([Value(c),a,b]) => c == Lattices.True ? a : b 
                                                | _ => failwith("Bad input")
                                                }
                  )

let exampleFunctions = List.concat([specialMorphisms(v), 
                                    [id(v,1).c,
                                     funcBlackBox(v, "F", 1, 1).c,
                                     funcBlackBox(v, "G", 1, 1).c, 
                                     funcBlackBox(v, "A", 2, 2).c, 
                                     funcBlackBox(v, "H", 1, 2).c, 
                                     andGate(v).c,
                                     orGate(v).c,
                                     multiplexer
                                    ]])

let exampleString = "(t^2 * (t . id{1}) * f) . x{2,2} . (A * A) . (AND * AND) . \\/";
/*let exampleString = "t * H . A" */ 
/* let exampleString = "(A * A) . (AND * AND) . \\/" */
/*let exampleString = "Tr{1}(A))"*/

let exampleCombinational = "Tr{1}((x{1,1} * /\\) . (/\\ * x{1,1} * 1) . (/\\ * (m . G . /\\) * 1) . (3 * x{1,1}) . (1 * (m . F) * 1) . (x{1,1} * 1) . (/\\ * 2)) . (x{1,1} * 1) . m"

let exampleTokenised = tokenise(exampleString);
let exampleParsed = parse(v, exampleFunctions, exampleTokenised);
let exampleCircuit = {v:v,c:exampleParsed};

/*let exampleTokenised = tokenise(exampleCombinational);
let exampleParsed = parse(v, exampleFunctions, exampleTokenised);
let exampleCircuit = {v:v,c:exampleParsed};*/

let exampleReduced = evaluate(exampleCircuit);


