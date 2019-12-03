open Circuits;
open Rewrites;
open Lattices;
open Examples;
open Helpers;

/* v = Bottom, (False, True), Top */

let halfAdder = {
    composemany([
        dfork(v,2),
        tensor([andGate(v), xorGate(v)])
    ])
}

let test1 = composemany([
               tensor([t,f,bot]),
               dfork(v,3),
               id(v,6)
            ])

let test2 = composemany([
                tensor([t,f]),
                tensor([notGate(v),notGate(v)])
])

let test2Reduced = evaluateOneStep(test2)

let basicTrace = trace(1,id(v,2))
let traceTwo = trace(2, id(v,3))
let basicTraceAsIteration = traceAsIteration(basicTrace);
let traceTwoAsIteration = traceAsIteration(traceTwo);

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