open Circuits;
open Rewrites;
open Lattices;
open Examples;
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