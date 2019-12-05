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

let exampleString = "(t * f * (t . id) * f) . (F * F) . (F * F)"
let exampleTokenised = tokenise(exampleString);
let exampleParsed = parse(v, exampleTokenised);

let printComponentList = (xs) => printList(xs, (x) => printComponent(v,x));