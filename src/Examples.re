/**
 * Sandbox file, containing examples
 */

open Circuits;
open Constructs;

let halfAdder = {
    macro(v, "hadd", "+_{half}", composemany([
        dfork(v,2),
        tensor([xorGate(v), andGate(v)])
    ]),[])
}

let fullAdder = {
    macro(v, "fadd", "+_{full}", composemany([
        tensor([dfork(v,2), idcirc(v,1)]),
        tensor([idcirc(v,2), swap(v,2,1)]),
        tensor([xorGate(v), idcirc(v,3)]),
        tensor([dfork(v,2), idcirc(v,2)]),
        tensor([xorGate(v), andGate(v), andGate(v)]),
        tensor([idcirc(v,1), orGate(v)])
    ]),[])
}

let flipFlop = {
    macro(v, "sr_nor", "SR_{\\downarrow}", composemany([
        trace(1,
        composemany([
            tensor([
                composemany([
                    norGate(v),
                    fork(v)
                ]),
                idcirc(v,1)
            ]),
            tensor([
                idcirc(v,1),
                composemany([
                    swap(v,1,1),
                    norGate(v),
                    fork(v)
                ])
            ]),
            tensor([
                swap(v,1,1),
                idcirc(v,1)
            ])
        ])),
        swap(v,1,1)
    ]), [])
}

let halfAdderApplied = composemany([
                            tensor([t, f]),
                            halfAdder
                       ])

let exampleFunctions = List.concat([specialMorphisms(v), 
            [id(v,1),
                funcBlackBox(v, "k", "\\text{k}", 1, 1),
                funcBlackBox(v, "F", "\\text{F}", 1, 1),
                funcBlackBox(v, "G", "\\text{G}", 1, 1), 
                funcBlackBox(v, "A", "\\text{A}", 2, 2), 
                funcBlackBox(v, "H", "\\text{H}", 1, 1), 
                funcBlackBox(v, "test", "\\text{test}", 3, 2), 
                funcBlackBox(v, "a", "\\text{a}", 1, 1),
                funcBlackBox(v, "b", "\\text{b}", 1, 1),
                funcBlackBox(v, "c", "\\text{c}", 1, 1),
                funcBlackBox(v, "d", "\\text{d}", 1, 1),
                funcBlackBox(v, "e", "\\text{e}", 2, 1),
                funcBlackBox(v, "bee", "\\text{b1}", 1, 3),
                andGate(v),
                orGate(v),
                xorGate(v),
                notGate(v),
                multiplexer(v),
                first(v),
                second(v),
                id(v,2),
]])

let exampleMacros = [halfAdder, fullAdder, flipFlop];

/*
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

let fullAdderApplied = composemany([
                            tensor([t,f,f]),
                            fullAdder
                        ])

let fullAdderReduced = evaluate(fullAdderApplied)

/*let exampleString = "Tr{1}((1 * t^2 * (t . id{1})^2) . 1 * x{2,2} . (1 * A * A) . (1 * AND * AND) . 1 * \\/)^2";*/
/*let exampleString = "t * H . A" */ 
/* let exampleString = "(A * A) . (AND * AND) . \\/" */
/*let exampleString = "Tr{1}(A))"*/
let exampleString = "\\x,y. x * 1 . 1 * y . fst"

let exampleCombinational = "f * t . Tr{1}((x{1,1} * /\\) . (/\\ * x{1,1} * 1) . (/\\ * (m . G . /\\) * 1) . (3 * x{1,1}) . (1 * (m . F) * 1) . (x{1,1} * 1) . (/\\ * 2)) . (x{1,1} * 1) . m"

let exampleTokenised = tokenise(exampleString);
let exampleParsed = parse(v, exampleFunctions, exampleTokenised);
let exampleCircuit = {v:v,c:fst(exampleParsed),l:[]};

/*let exampleTokenised = tokenise(exampleCombinational);
let exampleParsed = parse(v, exampleFunctions, exampleTokenised);
let exampleCircuit = {v:v,c:fst(exampleParsed)};*/

let rec evaluateStepNo = (circuit, n) => {
    Js.log(printCircuit(circuit));
    switch(n) {
    | 0 => circuit
    | n => evaluateStepNo(evaluateOneStep(circuit), n-1)
    }
}

let exampleReduced = exampleCircuit /*evaluateStepNo(exampleCircuit, 5)*/

/*let exampleReduced = evaluate(exampleCircuit);*/


*/