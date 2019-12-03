open Circuits;
open Examples;

let halfAdder = {
    composemany([
        dfork(v,2),
        tensor([andGate(v), xorGate(v)])
    ])
}

let basicTrace = trace(1,id(v,2))
let basicTraceAsIteration = traceAsIteration(basicTrace);

Js.log(printCircuit(basicTrace));
Js.log(printCircuit(basicTraceAsIteration));