open Circuits;

let andGate = Function("AND", 2, 1);
let orGate = Function("OR", 2, 1);
let t = Value(True);
let f = Value(False);

let tensor1 = Tensor([t,t,t]);
let tensor2 = Tensor([andGate, Identity(1)]);

let circuit = composemany([tensor1,tensor2,orGate])

printCircuit(Circuit([],[],circuit, "My first circuit"));

let f = Function("f", 1, 1);
let g = Function("g", 1, 1);
let h = Function("h", 2, 1);
let delay = Delay;

let tensor3 = Tensor([delay,g]);
let tensor4 = Tensor([f,Identity(1)]);

let circuit2 = composemany([fork, tensor3, tensor4, h]);

printCircuit(Circuit([],[],circuit2,"Second wind"));

let f' = Function("f", 3, 3);
let circuit3 = trace(2, f');

printCircuit(Circuit([],[],circuit3,"Trace"));

let circuit4 = traceAsIteration(circuit3);
printCircuit(makeCircuit(circuit4, "Iteration"));