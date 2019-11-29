open Circuits;

let andGate = Function("AND", 2, 1, id);
let orGate = Function("OR", 2, 1, id);
let t = Value(True);
let f = Value(False);

let tensor1 = Tensor([t,t,t]);
let tensor2 = Tensor([andGate, Identity(1)]);

Js.log(makeCircuit(composemany([t,t,t,t]), "test"));

let circuit = composemany([tensor1,tensor2,orGate])

Js.log(printCircuit(Circuit([],[],circuit, "My first circuit")));

let f = Function("f", 1, 1, id);
let g = Function("g", 1, 1, id);
let h = Function("h", 2, 1, id);
let delay = Delay;

let tensor3 = Tensor([delay,g]);
let tensor4 = Tensor([f,Identity(1)]);

let circuit2 = composemany([fork, tensor3, tensor4, h]);

let circ2 = Circuit([],[],circuit2,"Second wind");

Js.log(printCircuit(Circuit([],[],circuit2,"Second wind")));

let f' = Function("f", 3, 3, id);
let circuit3 = trace(2, f');

Js.log(printCircuit(Circuit([],[],circuit3,"Trace")));

let circuit4 = traceAsIteration(circuit3);
Js.log(printCircuit(makeCircuit(circuit4, "Iteration")));

let swap1 = swap(2,1);
Js.log(string_of_int(inputs(swap1)));
Js.log(string_of_int(outputs(swap1)));

Js.log(string_of_int(inputs(Tensor([join, Identity(1)]))));

let circuit5 = composemany([swap(2,1), Tensor([join, Identity(1)])]);
let circ5 = makeCircuit(circuit5, "Swap");
Js.log(printCircuit(circ5));

let circuit6 = compose(t, fork);
let circ6 = makeCircuit(circuit6, "Fork II");
Js.log(printCircuit(circ6));
Js.log(printCircuit(makeCircuit(evaluateOneStep(circuit6), "Evaluated Fork II")));