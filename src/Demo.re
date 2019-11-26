open Circuits;

let andgate = Gate("AND", 2, 1);
let orgate = Gate("OR", 2, 1);
let t = Value(True);
let f = Value(False);

let tensor1 = Tensor([t,t,t]);
let tensor2 = Tensor([andgate, Identity(1)]);

Js.log(outputs(tensor1));
Js.log(inputs(tensor2));

let circuit = composemany([tensor1,tensor2,orgate])

Js.log("Hello!");
printCircuit(Circuit([],[],circuit, "My first circuit"));

let f = Gate("f", 1, 1);
let g = Gate("g", 1, 1);
let h = Gate("h", 2, 1);
let delay = Delay;

let tensor3 = Tensor([delay,g]);
let tensor4 = Tensor([f,Identity(1)]);

let circuit2 = composemany([fork, tensor3, tensor4, h]);

printCircuit(Circuit([],[],circuit2,"Second wind"));

let f' = Gate("f", 3, 3);
let circuit3 = trace(2, f');

printCircuit(Circuit([],[],circuit3,"Trace"));