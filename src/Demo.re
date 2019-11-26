type lattice = Bottom | False | True | Top;

let printLattice = (lattice) => {
    switch (lattice) {
    | Bottom => "⊥"
    | False  => "f"
    | True   => "t"
    | Top    => "T"
    ;}
}

type component = Value(lattice)
               | Identity(int)
               | Composition(component, component)    
               | Tensor(list(component))
               | Gate(string, int, int)
               | Input(int)
               | Output(int)
               | Link(int, int, component)
and circuit = Circuit(list(int), list(int), component, string);

let rec inputs = (item) => {
    switch (item) {
    | Value(x)               => 0
    | Identity(x)            => x
    | Composition(f, g)      => inputs(f)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + inputs(comp)), 0, fs)                        
    | Gate(id, x, y)         => x
    | Input(int)             => 0   
    | Output(int)            => 1
    | Link(inlink, outlink, circuit) => inputs(circuit)
    ;}
}

let rec outputs = (item) => {
    switch (item) {
    | Value(x)               => 1
    | Identity(x)            => x
    | Composition(f, g)      => outputs(g)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + outputs(comp)), 0, fs)    
    | Gate(id, x, y)         => y
    | Input(int)             => 1   
    | Output(int)            => 0
    | Link(inlink, outlink, circuit) => outputs(circuit)
    ;}
}

let compose = (f, g) => {
    assert(outputs(f) == inputs(g));
    Composition(f, g);
}

let rec composemany = (list) => {
    switch(list){
    | [f, ...tl] => List.fold_left(((composition, component) => compose(composition, component)), f, tl)
    ;}
}

let rec printCircuit' = (component) => {
    switch (component) {
    | Value(x)               => printLattice(x) 
    | Identity(x)            => string_of_int(x)
    | Composition(f, g)      => printCircuit'(f) ++ {js| ⋅ |js} ++ printCircuit'(g)
    | Tensor([f, ...tl])     => "[" ++ List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printCircuit'(comp)), printCircuit'(f), tl) ++ "]"
    | Gate(id, x, y)         => id
    | Input(int)             => ":" ++ string_of_int(int)   
    | Output(int)            => string_of_int(int) ++ ":"
    | Link(inlink, outlink, circuit) => "|" ++ string_of_int(inlink) ++ "-" ++ string_of_int(outlink) ++ "|" ++ printCircuit'(circuit) 
    ;}
}

let printCircuit = (circuit) => 
    switch(circuit){
    | Circuit (_,_,comp,_) => Js.log(printCircuit'(comp));}

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
printCircuit(Circuit([],[],circuit, ""));