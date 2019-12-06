open Helpers;
open Lattices;

/* A circuit is a component associated with a lattice v */
type component('element) = 
    | Value('element)
    | Identity(int)
    | Composition(component('element), component('element))    
    | Tensor(list(component('element)))
    | Function(string, int, int, (lattice('element), component('element)) => component('element))
    | Delay(int)
    | Trace(int, component('element))
    | Iter(int, component('element))
    | Input(int)
    | Output(int)
    | Link(int, int, component('element))
    | Macro(string, component('element))
and circuit('element) = { 
    v: lattice('element),
    c: component('element),
}

let rec inputs' = (c) => {
    switch (c) {
    | Value(_)               => 0
    | Identity(x)            => x
    | Composition(f, _)      => inputs'(f)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + inputs'(comp)), 0, fs)                        
    | Function(_, x, _, _)   => x
    | Delay(_)               => 1
    | Trace(x, comp)         => inputs'(comp) - x
    | Iter(x, comp)          => inputs'(comp) - x
    | Input(_)               => 0   
    | Output(_)              => 1
    | Link(_, _, circuit)    => inputs'(circuit)
    | Macro(_,f)             => inputs'(f)
    ;}
}

let inputs = ({c}) => inputs'(c)


let rec outputs' = (c) => {
    switch (c) {
    | Value(_)               => 1
    | Identity(x)            => x
    | Composition(_, g)      => outputs'(g)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + outputs'(comp)), 0, fs)    
    | Function(_, _, y, _)   => y
    | Delay(_)               => 1
    | Trace(x, comp)         => outputs'(comp) - x
    | Iter(_, comp)          => outputs'(comp)
    | Input(_)               => 1   
    | Output(_)              => 0
    | Link(_, _, circuit)    => outputs'(circuit)
    | Macro (_,f)            => outputs'(f)
    ;}
}

let outputs = ({c}) => outputs'(c)

let rec printComponent = (v, c) => {
    switch (c) {
    | Value(x)                          => v.print(x)
    | Identity(x)                       => string_of_int(x)
    | Composition(f, g)                 => "(" ++ printComponent(v,f) ++ {js| ⋅ |js} ++ printComponent(v,g) ++ ")"
    | Tensor([])                        => "[]"
    | Tensor([x])                       => printComponent(v,x)
    | Tensor([f, ...tl])                => List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printComponent(v,comp)), printComponent(v,f), tl)
    | Function(id, _, _, _)             => id
    | Delay(x)                          => {js|ẟ{|js} ++ string_of_int(x) ++ "}"
    | Trace(x, component)               => "Tr{" ++ string_of_int(x) ++ "}(" ++ printComponent(v,component) ++ ")" 
    | Iter(x, component)                => "iter{" ++ string_of_int(x) ++ "}(" ++ printComponent(v,component) ++ ")" 
    | Input(int)                        => ":" ++ string_of_int(int)   
    | Output(int)                       => string_of_int(int) ++ ":"
    | Link(inlink, outlink, circuit)    => "|" ++ string_of_int(inlink) ++ "-" ++ string_of_int(outlink) ++ "|" ++ printComponent(v,circuit) 
    | Macro(id, _)                      => id
    ;}
}

let printCircuit = ({v,c}) => printComponent(v,c)

let printComponentList = (v, xs) => printList(xs, (x) => printComponent(v,x));
let printComponentListCommas = (v, xs) => printListCommas(xs, (x) => printComponent(v,x));

let value = (v,v') => {v:v, c:Value(v')}

let identity = (v, n) => {v:v, c:Identity(n)}

/* Compose two components together */
let compose' = (v, c, c') => {
    assert'(outputs'(c) == inputs'(c'), "Outputs of circuit " ++ printComponent(v,c) ++ " do not match inputs of circuit " ++ printComponent(v,c'));
    Composition(c, c')
}

/* Create a composition circuit */
let compose = (c, c') => {
    assert'(c.v == c'.v, "Circuits use different lattices!");
    {v:c.v, c:compose'(c.v, c.c,c'.c)};
}


/* Compose many circuits at once */
let rec composemany = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), "Not all circuits use the same lattice!");
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => {v:List.hd(xs).v, c:Composition(x.c, composemany(xs).c)}
    }
}

/* Create a tensor circuit */
let tensor = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), "Not all circuits use the same lattice!");
    let ys = List.map((x => x.c), xs);
    {v:List.hd(xs).v, c:Tensor(ys)}
}

/* Helper function for exp */
let rec exp' = (f, x) => {
    if(x == 0){
        [];
    } else {
        [f, ...exp'(f, x-1)];
    }
}

/* Create an exponential tensor (i.e. multiple copies of one circuit tensor'd together) */
let exp = (f, x) => {
    tensor(exp'(f, x));
}

/* Create a function. */
let func = (v, id, ins, outs, f) =>
    {v:v, c: Function(id,ins,outs,f)}

/* Create a function that acts as a 'black box' - it transforms the inputs into the outputs but we don't know how exactly. */
let funcBlackBox = (v, id, ins, outs) => {
    let rec bb = Function(id,ins,outs, (v,c) => {
                    let id' = switch(c){
                    | Tensor(xs) => printComponentList(v,xs)
                    | _          => printComponent(v,c)
                    };
                    Function(id ++ "(" ++ id' ++ ")", inputs'(c), outs, (v,c) => composemany([{v,c},{v,c:bb}]).c)
                });
    {v:v, c: bb}
}

let macro = (v, id, f) =>
    {v:v, c: Macro(id, f)}

/* Special morphisms */

/* Fork a wire into two wires */
let fork = (v) => func(v,{js|⋏|js}, 1, 2, (_, c) => Tensor([c, c]));

/* Join two wires into one, taking the join of their values */
let rec join = (v) => func(v,{js|⋎|js}, 
                            2, 
                            1, 
                            (v, c) => switch(c){
                                        | Tensor([Value(x), Value(y)]) => Value(v.joinOp(x,y))
                                        | Tensor([x, y]) => Function(printComponent(v,x) ++ {js|⊔|js} ++ printComponent(v,y), inputs'(c), 1, (v,c) => compose({v,c}, join(v)).c)
                                        | _ => failwith("Join can only take two arguments")
                                    }
                        )  

/* Stub a wire, leading to the unique identity on 0 */
let stub = (v) => func(v, {js|~|js}, 1, 0, (_, _) => Identity(0));

let specialMorphisms = (v) => [fork(v).c, join(v).c, stub(v).c];

/* Swap buses of width x and y */
let swap = (v, x, y) => func(v, {js|×|js} ++ "{" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "}", 
                                x + y, 
                                x + y, 
                                (_, comp) => switch(comp){
                                | Tensor(xs) => let (top, bot) = split(x, xs); Tensor(List.concat([bot, top]));
                                | _ => failwith("Swap can only swap a tensor")
                                }
                            )



/* Fork all wires in a bus */
let rec dfork = (v,n) => {
    let comp = switch(n){
        | 0 => Identity(0)
        | 1 => fork(v).c
        | n => composemany([
                        tensor([dfork(v,n-1), fork(v)]),
                        tensor([identity(v,n-1), swap(v, n-1, 1), identity(v,1)])
                        ]).c
        };
    macro(v, {js|Δ{|js} ++ string_of_int(n) ++ "}", comp)
}

/* Join all wires in a bus */
let rec djoin = (v,n) => {
    let comp = switch(n){
        | 0 => Identity(0)
        | 1 => join(v).c
        | n => composemany([
                        tensor([identity(v,n-1), swap(v, 1, n-1), identity(v,1)]),
                        tensor([djoin(v,n-1), join(v)])
                        ]).c
        };
    macro(v, {js|∇{|js} ++ string_of_int(n) ++ "}", comp)
}

let djoinRegEx = [%bs.re "/\\\\\/\{([0-9]+)\}/"];
let swapRegEx = [%bs.re "/x\{([0-9]+),([0-9]+)\}/"];
let dforkRegEx = [%bs.re "/\/\\\{([0-9]+)\}/"];

/* Create a delay */
let delay = (v,n) => {v:v, c:Delay(n)}
let delayRegEx = [%bs.re "/o\{([0-9]+)\}/"];

/* Create a trace */
let trace = (x, f) => {
    assert'(inputs(f) >= x && outputs(f) >= x, "Inputs and outputs of circuit " ++ printCircuit(f) ++ " are less than the size of the trace.");
    {v:f.v, c:Trace(x, f.c)}
}

/* Create an iteration */
let iter = (f) => {
    assert'(inputs(f) >= outputs(f), "Not enough inputs of circuit " ++ printCircuit(f) ++ " to iterate.");
    {v:f.v, c:Iter(outputs(f), f.c)}
}
                  
