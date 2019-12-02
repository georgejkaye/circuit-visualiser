open Helpers;
open Lattices;

type component('element) = 
    | Value('element)
    | Identity(int)
    | Composition(component('element), component('element))    
    | Tensor(list(component('element)))
    | Function(string, int, int, (lattice('element), component('element)) => component('element))
    | Delay
    | Trace(int, component('element))
    | Iter(int, component('element))
    | Input(int)
    | Output(int)
    | Link(int, int, component('element))
and circuit('element) = { 
    v: lattice('element),
    c: component('element),
}

let rec inputs = (c) => {
    switch (c) {
    | Value(x)                       => 0
    | Identity(x)                    => x
    | Composition(f, g)              => inputs(f)
    | Tensor(fs)                     => List.fold_left(((no, comp) => no + inputs(comp)), 0, fs)                        
    | Function(id, x, y, func)       => x
    | Delay                          => 1
    | Trace(x, comp)                 => inputs(comp) - x
    | Iter(x, comp)                  => inputs(comp) - x
    | Input(int)                     => 0   
    | Output(int)                    => 1
    | Link(inlink, outlink, circuit) => inputs(circuit)
    ;}
}

let rec outputs = (c) => {
    switch (c) {
    | Value(x)               => 1
    | Identity(x)            => x
    | Composition(f, g)      => outputs(g)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + outputs(comp)), 0, fs)    
    | Function(id, x, y, func)         => y
    | Delay                  => 1
    | Trace(x, comp)         => outputs(comp) - x
    | Iter(x, comp)          => outputs(comp)
    | Input(int)             => 1   
    | Output(int)            => 0
    | Link(inlink, outlink, circuit) => outputs(circuit)
    ;}
}

let rec printComponent = (v, c) => {
    switch (c) {
    | Value(x)                          => v.print(x)
    | Identity(x)                       => string_of_int(x)
    | Composition(f, g)                 => printComponent(v,f) ++ {js| ⋅ |js} ++ printComponent(v,g)
    | Tensor([f, ...tl])                => "[" ++ List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printComponent(v,comp)), printComponent(v,f), tl) ++ "]"
    | Function(id, x, y, func)          => id
    | Delay                             => {js|ẟ|js}
    | Trace(x, component)               => "Tr[" ++ string_of_int(x) ++ "](" ++ printComponent(v,component) ++ ")" 
    | Iter(x, component)                => "iter[" ++ string_of_int(x) ++ "](" ++ printComponent(v,component) ++ ")" 
    | Input(int)                        => ":" ++ string_of_int(int)   
    | Output(int)                       => string_of_int(int) ++ ":"
    | Link(inlink, outlink, circuit)    => "|" ++ string_of_int(inlink) ++ "-" ++ string_of_int(outlink) ++ "|" ++ printComponent(v,circuit) 
    ;}
}

let printCircuit = ({v,c}) => printComponent(v,c)

let rec composemany = (xs) => {
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => Composition(x, composemany(xs))
    }
}


let rec split' = (n, xs, ys) => {
    switch(ys){
    | []         => (xs, [])
    | [y, ...yss] => n == 0 ? (xs, [y, ...yss]) : split'(n-1, List.concat([xs, [y]]), yss)
    }
}

let split = (n, xs) => split'(n, [], xs);

/* Special morphisms */
let fork = Function({js|⋏|js}, 1, 2, (v, c) => Tensor([c, c]));

let swap = (x, y) => Function({js|×|js} ++ "[" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "]", 
                                x + y, 
                                x + y, 
                                (v, comp) => switch(comp){
                                | Tensor(xs) => let (top, bot) = split(x, xs); Tensor(List.concat([bot, top]));
                                }
                            );

let rec dfork = (n) => {
    switch(n) {
    | 0 => Identity(0)
    | 1 => fork
    | n => composemany([
                Tensor([dfork(n-1), fork]),
                Tensor([Identity(1), swap(n-1, 1), Identity(1)])
            ])
    }
}

/*
let rec compn = (n, component) => {
    switch(component){
    | Composition(x,y) =>   (n == 0) ? x :
                                (switch(component) {
                                    | Composition(x, y)  => compn(n-1, y)
                                    | _                  => failwith("not enough composition")
                                })
    | _ => (n == 0) ? component : failwith("not enough composition")
        
    }
}

let makeCircuit = (component, string) => {
    Circuit([],[],component,string)
}

let compose = (f, g) => {
    assert'(outputs(f) == inputs(g), "Outputs of circuit " ++ printComponent(f) ++ " do not match inputs of circuit " ++ printComponent(f));
    Composition(f, g);
}

let rec last = (list) => {
    switch(list){
    | [] => []
    | [x] => x
    | [x, ...xs] => last(xs)
    }
}

let rec exp' = (f, x) => {
    if(x == 0){
        [];
    } else {
        [f, ...exp'(f, x-1)];
    }
}

let exp = (f, x) => {
    Tensor(exp'(f, x));
}

let trace = (x, f) => {
    assert'(inputs(f) >= x && outputs(f) >= x, "Inputs and outputs of circuit " ++ printComponent(f) ++ " are less than the size of the trace.");
    Trace(x, f);
}

                            

let id = (x) => x;


/* Special morphisms */
/* TODO add functions of special morphisms */
let join = Function({js|⋎|js}, 2, 1, (comp) => switch(comp){
                                                | Tensor([Value(x), Value(y)]) => Value(x#lub(x,y))
                                                | Tensor([x, y]) => failwith("Not implemented")
                                                | _ => failwith("Join can only take two arguments")
                                                });
let stub = Function({js|~|js}, 1, 0, id);




let delay = Delay;

/** Trace rewrites */

let traceAsIteration = (trace) => {
    switch (trace) {
    | Trace(x, f)    =>  composemany([
                                Iter(
                                    x + outputs(trace),
                                    composemany([
                                        Tensor([
                                            Identity(x), 
                                            exp(stub, outputs(trace)), 
                                            Identity(inputs(trace))
                                        ]), 
                                        f
                                    ]),
                                ), 
                                Tensor([
                                    exp(stub, x), 
                                    Identity(outputs(trace))
                                ])
                            ])
    ;}
}

let rec evaluateOneStep = (comp) => {
    switch(comp){
    | Composition(fst, rest) => Js.log("first = " ++ printComponent(fst) ++ " snd = " ++ printComponent(compn(0, rest)));
                                 switch(compn(0,rest)){
                                 | Function(_,_,_,func) => func(fst)
                                 | _ => failwith("not implemented")
                                 }
    }
}

/*let unfoldIteration = (trace) => {
    switch (trace) {
    | Iter(x, f)    =>  c
    }
}*/*/