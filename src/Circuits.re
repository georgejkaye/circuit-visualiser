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
and circuit('element) = { 
    v: lattice('element),
    c: component('element),
}

let rec inputs = (c) => {
    switch (c) {
    | Value(_)               => 0
    | Identity(x)            => x
    | Composition(f, _)      => inputs(f)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + inputs(comp)), 0, fs)                        
    | Function(_, x, _, _)   => x
    | Delay(_)               => 1
    | Trace(x, comp)         => inputs(comp) - x
    | Iter(x, comp)          => inputs(comp) - x
    | Input(_)               => 0   
    | Output(_)              => 1
    | Link(_, _, circuit)    => inputs(circuit)
    ;}
}

let rec outputs = (c) => {
    switch (c) {
    | Value(_)               => 1
    | Identity(x)            => x
    | Composition(_, g)      => outputs(g)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + outputs(comp)), 0, fs)    
    | Function(_, _, y, _)   => y
    | Delay(_)               => 1
    | Trace(x, comp)         => outputs(comp) - x
    | Iter(_, comp)          => outputs(comp)
    | Input(_)               => 1   
    | Output(_)              => 0
    | Link(_, _, circuit)    => outputs(circuit)
    ;}
}

let rec printComponent = (v, c) => {
    switch (c) {
    | Value(x)                          => v.print(x)
    | Identity(x)                       => string_of_int(x)
    | Composition(f, g)                 => printComponent(v,f) ++ {js| ⋅ |js} ++ printComponent(v,g)
    | Tensor([])                        => "[]"
    | Tensor([f, ...tl])                => "[" ++ List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printComponent(v,comp)), printComponent(v,f), tl) ++ "]"
    | Function(id, _, _, _)          => id
    | Delay(x)                          => {js|ẟ[|js} ++ string_of_int(x) ++ "]"
    | Trace(x, component)               => "Tr[" ++ string_of_int(x) ++ "](" ++ printComponent(v,component) ++ ")" 
    | Iter(x, component)                => "iter[" ++ string_of_int(x) ++ "](" ++ printComponent(v,component) ++ ")" 
    | Input(int)                        => ":" ++ string_of_int(int)   
    | Output(int)                       => string_of_int(int) ++ ":"
    | Link(inlink, outlink, circuit)    => "|" ++ string_of_int(inlink) ++ "-" ++ string_of_int(outlink) ++ "|" ++ printComponent(v,circuit) 
    ;}
}

let printCircuit = ({v,c}) => printComponent(v,c)

let compose = ((v,c), (v',c')) => {
    assert'(outputs(c) == inputs(c'), "Outputs of circuit " ++ printComponent(v, c) ++ " do not match inputs of circuit " ++ printComponent(v',c'));
    {v:v, c:Composition(c, c')};
}

let rec composemany = (v, xs) => {
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => {v:v, c:Composition(x.c, composemany(v, xs).c)}
    }
}

let tensor = (v, xs) => {
    {v:v, c:Tensor(xs)}
}

/* Special morphisms */
let fork = (v) => {v:v, c:Function({js|⋏|js}, 1, 2, (_, c) => Tensor([c, c]))};

let swap = (v, x, y) => {v:v, c:Function({js|×|js} ++ "[" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "]", 
                                x + y, 
                                x + y, 
                                (_, comp) => switch(comp){
                                | Tensor(xs) => let (top, bot) = split(x, xs); Tensor(List.concat([bot, top]));
                                | _ => failwith("Swap can only swap a tensor")
                                }
                            )
                        }

let join = Function({js|⋎|js}, 2, 1, (v, c) => switch(c){
                                | Tensor([Value(x), Value(y)]) => Value(v.joinOp(x,y))
                                | Tensor([x, y]) => failwith("Not implemented")
                                | _ => failwith("Join can only take two arguments")
                                }
                    );

let stub = Function({js|~|js}, 1, 0, (_, _) => Identity(0));


let delay = (n) => Delay(n);

let rec dfork = (v,n) => {
    switch(n) {
        | 0 => {v:v, c:Identity(0)}
        | 1 => fork(v)
        | n => composemany(v, [
                    tensor(v,[dfork(v,n-1).c, fork(v).c]),
                    tensor(v,[Identity(1), swap(v, n-1, 1).c, Identity(1)])
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