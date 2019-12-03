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
    ;}
}

let outputs = ({c}) => outputs'(c)

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

let identity = (v, n) => {v:v, c:Identity(n)}

/* Create a composition circuit */
let compose = (c, c') => {
    assert'(outputs(c) == inputs(c'), "Outputs of circuit " ++ printCircuit(c) ++ " do not match inputs of circuit " ++ printCircuit(c'));
    assert'(c.v == c'.v, "Circuits use different lattices!");
    {v:c.v, c:Composition(c.c, c'.c)};
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

let func = (v, id, ins, outs, f) =>
    {v:v, c: Function(id,ins,outs,f)}

/* Special morphisms */

/* Fork a wire into two wires */
let fork = (v) => {v:v, c:Function({js|⋏|js}, 1, 2, (_, c) => Tensor([c, c]))};

/* Join two wires into one, taking the join of their values */
let join = (v) => {v:v, c:Function({js|⋎|js}, 
                            2, 
                            1, 
                            (v, c) => switch(c){
                                        | Tensor([Value(x), Value(y)]) => Value(v.joinOp(x,y))
                                        | Tensor([x, y]) => failwith("Not implemented")
                                        | _ => failwith("Join can only take two arguments")
                                    }
                        )  
                  }

/* Swap buses of width x and y */
let swap = (v, x, y) => {v:v, c:Function({js|×|js} ++ "[" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "]", 
                                x + y, 
                                x + y, 
                                (_, comp) => switch(comp){
                                | Tensor(xs) => let (top, bot) = split(x, xs); Tensor(List.concat([bot, top]));
                                | _ => failwith("Swap can only swap a tensor")
                                }
                            )
                        }

/* Stub a wire, leading to the unique identity on 0 */
let stub = (v) => {v:v, c:Function({js|~|js}, 1, 0, (_, _) => Identity(0))};

/* Fork all wires in a bus */
let rec dfork = (v,n) => {
    switch(n) {
        | 0 => {v:v, c:Identity(0)}
        | 1 => fork(v)
        | n => composemany([
                    tensor([dfork(v,n-1), fork(v)]),
                    tensor([identity(v,1), swap(v, n-1, 1), identity(v,1)])
                ])
        }
}

/* Join all wires in a bus */
let rec djoin = (v,n) => {
    switch(n) {
        | 0 => {v:v, c:Identity(0)}
        | 1 => join(v)
        | n => composemany([
                    tensor([identity(v,1), swap(v, 1, n-1), identity(v,1)]),
                    tensor([djoin(v,n-1), join(v)])
                ])
        }
}

/* Create a delay */
let delay = (n) => Delay(n);

/* Create a trace */
let trace = (x, f) => {
    assert'(inputs(f) >= x && outputs(f) >= x, "Inputs and outputs of circuit " ++ printCircuit(f) ++ " are less than the size of the trace.");
    {v:f.v, c:Trace(x, f.c)}
}

let iter = (f) => {
    assert'(inputs(f) >= outputs(f), "Not enough inputs of circuit " ++ printCircuit(f) ++ " to iterate.");
    {v:f.v, c:Iter(outputs(f), f.c)}
}

/* Trace rewrites */
let traceAsIteration = (trace) => {
    switch (trace.c) {
    | Trace(x, f)    =>  composemany([
                                iter(
                                    composemany([
                                        tensor([
                                            identity(trace.v,x),
                                            exp(stub(trace.v),outputs(trace)),
                                            identity(trace.v, inputs'(f))
                                        ]),
                                        {v:trace.v,c:trace.c}
                                    ])
                                ),
                                tensor([
                                    exp(stub(trace.v),
                                    outputs(trace))
                                ]),
                            ])
    | _ => failwith("This is not a trace")
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

                            

let id = (x) => x;


/** Trace rewrites */

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