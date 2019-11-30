type lattice = Bottom | False | True | Top | Placeholder;

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
               | Function(string, int, int, (component) => component)
               | Delay
               | Trace(int, component)
               | Iter(int, component)
               | Input(int)
               | Output(int)
               | Link(int, int, component)
and circuit = Circuit(list(int), list(int), component, string);

let lub = (a,b) => {
    switch(a, b){
    | (Value(x), Value(y)) =>
        switch(x, y){
        | (Top, _) => Value(Top)
        | (_, Top) => Value(Top)
        | (a, Bottom) => Value(a)
        | (Bottom, b) => Value(b)
        | (True, True) => Value(True)
        | (True, False) => Value(True)
        | (False, True) => Value(False)
        | (False, False) => Value(False)
        }
    | _ => failwith("Not implemented")
    }
}

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

let rec inputs = (item) => {
    switch (item) {
    | Value(x)               => 0
    | Identity(x)            => x
    | Composition(f, g)      => inputs(f)
    | Tensor(fs)             => List.fold_left(((no, comp) => no + inputs(comp)), 0, fs)                        
    | Function(id, x, y, func)         => x
    | Delay                  => 1
    | Trace(x, comp)         => inputs(comp) - x
    | Iter(x, comp)          => inputs(comp) - x
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
    | Function(id, x, y, func)         => y
    | Delay                  => 1
    | Trace(x, comp)         => outputs(comp) - x
    | Iter(x, comp)          => outputs(comp)
    | Input(int)             => 1   
    | Output(int)            => 0
    | Link(inlink, outlink, circuit) => outputs(circuit)
    ;}
}

let makeCircuit = (component, string) => {
    Circuit([],[],component,string)
}

let compose = (f, g) => {
    assert(outputs(f) == inputs(g));
    Composition(f, g);
}

let rec last = (list) => {
    switch(list){
    | [] => []
    | [x] => x
    | [x, ...xs] => last(xs)
    }
}

/*let rec composemany = (xs) => {
    let ys = List.rev(xs);
    List.fold_right(((component, composition) => compose(component, composition)), xs, )
}*/

let rec composemany = (xs) => {
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => Composition(x, composemany(xs))
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
    assert(inputs(f) >= x && outputs(f) >= x);
    Trace(x, f);
}

let rec printCircuit' = (component) => {
    switch (component) {
    | Value(x)                          => printLattice(x) 
    | Identity(x)                       => string_of_int(x)
    | Composition(f, g)                 => printCircuit'(f) ++ {js| ⋅ |js} ++ printCircuit'(g)
    | Tensor([f, ...tl])                => "[" ++ List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printCircuit'(comp)), printCircuit'(f), tl) ++ "]"
    | Function(id, x, y, func)          => id
    | Delay                             => {js|ẟ|js}
    | Trace(x, component)               => "Tr[" ++ string_of_int(x) ++ "](" ++ printCircuit'(component) ++ ")" 
    | Iter(x, component)                => "iter[" ++ string_of_int(x) ++ "](" ++ printCircuit'(component) ++ ")" 
    | Input(int)                        => ":" ++ string_of_int(int)   
    | Output(int)                       => string_of_int(int) ++ ":"
    | Link(inlink, outlink, circuit)    => "|" ++ string_of_int(inlink) ++ "-" ++ string_of_int(outlink) ++ "|" ++ printCircuit'(circuit) 
    ;}
}

let printCircuit = (circuit) => 
    switch(circuit){
    | Circuit (_,_,comp,name) => name ++ " : " ++ string_of_int(inputs(comp)) ++ {js| → |js} ++ string_of_int(outputs(comp)) ++ "\n" ++ printCircuit'(comp);}

let rec valueList = (v, x) => {
    switch(x) {
    | 0 => []
    | n => [Placeholder, ...valueList(v, x-1)]
    }
}

let rec split' = (n, xs, ys) => {
    switch(ys){
    | []         => (xs, [])
    | [y, ...yss] => n == 0 ? (xs, [y, ...yss]) : split'(n-1, List.concat([xs, [y]]), yss)
    }
}

let split = (n, xs) => split'(n, [], xs);
                            

let id = (x) => x;


/* Special morphisms */
/* TODO add functions of special morphisms */
let fork = Function({js|⋏|js}, 1, 2, (comp) => Tensor([comp, comp]));
let join = Function({js|⋎|js}, 2, 1, (comp) => switch(comp){
                                                | Tensor([a, b]) => lub(a,b)
                                                });
let stub = Function({js|~|js}, 1, 0, id);
let swap = (x, y) => Function({js|×|js} ++ "[" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "]", 
                                x + y, 
                                x + y, 
                                (comp) => switch(comp){
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
    | Composition(fst, rest) => Js.log("first = " ++ printCircuit'(fst) ++ " snd = " ++ printCircuit'(compn(0, rest)));
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
}*/