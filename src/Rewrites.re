open Circuits;
open Helpers;

let rec normalForm = (c) => {
    switch(c){
    | Value(_)          => true
    | Identity(_)       => true
    | Composition(_, _) => false
    | Tensor(xs)        => List.fold_left((&&), true, List.map(normalForm, xs))
    | Function(_,_,_,_) => true
    | Delay(_)          => true
    | Trace(_,f)        => normalForm(f)
    | Input(_)          => true
    | Output(_)         => true
    | Iter(_,f)         => normalForm(f)
    | Link(_,_,f)       => normalForm(f)
    | Macro(_,_)        => false
    }
}

/* Flatten a tensor of tensors into one big tensor */
let rec simplifyTensor = (v,c) => {
    switch(c){
    | Tensor(xs) => Tensor(simplifyTensor'(v,xs))
    | Composition(Tensor(xs), y) => Composition(Tensor(simplifyTensor'(v,xs)), y)
    | _                          => c
    };
} and simplifyTensor' = (v,xs) => {
    switch(xs){
    | [] => []
    | [Tensor(ys), ...xs] => simplifyTensor'(v,List.concat([ys, xs]))
    | [x, ...xs] => [x, ...simplifyTensor'(v,xs)]
    }
}

/* Apply a tensor to some arguments 
 *   v: lattice
 *   lhs: the argument to the apply to the tensor
 *   rhs: the elements of the tensor
 */
let rec applyTensor = (v, arg, ys) => {
    switch(arg) {
    | Tensor(xs) => Tensor(applyTensor'(v, xs, 0, ys, 0,[],[]))
    | f          => Composition(f, Tensor(ys))
    }
} and applyTensor' = (v, xs, nx, ys, ny, xs', ys') => {
    switch(xs, ys){
    | ([], []) => []
    | (_, []) => failwith("bad arguments a" ++ printList(xs, printComponent(v)))
    | ([], _) => failwith("bad arguments b" ++ printList(ys, printComponent(v)))
    | ([x, ...xs], [y, ...ys]) => if(outputs'(x) - nx == inputs'(y) - ny){
                                        let lhs =   switch(xs',ys'){
                                                    | ([], []) => Composition(x, y)
                                                    | (_, [])  => Composition(Tensor(List.concat([xs', [x]])), y)
                                                    | ([], _)  => Composition(x, Tensor(List.concat([ys', [y]])))
                                                    | (_, _)   => Composition(Tensor(List.concat([xs', [x]])), Tensor(List.concat([ys', [y]])))
                                                    };
                                                    [lhs, ...applyTensor'(v,xs,0,ys,0,[],[])]
                                    } else {
                                        if(outputs'(x) - nx < inputs'(y) - ny) {
                                            applyTensor'(v, xs, 0, [y,...ys], ny + outputs'(x), List.concat([xs', [x]]), ys')
                                        } else {
                                            applyTensor'(v, [x,...xs], nx + inputs'(x), ys, 0, xs', List.concat([ys', [y]]))
                                        }
                                    }
    }
}

let rec tensorAllValues = (xs) => {
    switch(xs){
    | []                => false
    | [Value(_)]        => true
    | [_]               => false
    | [Value(_),...xs]  => tensorAllValues(xs)
    | [_,..._]          => false
    }
}

let rec evaluateOneStepTensor = (v,xs) => {
    Js.log("evaluateOneStepTensor " ++ printList(xs, printComponent(v)));
    switch(xs){
    | []         => []
    | [x, ...xs] => if (normalForm(x)){
                        [x, ...evaluateOneStepTensor(v,xs)]
                    } else {
                        List.concat([[evaluateOneStep({v:v, c:x}).c], xs])
                    }
    };
} and evaluateOneStepComposition = (v, x, y) => {
    Js.log("evaluateOneStepComposition " ++ printComponent(v,x) ++ " --- " ++ printComponent(v,y));
    if(normalForm(x)){
        switch(y){
        | Identity(_)        =>  x
        | Function(id,_,_,f) => switch(x){
                                | Value(a)   => f(v,x)
                                | Tensor(xs) => if(tensorAllValues(xs)) {
                                                    f(v,x)
                                                } else {
                                                    Function(id ++ "(" ++ printComponentListCommas(v,xs) ++ ")", inputs'(x), outputs'(y), (v,c) => composemany([{v,c},{v:v,c:x},{v,c:y}]).c) 
                                                }
                                }
        | Tensor(ys)        =>  applyTensor(v,x,ys)
        | Composition(a,b)  =>  Composition(evaluateOneStepComposition(v,x,a),b)
        | f                 =>  Composition(x, evaluateOneStep({v:v, c:f}).c)
        }        
    } else {
        Composition(evaluateOneStep({v:v,c:x}).c, y)
    }
} and evaluateOneStep = ({v, c}) => {
    Js.log("evaluateOneStep " ++ printComponent(v,c));
    let eval = {
        if(normalForm(c)){
            c;
        } else {
            switch(c){
            | Macro(_,f)       => f
            | Trace(x,f)       => Trace(x,evaluateOneStep({v:v,c:f}).c)
            | Iter(x,f)        => Iter(x,evaluateOneStep({v:v,c:f}).c)
            | Tensor(xs)       => Tensor(evaluateOneStepTensor(v,xs))
            | Composition(x,y) => evaluateOneStepComposition(v,x,y)
            | _                => failwith("todo evaluateOneStep " ++ printCircuit({v:v,c:c}))
            }
        }
    };
    {v:v, c:simplifyTensor(v,eval)}
}


/* Evaluate until a normal form is reached */
let rec evaluate = (orig) => {
    let eval = evaluateOneStep(orig);
    if(eval == orig){
        eval    
    } else {
        evaluate(eval)
    }
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
                                        {v:trace.v,c:f}
                                    ])
                                ),
                                tensor([
                                    exp(stub(trace.v), x),
                                    identity(trace.v, outputs(trace))
                                ]),
                            ])
    | _ => failwith("This is not a trace")
    }
}