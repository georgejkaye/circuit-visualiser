/**
 * File containing functions for rewriting and
 * evaluating circuits
 */

open Circuits;
open Helpers;

let rec normalForm = (c) => {
    switch(c){
    | Value(_)          => true
    | Identity(_)       => true
    | Composition(_, _) => false
    | Tensor(xs)        => List.fold_left((&&), true, List.map(normalForm, xs))
    | Function(_,_,_,_,_) => true
    | Delay(_)          => true
    | Trace(_,f)        => normalForm(f)
    | Input(_)          => true
    | Output(_)         => true
    | Iter(_,f)         => normalForm(f)
    | Link(_,_,f)       => normalForm(f)
    | Macro(_,_,_)        => false
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
let rec applyTensor = (v, l, arg, ys) => {
    switch(arg) {
    | Tensor(xs) => Tensor(applyTensor'(v, l, xs, 0, ys, 0,[],[]))
    | f          => Composition(f, Tensor(ys))
    }
} and applyTensor' = (v, l, xs, nx, ys, ny, xs', ys') => {
    switch(xs, ys){
    | ([], []) => []
    | (_, []) => failwith("bad arguments a" ++ printList(xs, (x) => printComponent(v,x,l)))
    | ([], _) => failwith("bad arguments b" ++ printList(ys, (x) => printComponent(v,x,l)))
    | ([x, ...xs], [y, ...ys]) => if(outputs'(x) - nx == inputs'(y) - ny){
                                        let lhs =   switch(xs',ys'){
                                                    | ([], []) => Composition(x, y)
                                                    | (_, [])  => Composition(Tensor(List.concat([xs', [x]])), y)
                                                    | ([], _)  => Composition(x, Tensor(List.concat([ys', [y]])))
                                                    | (_, _)   => Composition(Tensor(List.concat([xs', [x]])), Tensor(List.concat([ys', [y]])))
                                                    };
                                                    [lhs, ...applyTensor'(v,l,xs,0,ys,0,[],[])]
                                    } else {
                                        if(outputs'(x) - nx < inputs'(y) - ny) {
                                            applyTensor'(v, l, xs, 0, [y,...ys], ny + outputs'(x), List.concat([xs', [x]]), ys')
                                        } else {
                                            applyTensor'(v, l, [x,...xs], nx + inputs'(x), ys, 0, xs', List.concat([ys', [y]]))
                                        }
                                    }
    }
}

/* Check if all the elements of a tensor are values */
let rec tensorAllValues = (xs) => {
    switch(xs){
    | []                => false
    | [Value(_)]        => true
    | [_]               => false
    | [Value(_),...xs]  => tensorAllValues(xs)
    | [_,..._]          => false
    }
}

/* Trace rewrites */
let traceAsIteration = (v,l,trace) => {
    switch (trace) {
    | Trace(x, f)    =>  composemany([
                                iter(
                                    composemany([
                                        tensor([
                                            identity(v,x),
                                            exp(stub(v),outputs'(trace)),
                                            identity(v, inputs'(trace))
                                        ]),
                                        {v,c:f,l}
                                    ])
                                ),
                                tensor([
                                    exp(stub(v), x),
                                    identity(v, outputs'(trace))
                                ]),
                            ]).c
    | _ => failwith("This is not a trace")
    }
}

let unfoldIteration = (v, trace) => {
    switch(trace.c) {
    | Iter(_, f) => {
        composemany([
            dfork(v, inputs(trace)),
            tensor([
                trace,
                identity(v,inputs(trace))
            ]),
        {v,c:f,l:[]}
        ])
    }
    }
}

/* Perform one evaluation step on a circuit */
let rec evaluateOneStep = ({v, c, l}) => {
    Js.log("evaluateOneStep " ++ printComponent(v,c,l));
    let eval = {
        if(normalForm(c)){
            c;
        } else {
            switch(c){
            | Macro(_,_,f)     => f
            | Trace(_,_)       => traceAsIteration(v,l,c)
            | Iter(_,_)        => unfoldIteration(v,{v,l,c}).c
            | Tensor(xs)       => Tensor(evaluateOneStepTensor(v,xs,l))
            | Composition(x,y) => evaluateOneStepComposition(v,x,y,l)
            | _                => failwith("todo evaluateOneStep " ++ printCircuit({v,c,l}))
            }
        }
    };
    {v, c:simplifyTensor(v,eval), l}
} and evaluateOneStepTensor = (v,xs,l) => {
    Js.log("evaluateOneStepTensor " ++ printComponentList(v,l,xs));
    switch(xs){
    | []         => []
    | [x, ...xs] => if (normalForm(x)){
                        [x, ...evaluateOneStepTensor(v,xs,l)]
                    } else {
                        List.concat([[evaluateOneStep({v, c:x, l}).c], xs])
                    }
    };
} and evaluateOneStepComposition = (v, x, y, l) => {
    Js.log("evaluateOneStepComposition " ++ printComponent(v,x,l) ++ " --- " ++ printComponent(v,y,l));
    if(normalForm(x)){
        switch(y){
        | Identity(_)        =>  x
        | Function(id,latex,_,_,f) => switch(x){
                                | Value(_)   => f(v,x,l)
                                | Tensor(xs) => switch(f(v,x,l)){
                                                | item => item
                                                | exception _ => Function(id ++ "(" ++ printComponentListCommas(v,l,xs) ++ ")", 
                                                                            latex ++ "(" ++ printComponentListLatexCommas(v,l,xs),
                                                                            inputs'(x), outputs'(y), (v,c,l) => composemany([{v,c,l},{v,c:x,l},{v,c:y,l}]).c) 
                                }
                                | Function(_,_,_,_,_) => switch(f(v,x,l)){
                                                        | item        => item
                                                        | exception _ => Function(id ++ "(" ++ printComponent(v,x,l) ++ ")", 
                                                                                      latex ++ "(" ++ printComponentLatex(v,x,l) ++ ")",
                                                                                      inputs'(x), outputs'(y), (v,c,l) => composemany([{v,c,l},{v,c:x,l},{v,c:y,l}]).c) 
                                                        }
                                }
        | Tensor(ys)        =>  applyTensor(v,l,x,ys)
        | Composition(a,b)  =>  Composition(evaluateOneStepComposition(v,x,a,l),b)
        | f                 =>  Composition(x, evaluateOneStep({v, c:f, l}).c)
        }        
    } else {
        Composition(evaluateOneStep({v,c:x,l}).c, y)
    }
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

