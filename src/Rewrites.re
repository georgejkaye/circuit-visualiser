open Circuits;
open Helpers;

let rec normalForm = (c) => {
    switch(c){
    | Value(_)          => true
    | Identity(_)       => true
    | Composition(x, y) => false
    | Tensor(xs)        => List.fold_left((&&), true, List.map(normalForm, xs))
    | Function(_,_,_,_) => true
    | Delay(_)          => true
    | Trace(_,f)        => normalForm(f)
    | Input(_)          => true
    | Output(_)         => true
    | Iter(_,f)         => normalForm(f)
    | Link(_,_,f)       => normalForm(f)
    | Macro(_,f)        => false
    }
}

/* Evaluation strategy:
 *      1) 
 *         
 * 
 * 
 */

let rec applyTensor = (v,lhs, rhs) => {
    switch(lhs) {
    | Tensor(xs) => Tensor(applyTensor'(v,xs, 0, rhs, 0,[],[]))
    | f          => Composition(f, Tensor(rhs))
    }
} and applyTensor' = (v, xs, nx, ys, ny, xs', ys') => {
    Js.log("doing " ++ printList(xs,printComponent(v)) ++ "@ " ++ printList(ys,printComponent(v)));
    switch(xs, ys){
    | ([], []) => []
    | (_, []) => failwith("bad arguments a" ++ printList(xs, printComponent(v)))
    | ([], _) => failwith("bad arguments b" ++ printList(ys, printComponent(v)))
    | ([x, ...xs], [y, ...ys]) =>  Js.log(string_of_int(nx) ++ ", " ++ string_of_int(ny)); 
                                    if(outputs'(x) - nx == inputs'(y) - ny){
                                        let lhs =   switch(xs',ys'){
                                                    | ([], []) => Composition(x, y)
                                                    | (xs, []) => Composition(Tensor(List.concat([xs', [x]])), y)
                                                    | ([], ys) => Composition(x, Tensor(List.concat([ys', [y]])))
                                                    | (xs, ys) => Composition(Tensor(List.concat([xs', [x]])), Tensor(List.concat([ys', [y]])))
                                                    };
                                                    [lhs, ...applyTensor'(v,xs,0,ys,0,[],[])]
                                    } else {
                                        if(outputs'(x) - nx < inputs'(y) - ny) {
                                            Js.log(printComponent(v,x) ++ " is smaller than " ++ printComponent(v,y));
                                            applyTensor'(v, xs, 0, [y,...ys], ny + outputs'(x), List.concat([xs', [x]]), ys')
                                        } else {
                                            Js.log(printComponent(v,x) ++ " is bigger than " ++ printComponent(v,y));
                                            applyTensor'(v, [x,...xs], nx + inputs'(x), ys, 0, xs', List.concat([ys', [y]]))
                                        }
                                    }
    }
}

let rec evaluateOneStepTensor = (v,xs) => {
    switch(xs){
    | []         => []
    | [x, ...xs] => if (normalForm(x)){
                        [x, ...evaluateOneStepTensor(v,xs)]
                    } else {
                        List.concat([[evaluateOneStep({v:v, c:x}).c], xs])
                    }
    };
} and evaluateOneStepComposition = (v, x, y) => {
    if(normalForm(x)){
        switch(y){
        | Function(_,_,_,f) =>  f(v,x)
        | Tensor(ys)        =>  if(normalForm(Tensor(ys))){
                                    applyTensor(v, x, ys)
                                } else {
                                    Tensor(evaluateOneStepTensor(v,ys))
                                }
        | Composition(a,b)  => Composition(evaluateOneStepComposition(v,x,a),b)
        | f                 => Composition(x, evaluateOneStep({v:v, c:f}).c)
        }        
    } else {
        Composition(evaluateOneStep({v:v,c:x}).c, y)
    }
} and evaluateOneStep = ({v, c}) => {
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
    {v:v, c:eval}
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