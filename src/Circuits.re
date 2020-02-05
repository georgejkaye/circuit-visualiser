/**
 * File containing circuit and component definitions
 */

open Helpers;
open Lattices;

type linklist = list(((string,int),(string,int)))

exception SemanticsError(string);

let semanticsError = (message) => raise(SemanticsError(message));

/* A circuit is a component associated with a lattice v */
type component = 
    | Value(latticeElement)
    | Identity(int)
    | Composition(circuit, circuit)    
    | Tensor(list(circuit))
    | Swap(int, int)
    | Function(string, string, int, int, circuit => circuit)
    | Delay(int)
    | Trace(int, circuit)
    | Iter(int, circuit)
    | Inlink(int)
    | Outlink(int)
    | Link(int, int, circuit)
    | Macro(string, string, circuit)
and circuit = { 
    v: lattice,
    c: component,
    l: linklist
}

/* Look up the string of a link from its number */
let rec lookupLink = (i, l) => {
    switch(l){
    | [] => failwith("Link does not exist, maybe you should be using the option version?")
    | [l, ...ls] => snd(fst(l)) == i ? fst(fst(l)) 
                                     : (snd(snd(l))) == i ? fst(snd(l)) 
                                                          : lookupLink(i,ls)
    }
}

/* Check if a link int exists in the linklist */
let rec doesLinkNumberExist = (i, l) => {
    switch(l){
    | [] => false
    | [l, ...ls] => snd(fst(l)) == i ? true
                                     : (snd(snd(l))) == i ? true
                                                          : doesLinkNumberExist(i,ls)
    }
}

/* Check if a link string exists in the linklist */
let rec doesLinkStringExist = (i, l) => {
    switch(l){
    | [] => false
    | [l, ...ls] => fst(fst(l)) == i ? true
                                     : (fst(snd(l))) == i ? true
                                                          : doesLinkStringExist(i,ls)
    }
}

/************/
/* Printing */
/************/

/* Helper for printComponent. */
let rec printComponent' = (v, c, l, i) => {
    switch (c) {
    | Value(x)                          => v.print(x)
    | Identity(x)                       => string_of_int(x)
    | Composition(f, g)                 => let b = printCircuit'(f,0) ++ {js| ⋅ |js} ++ printCircuit'(g,0);
                                           i == 0 ? b : "(" ++ b ++ ")"
    | Tensor([])                        => ""
    | Tensor([x])                       => printCircuit'(x,i+1)
    | Tensor([f, ...tl])                => List.fold_left(((string, f') => string ++ {js| ⊗ |js} ++ printCircuit'(f',i+1)), printCircuit'(f,i+1), tl)
    | Swap(x,y)                         => {js|×{|js} ++ string_of_int(x) ++ ", " ++ string_of_int(y)
    | Function(id, _, _, _, _)          => id
    | Delay(x)                          => {js|ẟ{|js} ++ string_of_int(x) ++ "}"
    | Trace(x, f)                       => "Tr{" ++ string_of_int(x) ++ "}(" ++ printCircuit'(f,0) ++ ")" 
    | Iter(x, f)                        => "iter{" ++ string_of_int(x) ++ "}(" ++ printCircuit'(f,0) ++ ")" 
    | Inlink(int)                        => string_of_int(int)/*lookupLink(int,l)   */
    | Outlink(int)                       => string_of_int(int)/*lookupLink(int,l)*/
    | Link(inlink, outlink, f)          => "\\" ++ lookupLink(inlink,l) ++ "," ++ lookupLink(outlink,l) ++ ". " ++ printCircuit'(f,i) 
    | Macro(id, _, _)                   => id
    ;}
} and printComponent = (v, c, l) => printComponent'(v, c, l, 0)
and printCircuit' = ({v,c,l}, i) => printComponent'(v,c,l,i)
and printCircuit = (c) => printCircuit'(c,0)

let rec printComponentLatex' = (v, c, l, i) => {
    switch (c) {
        | Value(x)                          => "\\text{" ++ v.print(x) ++ "}"
        | Identity(x)                       => string_of_int(x)
        | Composition(f, g)                 => let b = printCircuitLatex'(f,0) ++ " \\cdot " ++ printCircuitLatex'(g,0);
                                               i == 0 ? b : "(" ++ b ++ ")"
        | Tensor([])                        => ""
        | Tensor([x])                       => printCircuitLatex'(x,i+1)
        | Tensor([f, ...tl])                => List.fold_left(((string, f') => string ++ " \\otimes " ++ printCircuitLatex'(f', i+1)), printCircuitLatex'(f,i+1), tl)
        | Swap(x, y)                        => "\\times_{" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ "}"
        | Function(_, latex,  _, _, _)      => latex
        | Delay(x)                          => "\\delta_" ++ string_of_int(x)
        | Trace(x, f)                       => "\\text{Tr}^" ++ string_of_int(x) ++ "(" ++ printCircuitLatex'(f,0) ++ ")" 
        | Iter(x, f)                        => "\\text{iter}^" ++ string_of_int(x) ++ "(" ++ printCircuitLatex'(f,0) ++ ")" 
        | Inlink(int)                        => lookupLink(int,l)   
        | Outlink(int)                       => lookupLink(int,l)
        | Link(inlink, outlink, f)          => "\\overline{" ++ lookupLink(inlink,l) ++ "\\to " ++ lookupLink(outlink,l) ++ "}." ++ printCircuitLatex'(f,i) 
        | Macro(_, latex, _)                => latex
        ;}
} and printComponentLatex = (v, c, l) => printComponentLatex'(v, c, l, 0)
and printCircuitLatex' = ({v,c,l}, i) => printComponentLatex'(v,c,l,i)
and printCircuitLatex = (c) => printCircuitLatex'(c,0);

/* Print a list of components in the form [c1 :: c2 :: c3 :: c4] */
let printComponentList = (v, l, xs) => printList(xs, (x) => printComponent(v,x,l));

/* Print a list of components in the form [c1 :: c2 :: c3 :: c4] */
let printComponentListLatex = (v, l, xs) => printList(xs, (x) => printComponentLatex(v,x,l));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printComponentListCommas = (v, l, xs) => printListCommas(xs, (x) => printComponent(v,x,l));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printComponentListLatexCommas = (v, l, xs) => printListCommas(xs, (x) => printComponentLatex(v,x,l));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printCircuitListCommas = (xs) => printListCommas(xs, (x) => printCircuit(x));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printCircuitListLatexCommas = (xs) => printListCommas(xs, (x) => printCircuitLatex(x));

/****************/
/* Input-output */
/****************/

/* Get the inputs of a component */
let rec inputs' = (c) => {
    switch (c) {
    | Value(_)                  => 0
    | Identity(x)               => x
    | Composition(f, _)         => inputs(f)
    | Tensor(fs)                => List.fold_left(((no, f) => no + inputs(f)), 0, fs)     
    | Swap(x,y)                 => x + y                   
    | Function(_, _, x, _, _)   => x
    | Delay(_)                  => 1
    | Trace(x, f)               => inputs(f) - x
    | Iter(x, f)                => inputs(f) - x
    | Inlink(_)                  => 0   
    | Outlink(_)                 => 1
    | Link(_, _, f)             => inputs(f)
    | Macro(_,_,f)              => inputs(f)
    ;}
}
and inputs = ({c}) => inputs'(c)


/* Get the outputs of a component */
let rec outputs' = (c) => {
    switch (c) {
    | Value(_)                  => 1
    | Identity(x)               => x
    | Composition(_, g)         => outputs(g)
    | Tensor(fs)                => List.fold_left(((no, f) => no + outputs(f)), 0, fs)   
    | Swap(x,y)                 => x + y 
    | Function(_, _, _, y, _)   => y
    | Delay(_)                  => 1
    | Trace(x, f)               => outputs(f) - x
    | Iter(_, f)                => outputs(f)
    | Inlink(_)                  => 1   
    | Outlink(_)                 => 0
    | Link(_, _, f)             => outputs(f)
    | Macro(_,_,f)              => outputs(f)
    ;}
}
and outputs = ({c}) => outputs'(c)

/**********************************************************/
/* Safe constructors                                      */
/* Use these to ensure ports are always mapped correctly! */
/**********************************************************/

let circ = (v,c,l) => {v,c,l}

/* Zero circuit, represents nothingness */
let zero = (v) => {v:v, c:Identity(0), l:[]}

/* Create a value */
let value = (v,x) => {v:v, c:Value(x), l:[]}

/* Create an identity circuit */
let idcirc = (v, n) => {v:v, c:Identity(n),l:[]}

/* Create a composition circuit */
let compose = (c, c') => {
    assert'(c.v == c'.v, semanticsError, "Circuits use different lattices!");
    assert'(outputs(c) == inputs(c'), semanticsError, "Outputs of circuit " ++ printCircuit(c) ++ " do not match inputs of circuit " ++ printCircuit(c'));
    {v:c.v, c:Composition(c, c'), l: c.l @ c'.l};
}

/* Create a tensor circuit */
let tensor = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), semanticsError, "Not all circuits use the same lattice!");
    /*let ys = List.map((x => x.c), xs);*/
    {v:List.hd(xs).v, c:Tensor(xs), l:List.fold_left(((x,y) => x @ y.l), [], xs)}
}

/* Create a function circuit */
let func = (v, id, latex, ins, outs, f) => {v, c: Function(id,latex,ins,outs,f), l:[]}

/* Check that the input and output link exist before stringing them together */
let rec findLink = (x, out, c) => {
    Js.log("searching for " ++ string_of_int(x) ++ " in " ++ printCircuit(c))
    switch (c.c) {
        | Value(_)                  => false
        | Identity(_)               => false
        | Composition(f, g)         => findLink(x, out, f) || findLink(x, out, g)
        | Tensor(fs)                => List.fold_left((f,g) => (findLink(x,out,g) || f), false, fs)   
        | Swap(_,_)                 => false
        | Function(_, _, _, _, _)   => false
        | Delay(_)                  => false
        | Trace(_, f)               => findLink(x, out, f)
        | Iter(_, f)                => findLink(x, out, f)
        | Inlink(y)                 => x == y ? (out ? false : true) : false
        | Outlink(y)                => x == y ? (out ? true : false) : false
        | Link(_, _, f)             => findLink(x, out, f)
        | Macro(_,_,f)              => findLink(x, out, f)
    }
}

/* Create a link circuit */
let link = (v, oux, inx, f, l) => {
    if(findLink(oux, true, f)){
        if(findLink(inx, false, f)){
            {v, c: Link(oux,inx,f),l}
        } else {

        let ins = lookupLink(inx,l);
        semanticsError("Link " ++ ins ++ " does not exist in circuit");

        }
    } else {
    
        let outs = lookupLink(oux,l);
        semanticsError("Link " ++ outs ++ " does not exist in circuit");
    }
    
}

/* Create a macro circuit */
let macro = (v, id, latex, f, l) =>
    {v, c: Macro(id, latex, f), l}

/*********************/
/* Helpful shortcuts */
/*********************/

/* Compose many circuits at once */
let rec composemany = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), semanticsError, "Not all circuits use the same lattice!");
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => compose(x, composemany(xs))
    }
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

/* Create a function that acts as a 'black box' - it transforms the inputs into the outputs but we don't know how exactly. */
let funcBlackBox = (v, id, latex, ins, outs) => {
    let rec bb = Function(id,latex,ins,outs, (c) => {
                    let id' = switch(c.c){
                    | Tensor(xs) => (printCircuitListCommas(xs), printCircuitListCommas(xs))
                    | _          => (printCircuit(c), printCircuitLatex(c))
                    };
                    circ(c.v, Function(id ++ "(" ++ fst(id') ++ ")", latex ++ "(" ++ snd(id') ++ ")", inputs(c), outs, (c) => composemany([c,{v,c:bb,l:c.l}])), c.l)
                });
    {v, c: bb, l:[]}
}

/*********************/
/* Special morphisms */
/*********************/

/* Fork a wire into two wires */
let fork = (v) => func(v,{js|⋏|js}, "\\curlywedge", 1, 2, (c) => circ(c.v, Tensor([c,c]), c.l));

/* Join two wires into one, taking the join of their values */
let rec join = (v) => func(v,{js|⋎|js}, "\\curlyvee",
                            2, 
                            1, 
                            (c) => switch(c.c){
                                        | Tensor([{v:_,c:Value(x),l:_}, {v:_,c:Value(y),l:_}]) => circ(c.v, Value(v.joinOp(x,y)), c.l)
                                        | Tensor([x, y]) => let newfun = Function(printCircuit(x) ++ {js| ⊔ |js} ++ printCircuit(y), 
                                                                     printCircuitLatex(x) ++ " \\sqcup " ++ printCircuitLatex(y), 
                                                                     inputs(c), 1, (c) => compose(c, join(v)));
                                                            circ(c.v,newfun,c.l)
                                        | _ => failwith("Join can only take two arguments")
                                    }
                        )  

/* Stub a wire, leading to the unique idcirc on 0 */
let stub = (v) => func(v, {js|~|js}, "{\\sim}", 1, 0, (_) => zero(v));

let specialMorphisms = (v) => [fork(v), join(v), stub(v)];

/* Swap buses of width x and y */
let swap = (v, x, y) => {v, c:Swap(x, y), l:[]}

/* Fork all wires in a bus */
let rec dfork = (v,n) => {
    let comp = switch(n){
        | 0 => Identity(0)
        | 1 => fork(v).c
        | n => composemany([
                        tensor([dfork(v,n-1), fork(v)]),
                        tensor([idcirc(v,n-1), swap(v, n-1, 1), idcirc(v,1)])
                        ]).c
        };
    macro(v, {js|Δ{|js} ++ string_of_int(n) ++ "}", "\\Delta_" ++ string_of_int(n), circ(v,comp,[]), [])
}

/* Join all wires in a bus */
let rec djoin = (v,n) => {
    let comp = switch(n){
        | 0 => Identity(0)
        | 1 => join(v).c
        | n => composemany([
                        tensor([idcirc(v,n-1), swap(v, 1, n-1), idcirc(v,1)]),
                        tensor([djoin(v,n-1), join(v)])
                        ]).c
        };
    macro(v, {js|∇{|js} ++ string_of_int(n) ++ "}", "\\nabla_" ++ string_of_int(n), circ(v,comp,[]), [])
}

/*********/
/* Delay */
/*********/

/* Create a delay */
let delay = (v,n) => {v:v, c:Delay(n), l:[]}

/***********************/
/* Trace and iteration */
/***********************/

/* Create a trace circuit */
let trace = (x, f) => {
    assert'(inputs(f) >= x && outputs(f) >= x, semanticsError, "Inputs and outputs of circuit " ++ printCircuit(f) ++ " are less than the size of the trace.");
    circ(f.v, Trace(x,f), f.l)
}

/* Create an iteration circuit */
let iter = (f) => {
    assert'(inputs(f) >= outputs(f), semanticsError, "Not enough inputs of circuit " ++ printCircuit(f) ++ " to iterate.");
    circ(f.v, Iter(outputs(f), f), f.l);
}

/***********************/
/* Regular expressions */
/***********************/

/* Regexes for various constructs */
let exponentialSoloRegEx = [%bs.re "/\\^([0-9]+)/"]
let exponentialRegEx = [%bs.re "/(.+)?\\^([0-9]+)/"]
let delayRegEx = [%bs.re "/o\\{([0-9]+)\\}/"];
let djoinRegEx = [%bs.re "/\\\\\\/\\{([0-9]+)\\}/"];
let swapRegEx = [%bs.re "/x\\{([0-9]+),([0-9]+)\\}/"];
let dforkRegEx = [%bs.re "/\\/\\\\{([0-9]+)\\}/"];
let traceRegEx = [%bs.re "/Tr\\{([0-9]+)\\}/"]
let iterRegEx = [%bs.re "/iter\\{([0-9]+)\\}/"]
let iterRegEx2 = [%bs.re "/iter/"]
let linkIntroRegEx = [%bs.re "/\\\\([a-z]+),([a-z]+)\\./"]
let linkIntroRegEx2 = [%bs.re "/\\\\([a-z])([a-z])\\./"]

let constructRegExes = [
    swapRegEx, 
    dforkRegEx, 
    djoinRegEx, 
    delayRegEx, 
    traceRegEx, 
    iterRegEx, 
    iterRegEx2, 
    exponentialSoloRegEx, 
    linkIntroRegEx, 
    linkIntroRegEx2
]