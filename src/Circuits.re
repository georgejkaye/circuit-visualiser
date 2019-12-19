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
    | Value((int,int))
    | Identity(int)
    | Composition(component, component)    
    | Tensor(list(component))
    | Function(string, string, int, int, (lattice, component, linklist) => component)
    | Delay(int)
    | Trace(int, component)
    | Iter(int, component)
    | Input(int)
    | Output(int)
    | Link(int, int, component)
    | Macro(string, string, component)
and circuit = { 
    v: lattice,
    c: component,
    l: linklist
}

/* Look up the string of a link from its number */
let rec lookupLink = (i, l) => {
    switch(l){
    | [] => failwith("link not in list")
    | [l, ...ls] => snd(fst(l)) == i ? fst(fst(l)) 
                                     : (snd(snd(l))) == i ? fst(snd(l)) 
                                                          : lookupLink(i,ls)
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
    | Composition(f, g)                 => let b = printComponent'(v,f,l,0) ++ {js| ⋅ |js} ++ printComponent'(v,g,l,0);
                                           i == 0 ? b : "(" ++ b ++ ")"
    | Tensor([])                        => ""
    | Tensor([x])                       => printComponent'(v,x,l,i+1)
    | Tensor([f, ...tl])                => List.fold_left(((string, comp) => string ++ {js| ⊗ |js} ++ printComponent'(v,comp,l,i+1)), printComponent'(v,f,l,i+1), tl)
    | Function(id, _, _, _, _)             => id
    | Delay(x)                          => {js|ẟ{|js} ++ string_of_int(x) ++ "}"
    | Trace(x, component)               => "Tr{" ++ string_of_int(x) ++ "}(" ++ printComponent'(v,component,l,0) ++ ")" 
    | Iter(x, component)                => "iter{" ++ string_of_int(x) ++ "}(" ++ printComponent'(v,component,l,0) ++ ")" 
    | Input(int)                        => lookupLink(int,l)   
    | Output(int)                       => lookupLink(int,l)
    | Link(inlink, outlink, circuit)    => "\\" ++ lookupLink(inlink,l) ++ "," ++ lookupLink(outlink,l) ++ ". " ++ printComponent'(v,circuit,l,i) 
    | Macro(id, _, _)                      => id
    ;}
}

/* Get a string representation of a component */
let printComponent = (v, c, l) => printComponent'(v, c, l, 0);

/* Get a string representation of a circuit */
let printCircuit = ({v,c,l}) => printComponent(v,c,l)

let rec printComponentLatex' = (v, c, l, i) => {
    switch (c) {
        | Value(x)                          => "\\text{" ++ v.print(x) ++ "}"
        | Identity(x)                       => string_of_int(x)
        | Composition(f, g)                 => let b = printComponentLatex'(v,f,l,0) ++ " \\cdot " ++ printComponentLatex'(v,g,l,0);
                                               i == 0 ? b : "(" ++ b ++ ")"
        | Tensor([])                        => ""
        | Tensor([x])                       => printComponentLatex'(v,x,l,i+1)
        | Tensor([f, ...tl])                => List.fold_left(((string, comp) => string ++ " \\otimes " ++ printComponentLatex'(v,comp, l, i+1)), printComponentLatex'(v,f, l,i+1), tl)
        | Function(id, latex,  _, _, _)     => latex
        | Delay(x)                          => "\\delta_" ++ string_of_int(x)
        | Trace(x, component)               => "\\text{Tr}^" ++ string_of_int(x) ++ "(" ++ printComponentLatex'(v,component,l,0) ++ ")" 
        | Iter(x, component)                => "\\text{iter}^" ++ string_of_int(x) ++ "(" ++ printComponentLatex'(v,component,l,0) ++ ")" 
        | Input(int)                        => lookupLink(int,l)   
        | Output(int)                       => lookupLink(int,l)
        | Link(inlink, outlink, circuit)    => "\\overline{" ++ lookupLink(inlink,l) ++ "|" ++ lookupLink(outlink,l) ++ "}." ++ printComponentLatex'(v,circuit,l,i) 
        | Macro(_, latex, _)                      => latex
        ;}
}

/* Get a string representation of a component */
let printComponentLatex = (v, c, l) => printComponentLatex'(v, c, l, 0);

/* Get a string representation of a circuit */
let printCircuitLatex = ({v,c,l}) => printComponentLatex(v,c,l)

/* Print a list of components in the form [c1 :: c2 :: c3 :: c4] */
let printComponentList = (v, l, xs) => printList(xs, (x) => printComponent(v,x,l));

/* Print a list of components in the form [c1 :: c2 :: c3 :: c4] */
let printComponentListLatex = (v, l, xs) => printList(xs, (x) => printComponentLatex(v,x,l));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printComponentListCommas = (v, l, xs) => printListCommas(xs, (x) => printComponent(v,x,l));

/* Print a list of components in the form [c1, c2, c3, c4] */
let printComponentListLatexCommas = (v, l, xs) => printListCommas(xs, (x) => printComponentLatex(v,x,l));

/****************/
/* Input-output */
/****************/

/* Get the inputs of a component */
let rec inputs' = (c) => {
    switch (c) {
    | Value(_)                  => 0
    | Identity(x)               => x
    | Composition(f, _)         => inputs'(f)
    | Tensor(fs)                => List.fold_left(((no, comp) => no + inputs'(comp)), 0, fs)                        
    | Function(_, _, x, _, _)   => x
    | Delay(_)                  => 1
    | Trace(x, comp)            => inputs'(comp) - x
    | Iter(x, comp)             => inputs'(comp) - x
    | Input(_)                  => 0   
    | Output(_)                 => 1
    | Link(_, _, circuit)       => inputs'(circuit)
    | Macro(_,_,f)                => inputs'(f)
    ;}
}

/* Get the inputs of a circuit */
let inputs = ({c}) => inputs'(c)


/* Get the outputs of a component */
let rec outputs' = (c) => {
    switch (c) {
    | Value(_)                  => 1
    | Identity(x)               => x
    | Composition(_, g)         => outputs'(g)
    | Tensor(fs)                => List.fold_left(((no, comp) => no + outputs'(comp)), 0, fs)    
    | Function(_, _, _, y, _)   => y
    | Delay(_)                  => 1
    | Trace(x, comp)            => outputs'(comp) - x
    | Iter(_, comp)             => outputs'(comp)
    | Input(_)                  => 1   
    | Output(_)                 => 0
    | Link(_, _, circuit)       => outputs'(circuit)
    | Macro (_,_,f)               => outputs'(f)
    ;}
}

/* Get the outputs of a circuit */
let outputs = ({c}) => outputs'(c)

/**********************************************************/
/* Safe constructors                                      */
/* Use these to ensure ports are always mapped correctly! */
/**********************************************************/

/* Zero circuit, represents nothingness */
let zero = (v) => {v:v, c:Identity(0), l:[]}

/* Create a value */
let value = (v,x) => {v:v, c:Value(x), l:[]}

/* Create an identity */
let identity = (v, n) => {v:v, c:Identity(n),l:[]}

/* Compose two components together */
let compose' = (v, c, l, c', l') => {
    if(outputs'(c) != inputs'(c')){
        semanticsError("Outputs of circuit " ++ printComponent(v,c,l) ++ " do not match inputs of circuit " ++ printComponent(v,c',l'))
    } else {
        Composition(c, c')
    }
}

/* Create a composition circuit */
let compose = (c, c') => {
    assert'(c.v == c'.v, "Circuits use different lattices!");
    {v:c.v, c:compose'(c.v, c.c, c.l, c'.c, c'.l), l: c.l @ c'.l};
}

/* Create a tensor circuit */
let tensor = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), "Not all circuits use the same lattice!");
    let ys = List.map((x => x.c), xs);
    {v:List.hd(xs).v, c:Tensor(ys), l:List.fold_left(((x,y) => x @ y.l), [], xs)}
}

/* Create a tensor component */
let tensor' = (v, xs) => {
    Tensor(xs)
}

/* Create a function component */
let func' = (id, latex, ins, outs, f) => Function(id,latex,ins,outs,f)

/* Create a function circuit */
let func = (v, id, latex, ins, outs, f, l) => {v, c: func'(id,latex,ins,outs,f), l}

/* Create a macro circuit */
let macro = (v, id, latex, f, l) =>
    {v, c: Macro(id, latex, f), l}

/*********************/
/* Helpful shortcuts */
/*********************/

/* Compose many circuits at once */
let rec composemany = (xs) => {
    assert'(List.fold_left((x,y) => x && (y.v == List.hd(xs).v), true, xs), "Not all circuits use the same lattice!");
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => compose(x, composemany(xs))
    }
}

/* Compose many components at once */
/*let rec composemany' = (v, xs) => {
    switch(xs){
    | []         => failwith("no args")
    | [x]        => x
    | [x, ...xs] => compose'(v, x, composemany'(v, xs))
    }
}*/


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
    let rec bb = Function(id,latex,ins,outs, (v,c,l) => {
                    let id' = switch(c){
                    | Tensor(xs) => (printComponentListCommas(v,l,xs), printComponentListLatexCommas(v,l,xs))
                    | _          => (printComponent(v,c,l), printComponentLatex(v,c,l))
                    };
                    Function(id ++ "(" ++ fst(id') ++ ")", latex ++ "(" ++ snd(id') ++ ")", inputs'(c), outs, (v,c,l) => composemany([{v,c,l},{v,c:bb,l}]).c)
                });
    {v, c: bb, l:[]}
}

/*********************/
/* Special morphisms */
/*********************/

/* Fork a wire into two wires */
let fork = (v) => func(v,{js|⋏|js}, "\\curlywedge", 1, 2, (_, c,_) => Tensor([c, c]),[]);

/* Join two wires into one, taking the join of their values */
let rec join = (v) => func(v,{js|⋎|js}, "\\curlyvee",
                            2, 
                            1, 
                            (v, c, l) => switch(c){
                                        | Tensor([Value(x), Value(y)]) => Value(v.joinOp(x,y))
                                        | Tensor([x, y]) => Function(printComponent(v,x,l) ++ {js| ⊔ |js} ++ printComponent(v,y,l), 
                                                                     printComponentLatex(v,x,l) ++ " \\sqcup " ++ printComponentLatex(v,y,l), 
                                                                     inputs'(c), 1, (v,c,l) => compose({v,c,l}, join(v)).c)
                                        | _ => failwith("Join can only take two arguments")
                                    },
                            []
                        )  

/* Stub a wire, leading to the unique identity on 0 */
let stub = (v) => func(v, {js|~|js}, "{\\sim}", 1, 0, (_,_,_) => Identity(0),[]);

let specialMorphisms = (v) => [fork(v).c, join(v).c, stub(v).c];

/* Swap buses of width x and y */
let swap = (v, x, y) => func(v, {js|×|js} ++ "{" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ "}", 
                                "\\times_{" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ "}",
                                x + y, 
                                x + y, 
                                (_, comp,_) => switch(comp){
                                | Tensor(xs) => let (top, bot) = split(x, xs); Tensor(List.concat([bot, top]));
                                | _ => failwith("Swap can only swap a tensor")
                                }, []
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
    macro(v, {js|Δ{|js} ++ string_of_int(n) ++ "}", "\\Delta_" ++ string_of_int(n), comp, [])
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
    macro(v, {js|∇{|js} ++ string_of_int(n) ++ "}", "\\nabla_" ++ string_of_int(n), comp, [])
}

/*********/
/* Delay */
/*********/

/* Create a delay */
let delay = (v,n) => {v:v, c:Delay(n), l:[]}

/***********************/
/* Trace and iteration */
/***********************/

/* Create a trace component */
let trace' = (v, x, f, l) => {
    assert'(inputs'(f) >= x && outputs'(f) >= x, "Inputs and outputs of circuit " ++ printComponent(v,f,l) ++ " are less than the size of the trace.");
    Trace(x, f)
}

/* Create a trace circuit */
let trace = (x, f) => {
    {v:f.v, c:trace'(f.v, x, f.c, f.l), l:f.l}
}

/* Create an iteration component */
let iter' = (v,f,l) => {
    assert'(inputs'(f) >= outputs'(f), "Not enough inputs of circuit " ++ printComponent(v,f,l) ++ " to iterate.");
    Iter(outputs'(f), f)
}

/* Create an iteration circuit */
let iter = (f) => {
    {v:f.v, c:iter'(f.v, f.c, f.l), l:f.l}
}

/***********************/
/* Regular expressions */
/***********************/

/* Regexes for various constructs */
let exponentialSoloRegEx = [%bs.re "/\^([0-9]+)/"]
let exponentialRegEx = [%bs.re "/(.+)?\^([0-9]+)/"]
let delayRegEx = [%bs.re "/o\{([0-9]+)\}/"];
let djoinRegEx = [%bs.re "/\\\\\/\{([0-9]+)\}/"];
let swapRegEx = [%bs.re "/x\{([0-9]+),([0-9]+)\}/"];
let dforkRegEx = [%bs.re "/\/\\\{([0-9]+)\}/"];
let traceRegEx = [%bs.re "/Tr\{([0-9]+)\}/"]
let iterRegEx = [%bs.re "/iter\\{([0-9]+)\\}/"]
let iterRegEx2 = [%bs.re "/iter/"]
let linkIntroRegEx = [%bs.re "/\\\\([a-z]+),([a-z]+)\./"]
let linkIntroRegEx2 = [%bs.re "/\\\\([a-z])([a-z])\./"]

let constructRegExes = [swapRegEx, djoinRegEx, dforkRegEx, delayRegEx, traceRegEx, iterRegEx, iterRegEx2, exponentialSoloRegEx, linkIntroRegEx, linkIntroRegEx2]