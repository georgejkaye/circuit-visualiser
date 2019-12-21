open Circuits;
open Helpers;

exception GraphError(string);
let graphError = (message) => raise(GraphError(message));

/* An edge represents a morphism between buses */
type edge = { 
    id: int,
    sources: array((ref(edge), int)),                    
    targets: array((ref(edge), int)),
    label: string
} and hypernet = {
    inputs: edge,
    edges: list(ref(edge)),
    outputs: edge
}

let rec printEdge = (e) => {
    "edge " ++ string_of_int(e.id) ++ ": " ++ e.label ++ ", sources: " ++ printEdgeRefPortPairArray(e.sources) ++ ", targets: " ++ printEdgeRefPortPairArray(e.targets)
} and printEdgeArray = (es) => printArray(es, printEdge)
and printEdgeRefArray = (es) => printArray(es, (x) => printEdge(x^))
and printEdgeRefPortPairArray = (es) => printArray(es, ((x,n)) => (x^.label) ++ ":" ++ string_of_int(n))
and printEdgeRefList = (es) => printList(es, (x) => printEdge(x^))

let printHypernet = (h) => {
    "hypernet inputs " ++ printEdge(h.inputs) ++ ", outputs " ++ printEdge(h.outputs) ++ ", edges " ++ printEdgeRefList(h.edges)
}


let floatingEdge = (id,label) => {id, sources:[||], targets:[||], label} 

let zeroNet = {inputs:floatingEdge(0, "inputs"), edges:[], outputs:floatingEdge(1,"outputs")}
let identity = (array) => array

let composeSequential = (f,g) => {

    assert(Array.length(f.outputs.sources) == Array.length(g.inputs.targets));
    Js.log("Composing " ++ printHypernet(f) ++ " and " ++ printHypernet(g));

    for (i in 0 to Array.length(f.outputs.sources) - 1){
        let (e,k) = f.outputs.sources[i];
        let (e',k') = g.inputs.targets[i];
    
        e^.targets[k] = (e',k');
        e'^.sources[k'] = (e,k);

    };

    {inputs: f.inputs, edges: f.edges @ g.edges, outputs: g.outputs}

}

let composeParallel = (f,g) => {

    let newInputs = {id: f.inputs.id, sources: [||], targets: Array.append(f.inputs.targets, g.inputs.targets), label: "inputs"};
    let newOutputs = {id: f.outputs.id, sources: Array.append(f.outputs.sources, g.outputs.sources), targets: [||], label: "outputs"};

    let fins = Array.length(f.inputs.targets);
    let fouts = Array.length(f.outputs.sources);

    for(i in 0 to fins - 1){
        let (e,k) = f.inputs.targets[i];
        e^.sources[k] = (ref(newInputs), i);
    };

    for(i in 0 to Array.length(g.inputs.targets) - 1){
        let (e,k) = g.inputs.targets[i];
        e^.sources[k] = (ref(newInputs), i + fins);
    };

    for(i in 0 to fouts - 1){
        let (e,k) = f.outputs.sources[i];
        e^.targets[k] = (ref(newOutputs), i);
    };

    for(i in 0 to Array.length(g.outputs.sources) - 1){
        let (e,k) = g.outputs.sources[i];
        e^.targets[k] = (ref(newOutputs), i + fouts);
    };

    {inputs: newInputs, edges: f.edges @ g.edges, outputs: newOutputs}

}

let rec convertCircuitToHypernet = (circuit) => fst(convertCircuitToHypernet'(circuit, 0))
and convertCircuitToHypernet' = (circuit, i) => {
    
    switch(circuit.c){
    | Value(x)           => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:circuit.v.print(x)})
                            and ine = ref(floatingEdge(i,"inputs")) 
                            and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:"outputs"});
                            ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Identity(n)        => let rec ine = ref(floatingEdge(i,""));
                            let oute = ref({id:i+1, sources:Array.init(n, (n) => (ine, n)), targets:[||], label:"outputs"});
                            ine := {id:i, sources:[||], targets:Array.init(n, (n) => (oute, n)), label:"inputs"};
                            ({inputs:ine^, edges: [], outputs: oute^},i+2)
    | Composition(f,g)   => let fh = convertCircuitToHypernet'(f,i);
                            let gh = convertCircuitToHypernet'(g,snd(fh));
                            (composeSequential(fst(fh),fst(gh)), snd(gh))
    | Tensor([x,...xs])         => List.fold_left((f,g) => { let gh = convertCircuitToHypernet'(g,snd(f));
                                                 (composeParallel(fst(f),fst(gh)), snd(gh));}, 
                                                 (convertCircuitToHypernet'(x, i)), xs)
    }
}

let rec generateGraphvizCode = (net) => {
    let graph = generateGraphvizCodeEdges([net.inputs, net.outputs, ...List.map((x) => x^, net.edges)], "", "");
    "digraph{\n" ++ graph ++ "}"
} and generateGraphvizCodeEdges = (edges, nodes, transitions) => {
    switch(edges){
    | [] => nodes ++ transitions
    | [x,...xs] => let edgecode = generateGraphvizCodeEdge(x);
                    let edgedot = fst(edgecode) == "" ? "" : fst(edgecode) ++ ";\n";
                    let transdot = snd(edgecode);
                    generateGraphvizCodeEdges(xs, nodes ++ edgedot, transitions ++ transdot)
    }
} and generateGraphvizCodeEdge = (edge) => {
    let ins = Array.length(edge.sources);
    let outs = Array.length(edge.targets);
    let inports = generatePorts(ins);
    let outports = generatePorts(outs);
    let transitions = generateTransitions(edge.id, edge.targets);

    let instring = inports == "{}" ? "" : inports ++ " | ";
    let outstring = outports == "{}" ? "" : " | " ++ outports; 

    ("edge" ++ string_of_int(edge.id) ++ 
        " [shape=record,label=\"" ++ 
        instring ++ edge.label ++ outstring
        ++ "\"]", transitions)  
} and generatePorts = (n) => {
    "{" ++ generatePorts'(0,n) ++ "}"
} and generatePorts' = (x,n) => {
    switch(n){
    | 0 => ""
    | 1 => "<f" ++ string_of_int(x) ++ ">"
    | n => "<f" ++ string_of_int(x) ++ "> | " ++ generatePorts'(x+1,n-1)
    }
} and generateTransitions = (x, targets) => {
    let string = ref("");
    for(i in 0 to Array.length(targets) - 1){
        let (e,k) = targets[i];
        string := string^ ++ "\"edge" ++ string_of_int(x) ++ "\":f" ++ string_of_int(i) ++ " -> \"edge" ++ string_of_int(e^.id) ++ "\":f" ++ string_of_int(k) ++ ";\n"
    }
    string^;
}

let zeroDot = generateGraphvizCode(zeroNet);
Js.log(zeroDot);