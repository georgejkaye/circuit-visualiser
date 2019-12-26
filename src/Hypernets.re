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

let traceHypernet = (x, h) => {

    for(i in 0 to x-1){

        let (e, k) = h.inputs.targets[i];
        let (e', k') = h.outputs.sources[i];

        e'^.targets[k'] = (e, k);
        e^.sources[k] = (e',k');
    };

    let newInputs = {id: h.inputs.id, sources: [||], targets:Array.sub(h.inputs.targets, x, (Array.length(h.inputs.targets) - x)), label:"inputs"};
    let newOutputs = {id: h.outputs.id, sources: Array.sub(h.outputs.sources, x, (Array.length(h.outputs.sources) - x)), targets: [||], label:"outputs"};

    let refInputs = ref(newInputs);
    let refOutputs = ref(newOutputs);

    for(i in 0 to Array.length(newInputs.targets) - 1){
        let (e, k) = newInputs.targets[i];
        let k = (e^.label == "outputs") ? k - x : k;
        newInputs.targets[i] = (e,k)
    };

    for(i in 0 to Array.length(newOutputs.targets) - 1){
        let (e, k) = newInputs.sources[i];
        let k = (e^.label == "inputs") ? k - x : k;
        newInputs.sources[i] = (e,k)
    };

    for(i in 0 to Array.length(newInputs.targets) - 1){
        let (e, k) = newInputs.targets[i];
        let (e',k') = e^.sources[k];
        e^.sources[k] = (e',k'-x);
    };

    for(i in 0 to Array.length(newOutputs.sources) - 1){
        let (e, k) = newOutputs.sources[i];
        let (e',k') = e^.targets[k];
        e^.targets[k] = (e',k'-x);
    };

    {inputs: newInputs, edges: h.edges, outputs: newOutputs}
    
}

let rec convertCircuitToHypernet = (circuit) => fst(convertCircuitToHypernet'(circuit, 0))
and convertCircuitToHypernet' = (circuit, i) => {
    
    switch(circuit.c){
    | Value(x)                  => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:circuit.v.print(x)})
                                   and ine = ref(floatingEdge(i,"inputs")) 
                                   and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:"outputs"});
                                   ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Identity(n)               => let rec ine = ref(floatingEdge(i,""));
                                   let oute = ref({id:i+1, sources:Array.init(n, (n) => (ine, n)), targets:[||], label:"outputs"});
                                   ine := {id:i, sources:[||], targets:Array.init(n, (n) => (oute, n)), label:"inputs"};
                                   ({inputs:ine^, edges: [], outputs: oute^},i+2)
    | Composition(f,g)          => let fh = convertCircuitToHypernet'(f,i);
                                   let gh = convertCircuitToHypernet'(g,snd(fh));
                                   (composeSequential(fst(fh),fst(gh)), snd(gh))
    | Tensor([x,...xs])         => List.fold_left((f,g) => { let gh = convertCircuitToHypernet'(g,snd(f));
                                                 (composeParallel(fst(f),fst(gh)), snd(gh));}, 
                                                 (convertCircuitToHypernet'(x, i)), xs)
    | Function(id,_,ins,outs,_) =>  let ine = ref(floatingEdge(i,""));
                                    let oute = ref(floatingEdge(i+2,""));
                                    let fune = ref({id:i+1, sources:Array.init(ins, (n) => (ine, n)), targets:Array.init(outs, (n) => (oute, n)), label:id});
                                    ine := {id:i, sources:[||], targets:Array.init(ins, (n) => (fune, n)), label:"inputs"};
                                    oute := {id:i+2, sources:Array.init(outs, (n) => (fune, n)), targets:[||], label:"outputs"};
                                    ({inputs:ine^, edges: [fune], outputs:oute^}, i+3)
    | Delay(x)                  =>  let ine = ref(floatingEdge(i,""));
                                    let oute = ref(floatingEdge(i+2,""));
                                    let fune = ref({id:i+1, sources:[|(ine,0)|], targets:[|(oute,0)|], label:("&delta;" ++ generateUnicodeSubscript(x))});
                                    ine := {id:i, sources:[||], targets:[|(fune, 0)|], label:"inputs"};
                                    oute := {id:i+2, sources:[|(fune,0)|], targets:[||], label:"outputs"};
                                    ({inputs:ine^, edges: [fune], outputs:oute^}, i+3)
    | Trace(x, f)               => let (fh,i') = convertCircuitToHypernet'(f,i+1);
                                    (traceHypernet(x,fh), i')                           
                                    
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
    let inports = generatePorts(ins, false);
    let outports = generatePorts(outs, true);
    let transitions = generateTransitions(edge.id, edge.targets);

    let instring = inports == "{}" ? "" : inports ++ " | ";
    let outstring = outports == "{}" ? "" : " | " ++ outports; 

    ("edge" ++ string_of_int(edge.id) ++ 
        " [shape=record,label=\"" ++ 
        instring ++ edge.label ++ outstring
        ++ "\"]", transitions)  
} and generatePorts = (n, out) => {
    "{" ++ generatePorts'(0,n, out) ++ "}"
} and generatePorts' = (x,n,out) => {
    let y = out ? "o" : "i"
    switch(n){
    | 0 => ""
    | 1 => "<" ++ y ++ string_of_int(x) ++ ">"
    | n => "<" ++ y ++ string_of_int(x) ++ "> | " ++ generatePorts'(x+1,n-1,out)
    }
} and generateTransitions = (x, targets) => {
    let string = ref("");
    for(i in 0 to Array.length(targets) - 1){
        let (e,k) = targets[i];
        string := string^ ++ "\"edge" ++ string_of_int(x) ++ "\":o" ++ string_of_int(i) ++ " -> \"edge" ++ string_of_int(e^.id) ++ "\":i" ++ string_of_int(k) ++ ";\n"
    }
    string^;
}

let zeroDot = generateGraphvizCode(zeroNet);
Js.log(zeroDot);