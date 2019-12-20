open Circuits;

exception SemanticsError(string);
let semanticsError = (message) => raise(SemanticsError(message));

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

let identity = (array) => array

let compose = (f,g) => {

    assert(Array.length(f.outputs.sources) == Array.length(g.inputs.targets));

    for (i in 0 to Array.length(f.outputs.sources) - 1){
        let (e,k) = f.outputs.sources[i];
        let (e',k') = g.inputs.targets[i];
    
        e^.sources[k] = (e',k');
        e'^.sources[k'] = (e,k);

    };

    {inputs: f.inputs, edges: f.edges @ g.edges, outputs: g.outputs}

}

let floatingEdge = (id,label) => {id, sources:[||], targets:[||], label}

let rec convertCircuitToHypernet = (circuit) => fst(convertCircuitToHypernet'(circuit, 0))
and convertCircuitToHypernet' = (circuit, i) => {
    
    switch(circuit.c){
    | Value(x)           => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:circuit.v.print(x)})
                            and ine = ref(floatingEdge(i,"inputs")) 
                            and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:"outputs"});
                            ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Composition(f,g)   => let fh = convertCircuitToHypernet'(f,i);
                            let gh = convertCircuitToHypernet'(g,snd(fh));
                            (compose(fst(fh),fst(gh)), snd(gh))
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

    let instring = inports == "{}" ? "" : inports ++ "|";
    let outstring = outports == "{}" ? "" : "|" ++ outports; 

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
    | n => string_of_int(n) ++ " <f" ++ string_of_int(x) ++ "> | "
    }
} and generateTransitions = (x, targets) => {
    let string = ref("");
    for(i in 0 to Array.length(targets) - 1){
        let (e,k) = targets[i];
        string := "\"edge" ++ string_of_int(x) ++ "\":f" ++ string_of_int(i) ++ " -> \"edge" ++ string_of_int(e^.id) ++ "\":f" ++ string_of_int(k) ++ ";\n"
    }
    string^;
}

let zeroNet = {inputs:floatingEdge(0, "inputs"), edges:[], outputs:floatingEdge(1,"outputs")}
let zeroDot = generateGraphvizCode(zeroNet);
Js.log(zeroDot);