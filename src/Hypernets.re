
exception SemanticsError(string);
let semanticsError = (message) => raise(SemanticsError(message));

/* An edge represents a morphism between buses */
type edge = { 
    id: int,
    sources: array((ref(edge), int)),                    
    targets: array((ref(edge), int)),
    label: string,
    rule: array(ref(edge)) => array(ref(edge))   /* rewrite rule */
} and hypernet = {
    inputs: edge,
    edges: list(ref(edge)),
    outputs: edge
}

let identity = (array) => array

let compose = (f,g) => {

    assert(Array.length(f.outputs.sources) == Array.length(g.inputs.targets));

    for (i in 0 to Array.length(f.outputs.sources)){
        let (e,k) = f.outputs.sources[i];
        let (e',k') = g.inputs.targets[i];
    
        e^.sources[k] = (e',k');
        e'^.sources[k'] = (e,k);

    };

    {inputs: f.inputs, edges: f.edges @ g.edges, outputs: g.outputs}

}

let rec convertCircuitToHypernet = (circuit) => {

}

let rec generateGraphvizCode = (net) => {
    let graph = generateGraphvizCodeEdges(net.edges, "", "");
    "digraph{" ++ graph ++ "}"
} and generateGraphvizCodeEdges = (edges, nodes, transitions) => {
    switch(edges){
    | [] => nodes ++ transitions
    | [x,...xs] => let edgecode = generateGraphvizCodeEdge(x);
                    generateGraphvizCodeEdges(edges, nodes ++ fst(edgecode), transitions ++ snd(edgecode))
    }
} and generateGraphvizCodeEdge = (edge) => {
    let ins = Array.length(edge^.sources);
    let outs = Array.length(edge^.targets);
    let inports = generatePorts(ins);
    let outports = generatePorts(outs);
    let transitions = generateTransitions(edge^.id, edge^.targets);

    ("edge" ++ string_of_int(edge^.id) ++ 
        " [shape=record,label=\"" ++ 
        inports ++ " | " ++ edge^.label ++ " | " ++ outports
        ++ "]", transitions)  
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
    for(i in 0 to Array.length(targets)){
        let (e,k) = targets[i];
        string := "\"node" ++ string_of_int(x) ++ "\":f" ++ string_of_int(i) ++ " -> \"node" ++ string_of_int(e^.id) ++ "\":f" ++ string_of_int(k) ++ ";\n"
    }
    string^;
}