open Circuits;
open Helpers;

let alpha = {js|α|js}
let omega = {js|ω|js}
let dot = {js|•|js}

let alphaLatex = "\\alpha";
let omegaLatex = "\\omega";
let stubLatex = "\\sim";
let forkLatex = "\\curlywedge";
let joinLatex = "\\curlyvee";
let dforkLatex = "\\Delta";
let djoinLatex = "\\nabla";

exception GraphError(string);
let graphError = (message) => raise(GraphError(message));

/* An edge represents a morphism between buses */
type edge = { 
    id: int,
    sources: array((ref(edge), int)),                    
    targets: array((ref(edge), int)),
    label: string,
    latex: string
} and hypernet = {
    inputs: ref(edge),
    edges: list(ref(edge)),
    outputs: ref(edge)
}

/* Find all the edges with a given label in a net */
let rec findEdges = (string, net) => findEdges'(string, net.edges, []) 
and findEdges' = (string, edges, acc) => {
    switch(edges){
    | [] => acc
    | [x,...xs] => x^.label == string ? findEdges'(string, xs, [x,...acc]) :
                                       findEdges'(string, xs, acc)
    }
}

let rec printEdge = (e) => {
    "edge " ++ string_of_int(e.id) ++ ": " ++ e.label ++ ", sources: " ++ printEdgeRefPortPairArray(e.sources) ++ ", targets: " ++ printEdgeRefPortPairArray(e.targets)
} and printEdgeArray = (es) => printArray(es, printEdge)
and printEdgeRefArray = (es) => printArray(es, (x) => printEdge(x^))
and printEdgeRefPortPairArray = (es) => printArray(es, ((x,n)) => (x^.label) ++ ":" ++ string_of_int(n))
and printEdgeRefList = (es) => printList(es, (x) => printEdge(x^))

let printHypernet = (h) => {
    "hypernet inputs " ++ printEdge(h.inputs^) ++ ", outputs " ++ printEdge(h.outputs^) ++ ", edges " ++ printEdgeRefList(h.edges)
}


let floatingEdge = (id,label,latex) => ref({id, sources:[||], targets:[||], label, latex})
let initialisePorts = (x) => Array.init(x, (i => (floatingEdge(0,"",""), i)))

let zeroNet = {inputs:floatingEdge(0, alpha, alphaLatex), edges:[], outputs:floatingEdge(1,omega,omegaLatex)}
let identity = (array) => array

let rec removeEdge = ({inputs,edges,outputs},edge) => {inputs,edges:removeEdge'(edge, edges,[]),outputs}
and removeEdge' = (edge, edges, acc) => {
    switch(edges){
    | [] => acc
    | [x,...xs] => x^.id == edge^.id ? acc @ xs :
                                     removeEdge'(edge,xs,[x,...acc])
    }
}

let composeSequential = (f,g,i) => {

    assert(Array.length(f.outputs^.sources) == Array.length(g.inputs^.targets));

    let newInputs = {id: 0, sources: [||], targets: [||], label: alpha, latex: alphaLatex};
    let newOutputs = {id: i+1, sources: [||], targets: [||], label: omega, latex: omegaLatex};

    for (i in 0 to Array.length(f.outputs^.sources) - 1){
        let (e,k) = f.outputs^.sources[i];
        let (e',k') = g.inputs^.targets[i];
        
        e^.targets[k] = (e',k');
        e'^.sources[k'] = (e,k);

    };

    {inputs: f.inputs, edges: f.edges @ g.edges, outputs: g.outputs}

}

let composeParallel = (f,g,i) => {

    let newInputs = {id: 0, sources: [||], targets: Array.append(f.inputs^.targets, g.inputs^.targets), label: alpha, latex: alphaLatex};
    let newOutputs = {id: i+1, sources: Array.append(f.outputs^.sources, g.outputs^.sources), targets: [||], label: omega, latex: omegaLatex};

    let finputs = Array.length(f.inputs^.targets);
    let foutputs = Array.length(f.outputs^.sources);

    let refInputs = ref(newInputs);
    let refOutputs = ref(newOutputs);

    for(i in 0 to Array.length(f.inputs^.targets) - 1){
        let (e,k) = f.inputs^.targets[i];
        
        e^.label == omega ? 
            refOutputs^.sources[k] = (refInputs, i) :
            e^.sources[k] = (refInputs, i)
    };

    for(i in 0 to Array.length(g.inputs^.targets) - 1){
        let (e,k) = g.inputs^.targets[i];
        
        e^.label == omega ? 
            refOutputs^.sources[k+foutputs] = (refInputs, i + finputs) : 
            e^.sources[k] = (refInputs, i + finputs)
    };

    for(i in 0 to Array.length(f.outputs^.sources) - 1){
        let (e,k) = f.outputs^.sources[i];
        
        e^.label == alpha ? 
            refInputs^.targets[k] = (refOutputs, i) :
            e^.targets[k] = (refOutputs, i)
    };

    for(i in 0 to Array.length(g.outputs^.sources) - 1){
        let (e,k) = g.outputs^.sources[i];
        
        e^.label == alpha ? 
            refInputs^.targets[k+finputs] = (refOutputs, i + foutputs) : 
            e^.targets[k] = (refOutputs, i + foutputs)
    };


    {inputs: refInputs, edges: f.edges @ g.edges, outputs: refOutputs}

}

let functionNet = (label, latex, ins, outs, i) => {
    let ine = floatingEdge(i,"","");
    let oute = floatingEdge(i+2,"","");
    let fune = ref({id:i+1, sources:Array.init(ins, (n) => (ine, n)), targets:Array.init(outs, (n) => (oute, n)), label, latex});
    ine := {id:0, sources:[||], targets:Array.init(ins, (n) => (fune, n)), label:alpha, latex: alphaLatex};
    oute := {id:i+2, sources:Array.init(outs, (n) => (fune, n)), targets:[||], label:omega, latex: omegaLatex};
    ({inputs: ine, edges:[fune], outputs: oute}, i+3)
}

let traceHypernet = (x, h) => {

    for(i in 0 to x-1){

        let (e, k) = h.inputs^.targets[i];
        let (e', k') = h.outputs^.sources[i];

        e'^.targets[k'] = (e, k);
        e^.sources[k] = (e',k');
    };


    let newInputs = ref({id: 0, sources: [||], targets:Array.sub(h.inputs^.targets, x, (Array.length(h.inputs^.targets) - x)), label:alpha, latex:alphaLatex});
    let newOutputs = ref({id: h.outputs^.id, sources: Array.sub(h.outputs^.sources, x, (Array.length(h.outputs^.sources) - x)), targets: [||], label:omega, latex: omegaLatex});

    for(i in 0 to Array.length(newInputs^.targets) - 1){
        let (e, k) = newInputs^.targets[i];
        
        if(e^.id == h.outputs^.id) {
            newInputs^.targets[i] = (newOutputs, k - x);
            newOutputs^.sources[k - x] = (newInputs, i);
        } else {
            newInputs^.targets[i] = (e,k);
            let (e',k') = e^.sources[k];
            e^.sources[k] = (e',k'-x);
        }
    };

    for(i in 0 to Array.length(newOutputs^.sources) - 1){
        let (e, k) = newOutputs^.sources[i];

        if(e^.id == h.inputs^.id) {
            newOutputs^.sources[i] = (newInputs, k - x);
            newInputs^.targets[k - x] = (newOutputs, i);
        } else {
            newOutputs^.sources[i] = (e,k);
            let (e',k') = e^.targets[k];
            e^.targets[k] = (e',k'-x);
        }
    };

    for(i in 0 to Array.length(newOutputs^.sources) - 1){
        let (e, k) = newOutputs^.sources[i];

    };

    {inputs: newInputs, edges: h.edges, outputs: newOutputs}
    
}

let swapNet = (i, x, y) => {
    let ine = {id:0, sources:[||], targets:initialisePorts(x+y), label:alpha, latex: alphaLatex};
    let oute = {id:i+1, sources:initialisePorts(x+y), targets:[||], label:omega, latex: omegaLatex};

    let refin = ref(ine);
    let refout = ref(oute);

    for(i in 0 to x - 1){
        refin^.targets[i] = (refout, y+i);
        refout^.sources[y+i] = (refin, i);
    };

    for(i in x to x + y - 1){
        refin^.targets[i] = (refout, i-x);
        refout^.sources[i-x] = (refin, i);
    };

    {inputs: refin, edges: [], outputs: refout}

}

let delayNet = (i, n) => {
    let subscript = generateUnicodeSubscript(n);
    functionNet({js|ẟ|js} ++ subscript, "\\delta_{" ++ subscript ++ "}", 1, 1, i)
}

let forkNet = (i) => {
    functionNet({js|⋏|js}, forkLatex, 1, 2, i)
}

let dforkNet = (i, n) => {
    let subscript = generateUnicodeSubscript(n);
    functionNet({js|Δ|js} ++ subscript, dforkLatex ++ "_{" ++ subscript ++ "}", n, n*2, i)
}

let iterHypernet = (circuit, i) => {

    let n = Array.length(circuit.outputs^.sources);
    let nfork = dforkNet(i, n)
    let newCircuit = composeSequential(circuit, fst(nfork), snd(nfork));
    (traceHypernet(n, newCircuit), snd(nfork) + 2);    

}

let joinLinks = (net, l, outlink, inlink) => {

    let outedge = List.hd(findEdges(lookupLink(outlink, l), net));
    let inedge = List.hd(findEdges(lookupLink(inlink, l), net));
    
    let (e,k) = outedge^.sources[0];
    let (e',k') = inedge^.targets[0];
    
    e^.targets[k] = (e', k');
    e'^.sources[k'] = (e, k);

    let newNet = removeEdge(net, outedge)
    let newerNet = removeEdge(newNet,inedge)

    newerNet;

}

let rec convertCircuitToHypernet = (circuit) => fst(convertCircuitToHypernet'(circuit, 0))
and convertCircuitToHypernet' = (circuit, i) => {
    switch(circuit.c){
    | Value(x)                  => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:circuit.v.print(x), latex:circuit.v.printLatex(x)})
                                   and ine = floatingEdge(i,alpha,alphaLatex)
                                   and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:omega, latex:omegaLatex});
                                   ({inputs: ine, edges: [e], outputs: oute}, i+3)
    | Identity(n)               => let ine = floatingEdge(i,"","");
                                   let oute = ref({id:i+1, sources:Array.init(n, (n) => (ine, n)), targets:[||], label:omega, latex:omegaLatex});
                                   ine := {id:0, sources:[||], targets:Array.init(n, (n) => (oute, n)), label:alpha, latex:alphaLatex};
                                   ({inputs:ine, edges: [], outputs: oute},i+2)
    | Composition(f,g)          => let fh = convertCircuitToHypernet'(f,i);
                                   let gh = convertCircuitToHypernet'(g,snd(fh));
                                   (composeSequential(fst(fh),fst(gh),snd(gh)), snd(gh) + 2)
    | Tensor([x,...xs])         => List.fold_left((f,g) => { let gh = convertCircuitToHypernet'(g,snd(f));
                                                 (composeParallel(fst(f),fst(gh),snd(gh)), snd(gh) + 2);}, 
                                                 (convertCircuitToHypernet'(x, i)), xs)
    | Swap(x,y)                 => (swapNet(i,x,y), i+2)
    | Function(id,latex,ins,outs,_) => functionNet(id, latex, ins, outs, i)
    | Delay(x)                  => delayNet(i,x)
    | Trace(x, f)               => let (fh,i') = convertCircuitToHypernet'(f,i+1);
                                   (traceHypernet(x,fh), i')
    | Iter(_, f)                => let f' = compose(f, dfork(circuit.v, outputs(f)));
                                   let (fh,i') = convertCircuitToHypernet'(f',i+1);
                                   (traceHypernet(outputs(f), fh), i');  
    | Macro(_,_,f)              => convertCircuitToHypernet'(f,i);
    | Inlink(x)                 => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:lookupLink(x,circuit.l), latex:lookupLink(x,circuit.l) })
                                   and ine = floatingEdge(i,alpha,alphaLatex) 
                                   and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:omega, latex:omegaLatex});
                                   ({inputs: ine, edges: [e], outputs: oute}, i+3)
    | Outlink(x)                => let rec e = ref({id: i+1, sources:[|(ine,0)|], targets:[||], label:lookupLink(x,circuit.l), latex:lookupLink(x,circuit.l)})
                                   and ine = ref({id: i, sources:[||], targets:[|(e,0)|], label:alpha, latex:alphaLatex})
                                   and oute = floatingEdge(i+2,omega,omegaLatex); 
                                   ({inputs: ine, edges: [e], outputs: oute}, i+3)
    | Link(x,y,f)               => let f = convertCircuitToHypernet'(f,i);
                                   (joinLinks(fst(f),circuit.l,x,y), snd(f))
    | _                         => failwith("badly formed circuit");
                             
                                    
    }
}

let rec reachable = (input, output, edge) => reachable'(input, output, edge, [])
and reachable' = (input, output, edge, traversed) => {
    if(edge^.id == output.id) {
        true;
    } else if (List.mem(edge^.id, traversed)) {
        false;
    } else {

        let result = ref(false);

        for(i in 0 to Array.length(edge^.targets) - 1){
            result := (result^ || reachable'(input, output, fst(edge^.targets[i]), traversed @ [edge^.id]))
        }

        result^;
    }
}

let stubEdge = (i, e, k) => ref({id: i, sources: [|(e,k)|], targets: [||], label: "~", latex: stubLatex});

/* TRACE BREAKS THIS (obviously) */

let rec minimise = (net) => { 
    let newStubs = minimise'(net.inputs, net.outputs, net.inputs, []);
    let edges = List.filter(((e) => reachable(net.inputs^, net.outputs^, e)), net.edges) @ newStubs;
    {inputs: net.inputs, edges: edges, outputs: net.outputs}
} and minimise' = (input, output, edge, traversed) => {

    let newStubs = ref([]);

    if(edge^.id == output^.id){
        ();
    } else if (List.mem(edge^.id, traversed)){
        ();
    } else {

        for(i in 0 to Array.length(edge^.targets) - 1) {
            let (e,k) = edge^.targets[i];

            if(!reachable(input^,output^,e)){
                let newStubEdge = stubEdge(fst(edge^.targets[i])^.id, edge, i);
                edge^.targets[i] = (newStubEdge, 0);
                newStubs := newStubs^ @ [newStubEdge];
            } else {
                newStubs := newStubs^ @ minimise'(input, output, e, traversed @ [edge^.id]);
            };
        };
    }

    newStubs^

}