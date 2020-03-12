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

let numberOfEdges = (net) => List.length (net.edges)

let rec numberOfVertices = (net) => numberOfVertices' (net.edges, 0) + Array.length(net.inputs.targets)
and numberOfVertices' = (edges, acc) => {
    switch(edges){
    | [] => acc
    | [e,...es] => numberOfVertices' (es, acc + Array.length(e^.targets))
    }
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
    "hypernet inputs " ++ printEdge(h.inputs) ++ ", outputs " ++ printEdge(h.outputs) ++ ", edges " ++ printEdgeRefList(h.edges)
}


let floatingEdge = (id,label) => {id, sources:[||], targets:[||], label} 
let initialisePorts = (x) => Array.init(x, (i => (ref(floatingEdge(0,"")), i)))

let zeroNet = {inputs:floatingEdge(0, "in"), edges:[], outputs:floatingEdge(1,"out")}
let identity = (array) => array

let rec removeEdge = ({inputs,edges,outputs},edge) => {inputs,edges:removeEdge'(edge, edges,[]),outputs}
and removeEdge' = (edge, edges, acc) => {
    switch(edges){
    | [] => acc
    | [x,...xs] => x^.id == edge^.id ? acc @ xs :
                                     removeEdge'(edge,xs,[x,...acc])
    }
}

let composeSequential = (f,g) => {

    assert(Array.length(f.outputs.sources) == Array.length(g.inputs.targets));

    for (i in 0 to Array.length(f.outputs.sources) - 1){
        let (e,k) = f.outputs.sources[i];
        let (e',k') = g.inputs.targets[i];
    
        e^.targets[k] = (e',k');
        e'^.sources[k'] = (e,k);

    };

    {inputs: f.inputs, edges: f.edges @ g.edges, outputs: g.outputs}

}

let composeParallel = (f,g,i) => {

    let newInputs = {id: 0, sources: [||], targets: Array.append(f.inputs.targets, g.inputs.targets), label: "in"};
    let newOutputs = {id: i+1, sources: Array.append(f.outputs.sources, g.outputs.sources), targets: [||], label: "out"};

    let finputs = Array.length(f.inputs.targets);
    let foutputs = Array.length(f.outputs.sources);

    let refInputs = ref(newInputs);
    let refOutputs = ref(newOutputs);

    for(i in 0 to Array.length(f.inputs.targets) - 1){
        let (e,k) = f.inputs.targets[i];
        
        e^.label == "out" ? 
            refOutputs^.sources[k] = (refInputs, i) :
            e^.sources[k] = (refInputs, i)
    };

    for(i in 0 to Array.length(g.inputs.targets) - 1){
        let (e,k) = g.inputs.targets[i];
        
        e^.label == "out" ? 
            refOutputs^.sources[k+foutputs] = (refInputs, i + finputs) : 
            e^.sources[k] = (refInputs, i + finputs)
    };

    for(i in 0 to Array.length(f.outputs.sources) - 1){
        let (e,k) = f.outputs.sources[i];
        
        e^.label == "in" ? 
            refInputs^.targets[k] = (refOutputs, i) :
            e^.targets[k] = (refOutputs, i)
    };

    for(i in 0 to Array.length(g.outputs.sources) - 1){
        let (e,k) = g.outputs.sources[i];
        
        e^.label == "in" ? 
            refInputs^.targets[k+finputs] = (refOutputs, i + foutputs) : 
            e^.targets[k] = (refOutputs, i + foutputs)
    };


    {inputs: refInputs^, edges: f.edges @ g.edges, outputs: refOutputs^}

}

let functionNet = (id, ins, outs, i) => {
    let ine = ref(floatingEdge(i,""));
    let oute = ref(floatingEdge(i+2,""));
    let fune = ref({id:i+1, sources:Array.init(ins, (n) => (ine, n)), targets:Array.init(outs, (n) => (oute, n)), label:id});
    ine := {id:0, sources:[||], targets:Array.init(ins, (n) => (fune, n)), label:"in"};
    oute := {id:i+2, sources:Array.init(outs, (n) => (fune, n)), targets:[||], label:"out"};
    ({inputs: ine^, edges:[fune], outputs: oute^}, i+3)
}

let traceHypernet = (x, h) => {

    for(i in 0 to x-1){

        let (e, k) = h.inputs.targets[i];
        let (e', k') = h.outputs.sources[i];

        e'^.targets[k'] = (e, k);
        e^.sources[k] = (e',k');
    };

    let newInputs = {id: 0, sources: [||], targets:Array.sub(h.inputs.targets, x, (Array.length(h.inputs.targets) - x)), label:"in"};
    let newOutputs = {id: h.outputs.id, sources: Array.sub(h.outputs.sources, x, (Array.length(h.outputs.sources) - x)), targets: [||], label:"out"};

    for(i in 0 to Array.length(newInputs.targets) - 1){
        let (e, k) = newInputs.targets[i];
        let k = (e^.label == "out") ? k - x : k;
        newInputs.targets[i] = (e,k)
    };

    for(i in 0 to Array.length(newOutputs.targets) - 1){
        let (e, k) = newInputs.sources[i];
        let k = (e^.label == "in") ? k - x : k;
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

let swapNet = (i, x, y) => {
    let ine = {id:0, sources:[||], targets:initialisePorts(x+y), label:"in"};
    let oute = {id:i+1, sources:initialisePorts(x+y), targets:[||], label:"out"};

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

    {inputs: refin^, edges: [], outputs: refout^}

}

let delayNet = (i, n) => {
    let subscript = generateUnicodeSubscript(n);
    functionNet({js|ẟ|js} ++ subscript, 1, 1, i)
}

let forkNet = (i) => {
    functionNet({js|⋏|js}, 1, 2, i)
}

let dforkNet = (i, n) => {
    let subscript = generateUnicodeSubscript(n);
    functionNet({js|Δ|js} ++ subscript, n, n*2, i)
}

let iterHypernet = (circuit, i) => {

    let n = Array.length(circuit.outputs.sources);
    let nfork = dforkNet(i, n)
    let newCircuit = composeSequential(circuit, fst(nfork));
    (traceHypernet(n, newCircuit), snd(nfork));    

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
    Js.log(printCircuit(circuit));
    switch(circuit.c){
    | Value(x)                  => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:circuit.v.print(x)})
                                   and ine = ref(floatingEdge(i,"in")) 
                                   and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:"out"});
                                   ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Identity(n)               => let ine = ref(floatingEdge(i,""));
                                   let oute = ref({id:i+1, sources:Array.init(n, (n) => (ine, n)), targets:[||], label:"out"});
                                   ine := {id:0, sources:[||], targets:Array.init(n, (n) => (oute, n)), label:"in"};
                                   ({inputs:ine^, edges: [], outputs: oute^},i+2)
    | Composition(f,g)          => let fh = convertCircuitToHypernet'(f,i);
                                   let gh = convertCircuitToHypernet'(g,snd(fh));
                                   (composeSequential(fst(fh),fst(gh)), snd(gh))
    | Tensor([x,...xs])         => List.fold_left((f,g) => { let gh = convertCircuitToHypernet'(g,snd(f));
                                                 (composeParallel(fst(f),fst(gh),snd(gh)), snd(gh) + 2);}, 
                                                 (convertCircuitToHypernet'(x, i)), xs)
    | Swap(x,y)                 => (swapNet(i,x,y), i+2)
    | Function(id,_,ins,outs,_) => functionNet(id, ins, outs, i)
    | Delay(x)                  => delayNet(i,x)
    | Trace(x, f)               => let (fh,i') = convertCircuitToHypernet'(f,i+1);
                                   (traceHypernet(x,fh), i')
    | Iter(_, f)                => let f' = compose(f, dfork(circuit.v, outputs(f)));
                                   let (fh,i') = convertCircuitToHypernet'(f',i+1);
                                   (traceHypernet(outputs(f), fh), i');  
    | Macro(_,_,f)              => convertCircuitToHypernet'(f,i);
    | Inlink(x)                 => let rec e = ref({id: i+1, sources:[||], targets:[|(oute,0)|], label:lookupLink(x,circuit.l)})
                                   and ine = ref(floatingEdge(i,"in")) 
                                   and oute = ref({id: i+2, sources:[|(e,0)|], targets:[||], label:"out"});
                                   ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Outlink(x)                => let rec e = ref({id: i+1, sources:[|(ine,0)|], targets:[||], label:lookupLink(x,circuit.l)})
                                   and ine = ref({id: i, sources:[||], targets:[|(e,0)|], label:"in"})
                                   and oute = ref(floatingEdge(i+2,"out")); 
                                   ({inputs: ine^, edges: [e], outputs: oute^}, i+3)
    | Link(x,y,f)               => let f = convertCircuitToHypernet'(f,i);
                                   (joinLinks(fst(f),circuit.l,x,y), snd(f))
    | _                         => failwith("badly formed circuit");
                             
                                    
    }
}

let tab = "    "
let getTraceText = (x, e, left) => {
    let dir = left ? "l" : "r";
    let name = "trace" ++ dir ++ string_of_int(x) ++ "to" ++ string_of_int(e^.id);
    (name, name ++ "[shape=point, width=0.01]\n");
}

let rec generateGraphvizCode = (net) => {
    let graph = generateGraphvizCodeEdges(net.inputs, net.outputs, List.map((x) => x^, net.edges), "", "", "", "");
    "digraph{\n\n" ++ tab ++ "rankdir=LR;\n" ++ tab ++ "ranksep=1;" ++ graph ++ "}"
} and generateGraphvizCodeEdges = (inputs, outputs, edges, ranks, nodes, traces, transitions) => {
    let inid = inputs.id;
    let outid = outputs.id;
    switch(edges){
    | [] => let (inedgedot, intransdot) = 
                if(Array.length(inputs.targets) == 0){
                    ("","")
                } else {
                    let inedgecode = generateGraphvizCodeEdge(inputs,inid,outid);
                    let edgedot = fst(inedgecode) == "" ? "" : fst(inedgecode) ++ ";\n";
                    let transdot = snd(inedgecode)[2];
                    (edgedot, transdot)
                };
            
            let (outedgedot, outtransdot) = 
                if(Array.length(outputs.sources) == 0){
                    ("","")
                } else {
                    let outedgecode = generateGraphvizCodeEdge(outputs,inid,outid);
                    let edgedot = fst(outedgecode) == "" ? "" : fst(outedgecode) ++ ";\n";
                    let transdot = snd(outedgecode)[2];
                    (edgedot, transdot)
                };

            "\n" ++ ranks ++ "\n" ++ nodes ++ outedgedot ++ inedgedot ++ traces ++ "\n" ++ intransdot ++ transitions ++ outtransdot ++ "\n"
             
    | [x,...xs] => let edgecode = generateGraphvizCodeEdge(x,inid,outid);
                    let edgedot = fst(edgecode) == "" ? "" : fst(edgecode) ++ ";\n";
                    let ranksdot = snd(edgecode)[0];
                    let tracedot = snd(edgecode)[1];
                    let transdot = snd(edgecode)[2];
                    generateGraphvizCodeEdges(inputs, outputs, xs, ranks ++ ranksdot, nodes ++ edgedot, traces ++ tracedot, transitions ++ transdot)
    }
} and generateGraphvizCodeEdge = (edge, inid, outid) => {

    let ins = Array.length(edge.sources);
    let outs = Array.length(edge.targets);
    let inports = generatePorts(ins, false);
    let outports = generatePorts(outs, true);
    let transitionsdata = generateTransitions(edge.id, inid, outid, edge.targets);

    let instring = inports == "{}" ? "" : inports ++ " | ";
    let outstring = outports == "{}" ? "" : " | " ++ outports; 

    (tab ++ "edge" ++ string_of_int(edge.id) ++ 
        " [shape=Mrecord; label=\"{" ++ 
        instring ++ edge.label ++ outstring
        ++ "}\"]", transitionsdata)  
} and generatePorts = (n, out) => {
    "{" ++ generatePorts'(0,n, out) ++ "}"
} and generatePorts' = (x,n,out) => {
    let y = out ? "o" : "i"
    switch(n){
    | 0 => ""
    | 1 => "<" ++ y ++ string_of_int(x) ++ "> " ++ {js|•|js}
    | n => "<" ++ y ++ string_of_int(x) ++ "> " ++ {js|•|js} ++ " | " ++ generatePorts'(x+1,n-1,out)
    }
} and generateTransitions = (x, inid, outid, targets) => {
    let string = ref("");
    let tracenodes = ref("");
    let ranks = ref("");

    for(i in 0 to Array.length(targets) - 1){
        let (e,k) = targets[i];
        /* trace! */
        if(e^.id < x){

            Js.log("trace!");

            let (idl, tracel) = getTraceText(x,e,true);
            let (idr, tracer) = getTraceText(x,e,false);

            tracenodes := tracenodes^ ++ tab ++ tracel ++ tab ++ tracer

            string := string^ ++ tab ++ "edge" ++ string_of_int(x) ++ ":o" ++ string_of_int(i) ++ ":e -> " ++ idr ++ ":s;\n" ++
                                    tab ++ idr ++ ":n -> " ++ idl ++ ":n;\n" ++
                                    tab ++ idl ++ ":s -> edge" ++ string_of_int(e^.id) ++ ":i" ++ string_of_int(k) ++ ":w;\n";

            ranks := ranks^ ++ tab ++ "{rank=same; edge" ++ string_of_int(inid) ++ ", " ++ idl ++ "}\n" ++ tab ++ "{rank=same; edge" ++ string_of_int(outid) ++ ", " ++ idr ++ "}\n"

        } else {
            string := string^ ++ tab ++ "edge" ++ string_of_int(x) ++ ":o" ++ string_of_int(i) ++ ":e -> edge" ++ string_of_int(e^.id) ++ ":i" ++ string_of_int(k) ++ ":w;\n"
        }
        
    };

    [|ranks^, tracenodes^, string^|];
}

let zeroDot = generateGraphvizCode(zeroNet);
Js.log(zeroDot);

let rec scanList = (seen, id) => {
    switch(seen){
    | []        => (false,-1,-1)
    | [(x,i,ns),...xs] => (x == id) ? (true, i, ns) : scanList(xs, id)
    }
} 

let rec generateTensor = (v,es) => generateTensor'(v,es,[],[||],0)
and generateTensor' = (v,es,t,es_next,outs) => {
    switch(es){
    | [] => (List.rev(t), es_next,outs)
    | [(e,k),...xs] => let e' = e^; 
                       (e'.label == "out") ? 
                       generateTensor'(v, xs, [idcirc(v,1),...t], Array.append(es_next,[|(e,k)|]), outs + 1) : 
                       generateTensor'(v, xs, [funcBlackBox(v,e'.label,"\\text{" ++ e'.label ++ "}",Array.length(e'.sources),Array.length(e'.targets)),...t],Array.append(es_next,e'.targets), outs)
    }
}

let convertHypernetToEquation = (v,net) => {
    let cat = ref([]);
    let es_next = ref([||]);

    es_next := net.inputs.targets

    while(Array.length(es_next^) != 0){
        
        let es = es_next^;

        /*let (cat', es') = createSymmetry(es^)*/
        /*cat := List.rev_append(cat^,cat')*/
        
        let (t,es_next',outs) = generateTensor(v,Array.to_list(es));

        if(outs == Array.length(es_next')){
            es_next := [||]
        } else {
            es_next := es_next';
            cat := List.append(cat^, [tensor(t)]);
        }
    }

    Js.log(printCircuitListCommas(cat^));
    composemany(cat^);
}