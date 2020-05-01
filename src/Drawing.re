open Circuits;
open Hypernets;
open Algebraic;

let dot = {js|•|js}
let arrow = {js|→|js}

let tab = "    "
let nl = "\n"

let graphOptions = nl ++ tab ++ "rankdir=LR;" ++ nl ++ tab ++ "ranksep=0.5;" ++ nl ++ tab ++ "nodesep=0.25;" ++ nl;
let formalGraphOptions = tab ++ "rankdir=LR;" ++ nl ++ tab ++ "ranksep=0.75;" ++ nl ++ tab ++ "nodesep=0.25;" ++ nl;
let vertexOptions = "[style=filled, shape=circle, fillcolor=black; fixedsize=true; width=0.1; label=\"\"];"
let outputWireOptions = "[arrowhead=vee; arrowsize=0.5]"
let inputWireOptions = "[arrowhead=none; arrowsize=0.5]"
let invisibleWireOptions = "[style=invis]"
/*let traceWireOptions = "[arrowhead=none; arrowsize=0.5; constraint=false]"*/
let traceVertexOptions = "[shape=circle, fillcolor=black; fixedsize=true; width=0.05; label=\"\"]"

let formalInputWireOptions = (iv, ov) => "[arrowhead=vee; arrowsize=0.5; headlabel=\"" ++ string_of_int(iv) ++ " " ++ arrow ++ " " ++ string_of_int(ov) ++ "\"; labeldistance=3; labelangle=180]";
let formalOutputWireOptions = (ov) => "[arrowhead=vee; arrowsize=0.5; taillabel=\"" ++ string_of_int(ov) ++ "\"; labeldistance=2; labelangle=180]";

let getTraceText = (x, e, left) => {
    let dir = left ? "l" : "r";
    let name = "trace" ++ dir ++ string_of_int(x) ++ "to" ++ string_of_int(e^.id);
    (name, name ++ "[shape=point, width=0.01]\n");
}

let rec makeTransitionToAllEdgesWithNoSources = (inid, edges) => {
    switch(edges){
    | [] => ""
    | [x,...xs] => let result = (Array.length(x.sources) == 0) ? tab ++ "e" ++ string_of_int(inid) ++ "->" ++ "e" ++ string_of_int(x.id) ++ " " ++ invisibleWireOptions ++ nl : "" ;
                    result ++ makeTransitionToAllEdgesWithNoSources(inid, xs);
    }
}

let rec makeTransitionToAllEdgesWithNoTargets = (outid, edges) => {
    switch(edges){
    | [] => ""
    | [x,...xs] => let result = (Array.length(x.targets) == 0) ? tab ++ "e" ++ string_of_int(x.id) ++ "->" ++ "e" ++ string_of_int(outid) ++ " " ++ invisibleWireOptions ++ nl : "" ;
                    result ++ makeTransitionToAllEdgesWithNoTargets(outid, xs);
    }
}

let rec generateGraphvizCode = (net) => {
    
    let derefedEdges = List.map((x) => x^, net.edges) 
    let allEdges = [net.inputs^] @ derefedEdges @ [net.outputs^]
    let graph = generateGraphvizCodeEdges(net.inputs, net.outputs, allEdges, "", "", "", "", "", "", "");

    let emptyGraph = List.length(net.edges) == 0;

    let inputWires = !emptyGraph ? ((Array.length(net.inputs^.targets) == 0) ? nl ++ makeTransitionToAllEdgesWithNoSources(net.inputs^.id, derefedEdges) : "") : "";
    let outputWires = !emptyGraph ? ((Array.length(net.outputs^.sources) == 0) ? nl ++ makeTransitionToAllEdgesWithNoTargets(net.outputs^.id, derefedEdges) : "") : "";

    let inputOutputWires = (List.length(net.edges) == 0) ? nl ++ tab ++ "e" ++ string_of_int(net.inputs^.id) ++ "->" ++ "e" ++ string_of_int(net.outputs^.id) ++ invisibleWireOptions ++ nl : "";

    let finalGraph = "digraph {" ++ nl ++ nl ++ graphOptions ++ graph ++ inputWires ++ outputWires ++ inputOutputWires ++ nl ++ "}"

    Js.log(finalGraph);

    finalGraph

} and generateGraphvizCodeEdges = (inputs, outputs, es, ranks, edges, traces, vertices, inputWires, outputWires, traceWires) => {
    let inid = inputs^.id;
    let outid = outputs^.id;
    switch(es){
    | [] => let finalString = ref("");      
    
            ranks == "" ? () : finalString := finalString^ ++ ranks;
            edges == "" ? () : finalString := finalString^ ++ nl ++ edges;
            vertices == "" ? () : finalString := finalString^ ++ nl ++ vertices;
            traces == "" ? () : finalString := finalString^ ++ nl ++ traces;
            inputWires == "" ? () : finalString := finalString^ ++ nl ++ inputWires;
            outputWires == "" ? () : finalString := finalString^ ++ nl ++ outputWires;
            traceWires == "" ? () : finalString := finalString^ ++ nl ++ traceWires;

            finalString^
    | [x,...xs] => let edgecode = generateGraphvizCodeEdge(x,inid,outid);
                    let edgedot = fst(edgecode) == "" ? "" : fst(edgecode) ++ ";\n";
                    let ranksdot = snd(edgecode)[0];
                    let tracedot = snd(edgecode)[1];
                    let vertexDot = snd(edgecode)[2];
                    let inputWireDot = snd(edgecode)[3];
                    let outputWireDot = snd(edgecode)[4];
                    let traceWireDot = snd(edgecode)[5];
                    generateGraphvizCodeEdges(inputs, outputs, xs, ranks ++ ranksdot, edges ++ edgedot, traces ++ tracedot, vertices ++ vertexDot, inputWires ++ inputWireDot, outputWires ++ outputWireDot, traceWires ++ traceWireDot)
    }
} and generateGraphvizCodeEdge = (edge, inid, outid) => {

    let ins = Array.length(edge.sources);
    let outs = Array.length(edge.targets);
    let inports = generatePorts(ins, false);
    let outports = generatePorts(outs, true);
    
    let transitionsdata = generateTransitions(edge.id, inid, outid, edge.targets);

    let instring = inports == "{}" ? "" : inports ++ " | ";
    let outstring = outports == "{}" ? "" : " | " ++ outports; 

    (tab ++ "e" ++ string_of_int(edge.id) ++ 
        " [shape=Mrecord; label=\"{" ++ 
        instring ++ edge.label ++ outstring
        ++ "}\"]", transitionsdata)  
} and generatePorts = (n, out) => {
    "{" ++ generatePorts'(0,n, out) ++ "}"
} and generatePorts' = (x,n,out) => {
    let y = out ? "t" : "s"
    switch(n){
    | 0 => ""
    | 1 => "<" ++ y ++ string_of_int(x) ++ "> " ++ {js|•|js}
    | n => "<" ++ y ++ string_of_int(x) ++ "> " ++ {js|•|js} ++ " | " ++ generatePorts'(x+1,n-1,out)
    }
} and generateTransitions = (x, inid, outid, targets) => {
    let vertexString = ref("");
    let inputWireString = ref("");
    let outputWireString = ref("");
    let traceVertexString = ref("");
    let traceWireString = ref("");
    let ranks = ref("");

    for(i in 0 to Array.length(targets) - 1){
        let (e,k) = targets[i];
        
        let vertexId = "v" ++ string_of_int(x) ++ "_t" ++ string_of_int(i) ++ "_e" ++ string_of_int(e^.id) ++ "_s" ++ string_of_int(k);
        vertexString := vertexString^ ++ tab ++ vertexId ++ vertexOptions ++ nl;

        if(e^.id <= x){

            Js.log("Trace!");

            let traceVertexInId = vertexId ++ "_Tr_i";
            let traceVertexOutId = vertexId ++ "_Tr_o";
            let inport = (e^.id == x) ? ":n" : ""; 
            
            inputWireString := inputWireString^ ++ tab ++ vertexId ++ " -> e" ++ string_of_int(x) ++ ":t" ++ string_of_int(i) ++ ":e " ++ inputWireOptions ++ nl;
            outputWireString := outputWireString^ ++ tab ++ vertexId ++ " -> e" ++ string_of_int(e^.id) ++ ":s" ++ string_of_int(k) ++ ":w " ++ outputWireOptions ++ nl;


        } else {

            inputWireString := inputWireString^ ++ tab ++ "e" ++ string_of_int(x) ++ ":t" ++ string_of_int(i) ++ ":e -> " ++ vertexId ++ ":w " ++ inputWireOptions ++ nl
            outputWireString := outputWireString^ ++ tab ++ vertexId ++ ":e -> " ++ "e" ++ string_of_int(e^.id) ++ ":s" ++ string_of_int(k) ++ ":w " ++ outputWireOptions ++ nl
        }
        
    };

    [|ranks^, traceVertexString^, vertexString^, inputWireString^, outputWireString^, traceWireString^|];
} and generateOutputTransitions = (outid, edges) => {
    switch(edges){
    | [] => ""
    | [x,...xs] => "e" ++ string_of_int(x.id) ++ " -> e" ++ string_of_int(outid) ++ invisibleWireOptions ++ nl; 
    }
}

let zeroDot = generateGraphvizCode(zeroNet);

let generateFormalGraphvizVertices = (e, k, s, t) => {

    let inputVertexString = ref("");
    let outputVertexString = ref("");

    let inputWireString = ref("");
    let outputWireString = ref("");

    for(i in 0 to Array.length(t) - 1){

        let vertexId = "v" ++ string_of_int(t[i]) ++ "_I";

        let newInVertexString = vertexId ++ vertexOptions;
        inputVertexString := inputVertexString^ ++ nl ++ tab ++ newInVertexString;

        let newInWireString = "e" ++ string_of_int(e) ++ ":t" ++ string_of_int(i) ++ ":e -> " ++ vertexId ++ formalInputWireOptions(t[i], (k[t[i]])) ++ ";";
        inputWireString := inputWireString^ ++ nl ++ tab ++ newInWireString;
    };

    for(i in 0 to Array.length(s) - 1){

        let vertexId = "v" ++ string_of_int(s[i]) ++ "_O"

        let newOutVertexString =  vertexId ++ vertexOptions;
        outputVertexString := outputVertexString^ ++ nl ++ tab ++ newOutVertexString;

        let newOutWireString = vertexId ++ ":e -> " ++ "e" ++ string_of_int(e) ++ ":s" ++ string_of_int(i) ++ ":w " ++ formalOutputWireOptions(s[i]) ++ ";";
        outputWireString := outputWireString^ ++ nl ++ tab ++ newOutWireString;
    };

    (inputVertexString^, outputVertexString^, inputWireString^, outputWireString^)

}

let generateFormalGraphvizEdge = (i,k,s,t,l) => {
    
        let ins = Array.length(s);
        let outs = Array.length(t);

        let inports = generatePorts(ins, false);
        let outports = generatePorts(outs, true);
        
        let instring = inports == "{}" ? "" : inports ++ " | ";
        let outstring = outports == "{}" ? "" : " | " ++ outports;

        let newEdgeString = "e" ++ string_of_int(i) ++ "[shape=Mrecord; label=\"{" ++
        instring ++ l ++ outstring
        ++ "}\"];"

        let (inputVertexString, outputVertexString, inputWireString, outputWireString) = generateFormalGraphvizVertices(i, k, s, t);

        (newEdgeString, inputVertexString, outputVertexString, inputWireString, outputWireString)
}

let generateFormalGraphvizCode = ({v,e,i,o,k,lu,ll,fu,fl,s,t}) => {

    let edgeString = ref("");
    let inputVertexString = ref("");
    let outputVertexString = ref("");
    let inputWireString = ref("");
    let outputWireString = ref("");

    let normalSources = snd(s);
    let normalTargets = snd(t);

    for(j in 0 to (e - 1)) {
        let (newEdgeString, newInputVertexString, newOutputVertexString, newInputWireString, newOutputWireString) = generateFormalGraphvizEdge(j, k, normalSources[j], normalTargets[j], fu[j]);
        edgeString := tab ++ newEdgeString ++ nl ++ edgeString^;
        inputVertexString := tab ++ newInputVertexString ++ inputVertexString^;
        outputVertexString := tab ++ newOutputVertexString ++ outputVertexString^;
        inputWireString := tab ++ newInputWireString ++ inputWireString^;
        outputWireString := tab ++ newOutputWireString ++ outputWireString^;
    };

    let (inputEdgeString, inputInputString, _, inputInWireString, _) = generateFormalGraphvizEdge(e, k, [||], fst(t), alpha);
    let (outputEdgeString, _, outputOutputString, _, outputOutWireString) = generateFormalGraphvizEdge(e+1, k, fst(s), [||], omega);

    edgeString := tab ++ outputEdgeString ++ nl ++ edgeString^ ++ tab ++ inputEdgeString;
    inputVertexString := inputVertexString^ ++ tab ++ inputInputString;
    outputVertexString := tab ++ outputOutputString ++ outputVertexString^;
    inputWireString := inputWireString^ ++ inputInWireString;
    outputWireString := outputOutWireString ++ outputWireString^;

    let supportString = tab ++ "support [style=invis];" ++ nl ++ tab ++ "support -> e" ++ string_of_int(e) ++ " [style=invis];" ++ nl;

    let graphString = "digraph{" ++ nl ++ nl ++ formalGraphOptions ++ nl ++ edgeString^ ++ nl ++ inputVertexString^ ++ nl ++ outputVertexString^ ++ nl ++ inputWireString^ ++ nl ++ outputWireString^ ++ nl ++ nl ++ supportString ++ nl ++ "}";
    
    Js.log(graphString);
    graphString

}