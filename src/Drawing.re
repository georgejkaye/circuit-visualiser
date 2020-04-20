open Circuits;
open Hypernets;

let dot = {js|•|js}

let tab = "    "
let nl = "\n"

let graphOptions = tab ++ "rankdir=LR;" ++ nl ++ tab ++ "ranksep=0.5;" ++ nl;
let vertexOptions = "[style=filled, shape=circle, fillcolor=black; fixedsize=true; width=0.1; label=\"\"];"
let outputWireOptions = "[arrowhead=vee; arrowsize=0.5]"
let inputWireOptions = "[arrowhead=none; arrowsize=0.5]"
let invisibleWireOptions = "[style=invis]"
let traceWireOptions = "[arrowhead=none; arrowsize=0.5; constraint=false; weight=1000]"
let traceVertexOptions = "[shape=circle, fillcolor=black; fixedsize=true; width=0.05; label=\"\"]"

let getTraceText = (x, e, left) => {
    let dir = left ? "l" : "r";
    let name = "trace" ++ dir ++ string_of_int(x) ++ "to" ++ string_of_int(e^.id);
    (name, name ++ "[shape=point, width=0.01]\n");
}

/*  let (inedgedot, intransdot) = 
                if(Array.length(inputs^.targets) == 0){
                    ("","")
                } else {
                    let inedgecode = generateGraphvizCodeEdge(inputs,inid,outid);
                    let edgedot = fst(inedgecode) == "" ? "" : fst(inedgecode) ++ ";\n";
                    let transdot = snd(inedgecode)[2];
                    (edgedot, transdot)
                };
            
            let (outedgedot, outtransdot) = 
                if(Array.length(outputs^.sources) == 0){
                    ("","")
                } else {
                    let outedgecode = generateGraphvizCodeEdge(outputs,inid,outid);
                    let edgedot = fst(outedgecode) == "" ? "" : fst(outedgecode) ++ ";\n";
                    let transdot = snd(outedgecode)[2];
                    (edgedot, transdot)
                }; */

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

    let finalGraph = "digraph{" ++ nl ++ nl ++ graphOptions ++ graph ++ inputWires ++ outputWires ++ inputOutputWires ++ nl ++ "}"

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
        
        let vertexId = "v" ++ string_of_int(x) ++ "_t" ++ string_of_int(i) ++ "_e" ++ string_of_int(e^.id) ++ "_s" ++ string_of_int(k)
        vertexString := vertexString^ ++ tab ++ vertexId ++ vertexOptions ++ nl;

        if(e^.id <= x){

            Js.log("Trace!");

            let traceVertexInId = vertexId ++ "_Tr_i";
            let traceVertexOutId = vertexId ++ "_Tr_o";

            traceVertexString := traceVertexString^ ++ tab ++ traceVertexInId ++ traceVertexOptions ++ nl;
            traceVertexString := traceVertexString^ ++ tab ++ traceVertexOutId ++ traceVertexOptions ++ nl;
            
            inputWireString := inputWireString^ ++ tab ++ "e" ++ string_of_int(x) ++ ":t" ++ string_of_int(i) ++ ":e -> " ++ traceVertexInId ++ ":n " ++ inputWireOptions ++ nl;
            traceWireString := traceWireString^ ++ tab ++ traceVertexInId ++ ":s -> " ++ vertexId ++ ":e " ++ traceWireOptions ++ nl;
            traceWireString := traceWireString^ ++ tab ++ vertexId ++ ":w -> " ++ traceVertexOutId ++ ":s " ++ traceWireOptions ++ nl;
            outputWireString := outputWireString^ ++ tab ++ traceVertexOutId ++ ":n -> " ++ "e" ++ string_of_int(e^.id) ++ ":s" ++ string_of_int(k) ++ ":w " ++ outputWireOptions ++ nl


        } else {

            inputWireString := inputWireString^ ++ tab ++ "e" ++ string_of_int(x) ++ ":t" ++ string_of_int(i) ++ ":e -> " ++ vertexId ++ ":w " ++ inputWireOptions ++ nl
            outputWireString := outputWireString^ ++ tab ++ vertexId ++ ":e -> " ++ "e" ++ string_of_int(e^.id) ++ ":s" ++ string_of_int(k) ++ ":w " ++ outputWireOptions ++ nl
        }
        
    };

    [|ranks^, traceVertexString^, vertexString^, inputWireString^, outputWireString^, traceWireString^|];
} and generateOutputTransitions = (outid, edges) => {
    switch(edges){
    | [] => ""
    | [x,...xs] => Js.log(x.id); "e" ++ string_of_int(x.id) ++ " -> e" ++ string_of_int(outid) ++ invisibleWireOptions ++ nl; 
    }
}

let zeroDot = generateGraphvizCode(zeroNet);