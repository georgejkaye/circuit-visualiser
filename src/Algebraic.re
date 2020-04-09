open Hypernets
open Helpers

type edgePlusTwo = 
  | Edge(int)
  | Input
  | Output

type algebraicNet = {
    v : int,
    e : int,
    i : array(edgePlusTwo),
    o : array(edgePlusTwo),
    k : array(int),
    l : list(string),
    f : array(string)
}

let rec generateFin = (x, n) => {
    let strn = generateFin' (x, n, 0, "");
    (String.length(strn) > 0) ? "\\{" ++ String.sub(strn, 0, String.length(strn) - 1) ++ "\\}" : strn
} and generateFin' = (x, n, i, acc) => {
    n == 0 ? acc 
           : generateFin' (x, n-1, i+1, acc ++ x ++ "_{" ++ string_of_int(i) ++ "}" ++ ",")
}

let vertexLatex = (v) => "v_{" ++ string_of_int(v) ++ "}"

let edgePlusTwoLatex = (e) => {
    switch(e){
        | Edge(j) => "e_{" ++ string_of_int(j) ++ "}"
        | Input   => "\\alpha"
        | Output  => "\\omega"
        }
}

let printFunctionLatex = (func, x, prnt) => {

    let string = ref("\\{");

    for (i in 0 to Array.length(func) - 1){
        let img = prnt(func[i])
        string := string^ ++ x ++ "_{" ++ string_of_int(i) ++ "} \\mapsto " ++ img ++ ","
    };

    String.length(string^) == 2 ? "\\{\\}" : String.sub(string^, 0, String.length(string^) - 1) ++ "\\}"
}

let algebraicNetLatex = ({v,e,i,o,k,l,f}) => {
    "V^I = V^O = " ++ generateFin("v", v) ++ "\\\\" ++
    "E = " ++ generateFin("e", e) ++ "\\\\" ++
    "i = " ++ printFunctionLatex(i, "v", edgePlusTwoLatex) ++ "\\\\" ++
    "o = " ++ printFunctionLatex(o, "v", edgePlusTwoLatex) ++ "\\\\" ++
    "\\kappa = " ++ printFunctionLatex(k, "v", vertexLatex) ++ "\\\\ " ++
    "L = \\{" ++ printListCommas(l, (x) => x) ++ "\\}\\\\" ++
    "l = " ++ printFunctionLatex(f, "e", (x) => x)
}

/* Get the number of edges in a hypernet */
let numberOfEdges = (net) => List.length (net.edges)

let rec replaceEdgeMapFst = (valueMap, func) => replaceEdgeMapFst' (valueMap, func, []) 
and replaceEdgeMapFst' = (valueMap, func, acc) => {
    switch(func){
    | []             => List.rev(acc)
    | [(x,fx),...xs] => replaceEdgeMapFst' (valueMap, xs, [(List.assoc(x,valueMap), fx),...acc])
    }
}

let rec replaceEdgeMapSnd = (valueMap, func) => replaceEdgeMapSnd' (valueMap, func, []) 
and replaceEdgeMapSnd' = (valueMap, func, acc) => {
    switch(func){
    | []             => List.rev(acc)
    | [(x,fx),...xs] => replaceEdgeMapSnd' (valueMap, xs, [(x, List.assoc(fx,valueMap)),...acc])
    }
}

let rec normaliseEdgeIds = (inid, outid, eds, is, os,f) => {
    let valueMap = normaliseEdgeIds'(inid,outid,0,eds,[]); 
    (replaceEdgeMapSnd(valueMap,is), replaceEdgeMapSnd(valueMap,os), replaceEdgeMapFst(valueMap,f))
} and normaliseEdgeIds' = (inid,outid,n,eds,valueMap) => {
    switch(eds){
    | []        =>  valueMap
    | [e,...es] =>  let (newEdge, n) = (e == inid) ? (Input, n) 
                                                   : (e == outid) ? (Output, n) 
                                                                  : (Edge(n), n+1);
                    normaliseEdgeIds'(inid,outid,n,es, [(e,newEdge),...valueMap])

    }
}
 
let generateInputsAndOutputs = (i, o, edge) => {

    let sources = edge^.sources;
    let targets = edge^.targets;
    let id = edge^.id;

    let is = ref([]);
    let os = ref([]);

    for (j in 0 to Array.length(sources) - 1){
        let x = j + i
        is := [(x,id),...is^]
    };

    for (j in 0 to Array.length(targets) - 1){
        let x = j + o
        os := [(x,id),...os^]
    };

    (List.rev(is^), List.rev(os^))

}

let generateConnections = (edge, is, os) => {

    let targets = edge^.targets;
    let id = edge^.id;

    let ks = ref([]);
    let ous = List.filter((((_,e)) => e == id), os);
    Js.log("ous: " ++ printListCommas(ous, (((x,y)) => "(" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")")))


    for(j in 0 to Array.length(targets) - 1){
        
        let v1 = fst(List.nth(ous, j));
        let (e, k) = targets[j];

        let ins = List.filter((((_,e')) => e^.id == e'), is);
        Js.log("ins: " ++ printListCommas(ins, (((x,y)) => "(" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")")))
        let v2 = fst(List.nth(ins,k))
       
        ks := [(v1, v2),...ks^]

    };

    (List.rev(ks^))

}

let rec generateAllConnections = (os, is, es) => generateAllConnections'(os, is, es, [])
and generateAllConnections' = (os, is, es, acc) => {
    switch(es){
    | [] => acc
    | [e,...es] => let ks = generateConnections(e, is, os); 
                       generateAllConnections'(os, is, es, acc @ ks)
    }
}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let allEdges = [net.inputs] @ net.edges @ [net.outputs];
    let (i,eds,is,os,l,f) = generateAlgebraicDefinition' (allEdges);
    let ks = generateAllConnections(os,is,allEdges);
    let (is,os,f) = normaliseEdgeIds(net.inputs^.id, net.outputs^.id, eds, is, os, f);
    
    {v: i, 
     e: List.length(eds) - 2, 
     i:Array.of_list(List.map(snd,is)), 
     o:Array.of_list(List.map(snd,os)), 
     k:Array.of_list(List.map(snd,ks)), 
     l:l, 
     f:Array.of_list(List.map(snd,f))
    }

} and generateAlgebraicDefinition' = (edges) => generateAlgebraicDefinition''(edges, 0, 0, [], [], [], [], [])
and generateAlgebraicDefinition'' = (edges, i, o, eds, is, os, l, f) => {
    switch(edges){
        | [] => (i, List.rev(eds), is, os, List.rev(l), List.rev(f))
        | [e,...es] => let (is', os') = generateInputsAndOutputs(i,o,e);
                       let l' = List.mem(e^.label, l) || e^.label == "in" || e^.label == "out" ? l : [e^.label,...l];
                       let f' = e^.label == "in" || e^.label == "out" ? f : [(e^.id, e^.label),...f];
                       generateAlgebraicDefinition'' (es, i + Array.length(e^.sources), 
                                                      o + Array.length(e^.targets), 
                                                      [e^.id,...eds], 
                                                      is @ is', 
                                                      os @ os', 
                                                      l',
                                                      f'
                                                     )
    }



}