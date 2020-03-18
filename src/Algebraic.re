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
    "\\{" ++ String.sub(strn, 0, String.length(strn) - 1) ++ "\\}"
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

let rec printFunctionLatex = (func, x, prnt) => {

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
    Js.log("replaceEdgeMap");
    switch(func){
    | []             => List.rev(acc)
    | [(x,fx),...xs] => replaceEdgeMapFst' (valueMap, xs, [(List.assoc(x,valueMap), fx),...acc])
    }
}

let rec replaceEdgeMapSnd = (valueMap, func) => replaceEdgeMapSnd' (valueMap, func, []) 
and replaceEdgeMapSnd' = (valueMap, func, acc) => {
    Js.log("replaceEdgeMap");
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
                    Js.log("mapped " ++ string_of_int(e));
                    normaliseEdgeIds'(inid,outid,n,es, [(e,newEdge),...valueMap])

    }
}
 
let rec generateInputsAndOutputs = (i, o, edge) => {

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

    (is^, os^)

}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let (i,o,eds,is,os,k,l,f) = generateAlgebraicDefinition' ([ref(net.inputs)] @ net.edges @ [ref(net.outputs)]);
    let (is,os,f) = normaliseEdgeIds(net.inputs.id, net.outputs.id, eds, is, os, f);
    {v: i, e: List.length(eds), i:Array.of_list(List.map(snd,is)), o:Array.of_list(List.map(snd,os)), k:Array.of_list(k), l:l, f:Array.of_list(List.map(snd,f))}

} and generateAlgebraicDefinition' = (edges) => generateAlgebraicDefinition''(edges, 0, 0, [], [], [], [], [], [])
and generateAlgebraicDefinition'' = (edges, i, o, eds, is, os, k, l, f) => {
    switch(edges){
        | [] => (i, o, List.rev(eds), is, os, List.rev(k), List.rev(l), List.rev(f))
        | [e,...es] => let (is', os') = generateInputsAndOutputs(i,o,e);
                       let (l',f') = List.mem(e^.label, l) || e^.label == "in" || e^.label == "out" ? (l, f) : ([e^.label,...l], [(e^.id, e^.label),...f]);
                       generateAlgebraicDefinition'' (es, i + Array.length(e^.sources), o + Array.length(e^.targets), [e^.id,...eds], is @ is', os @ os', k, l',f')
    }



}