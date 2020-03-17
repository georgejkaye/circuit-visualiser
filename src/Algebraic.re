open Hypernets

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
    l : array(string),
    f : array(string)
}

let rec generateFin = (x, n) => {
    let strn = generateFin' (x, n, 0, "");
    "\\{" ++ String.sub(strn, 0, String.length(strn) - 1) ++ "\}"
} and generateFin' = (x, n, i, acc) => {
    n == 0 ? acc 
           : generateFin' (x, n-1, i+1, acc ++ x ++ "_{" ++ string_of_int(i) ++ "}" ++ ",")
}

let edgePlusTwoLatex = (e) => {
    switch(e){
        | Edge(j) => "e_{" ++ string_of_int(j) ++ "}"
        | Input   => "\alpha"
        | Output  => "\omega"
        }
}

let rec printFunctionLatex = (func, prnt) => {

    let string = ref("\{");

    for (i in 0 to Array.length(func) - 1){
        let img = prnt(func[i])
        string := string^ ++ "v_{" ++ string_of_int(i) ++ "} \mapsto " ++ img ++ ","
    };

    String.sub(string^, 0, String.length(string^) - 1) ++ "\}"
}

let algebraicNetLatex = ({v,e,i,o,k,l,f}) => {
    "V^I = V^O = " ++ generateFin("v", v) ++ "\\\\" ++
    "E = " ++ generateFin("e", e) ++ "\\\\" ++
    "i = " ++ printFunctionLatex(i, edgePlusTwoLatex) ++ "\\\\" ++
    "o = " ++ printFunctionLatex(o, edgePlusTwoLatex) ++ "\\\\" ++ ""
}

/* Get the number of edges in a hypernet */
let numberOfEdges = (net) => List.length (net.edges)

let rec replaceEdgeMap = (valueMap, func) => replaceEdgeMap' (valueMap, func, []) 
and replaceEdgeMap' = (valueMap, func, acc) => {
    Js.log("replaceEdgeMap");
    switch(func){
    | []             => List.rev(acc)
    | [(x,fx),...xs] => replaceEdgeMap' (valueMap, xs, [(x, List.assoc(x,valueMap)),...acc])
    }
}

let rec normaliseEdgeIds = (inid, outid, eds, is, os) => {
    let valueMap = normaliseEdgeIds'(inid,outid,0,eds,[]); 
    (replaceEdgeMap(valueMap,is), replaceEdgeMap(valueMap,os))
} and normaliseEdgeIds' = (inid,outid,n,eds,valueMap) => {
    switch(eds){
    | []        =>  valueMap
    | [e,...es] =>  let (newEdge, n) = (e == inid) ? (Input, n) 
                                                   : (e == outid) ? (Output, n) 
                                                                  : (Edge(n), n+1);
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
        is := [(x,id)]
    };

    for (j in 0 to Array.length(targets) - 1){
        let x = j + o
        os := [(x,id)]
    };

    (is^, os^)

}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let (i,o,eds,is,os,k,l,f) = generateAlgebraicDefinition' ([ref(net.inputs)] @ net.edges @ [ref(net.outputs)]);
    let (is,os) = normaliseEdgeIds(net.inputs.id, net.outputs.id, eds, is, os);
    {v: i, e: List.length(eds), i:Array.of_list(List.map(snd,is)), o:Array.of_list(List.map(snd,os)), k:Array.of_list(k), l:Array.of_list(l), f:Array.of_list(f)}

} and generateAlgebraicDefinition' = (edges) => generateAlgebraicDefinition''(edges, 0, 0, [], [], [], [], [], [])
and generateAlgebraicDefinition'' = (edges, i, o, eds, is, os, k, l, f) => {
    switch(edges){
        | [] => (i, o, List.rev(eds), is, os, List.rev(k), List.rev(l), List.rev(f))
        | [e,...es] => let (is', os') = generateInputsAndOutputs(i,o,e);
                       generateAlgebraicDefinition'' (es, i + Array.length(e^.sources), o + Array.length(e^.targets), [e^.id,...eds], is @ is', os @ os', k, l, f)
    }



}