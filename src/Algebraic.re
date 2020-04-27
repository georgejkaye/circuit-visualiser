open Hypernets
open Helpers

type edgePlusTwo = 
  | Edge(int)
  | Input
  | Output

let printEdgePlusTwo = (ept) => {
    switch(ept){
    | Edge(x) => string_of_int(x)
    | Input => "Input"
    | Output => "Output"
    }
}

type algebraicNet = {
    v : int,
    e : int,
    i : array(edgePlusTwo),
    o : array(edgePlusTwo),
    k : array(int),
    lu : list(string),
    ll : list(string),
    fu : array(string),
    fl : array(string),
    s : (list(int), array(list(int))),
    t : (list(int), array(list(int)))
}

let rec generateFin = (x, n) => {
    let strn = generateFin' (x, n, 0, "");
    (String.length(strn) > 0) ? "\\{" ++ String.sub(strn, 0, String.length(strn) - 1) ++ "\\}" : strn
} and generateFin' = (x, n, i, acc) => {
    n == 0 ? acc 
           : generateFin' (x, n-1, i+1, acc ++ x ++ "_{" ++ string_of_int(i) ++ "}" ++ ",")
}

let vertexLatex = (v) => "v_{" ++ string_of_int(v) ++ "}"
let edgeLatex = (e) => "e_{" ++ string_of_int(e) ++ "}"

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


let printFunctionFromEdgesToListsLatex = (func, bonus, front) => {

    let string = ref("");
    let bonusString = bonus ++ "\\mapsto [" ++ printListCommas(fst(func), vertexLatex) ++ "],"

    let func = snd(func);

    for(i in 0 to Array.length(func) - 1) {

        string := string^ ++ edgeLatex(i) ++ "\\mapsto [" ++ printListCommas(func[i], vertexLatex) ++ "],"

    }

    front ? string := bonusString ++ string^ : string := string^ ++ bonusString ;

    String.length(string^) == 2 ? "\\{\\}" : "\\{" ++ String.sub(string^, 0, String.length(string^) - 1) ++ "\\}"

}

let algebraicNetLatex = ({v,e,i,o,k,lu,ll,fu,fl,s,t}) => {
    let result = "V^I = V^O = " ++ generateFin("v", v) ++ "\\\\" ++
    "E = " ++ generateFin("e", e) ++ "\\\\" ++
    "i = " ++ printFunctionLatex(i, "v", edgePlusTwoLatex) ++ "\\\\" ++
    "o = " ++ printFunctionLatex(o, "v", edgePlusTwoLatex) ++ "\\\\" ++
    "\\kappa = " ++ printFunctionLatex(k, "v", vertexLatex) ++ "\\\\" ++
    "L = \\{" ++ printListCommas(ll, (x) => x) ++ "\\}\\\\" ++
    "l = " ++ printFunctionLatex(fl, "e", (x) => x) ++ "\\\\" ++
    "s = " ++ printFunctionFromEdgesToListsLatex(s, omegaLatex, false) ++ "\\\\" ++
    "t = " ++ printFunctionFromEdgesToListsLatex(t, alphaLatex, true)

    Js.log(result);
    result
}

/* Get the number of edges in a hypernet */
let numberOfEdges = (net) => List.length (net.edges)

let rec replaceEdgeMap = (valueMap, func) => replaceEdgeMap' (valueMap, func, [])
and replaceEdgeMap' = (valueMap, func, acc) => {
    switch(func){
        | []        => List.rev(acc)
        | [x,...xs] => replaceEdgeMap' (valueMap, xs, [List.assoc(x,valueMap),...acc])
        }
}

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

let rec normaliseEdgeIds = (inid, outid, eds, is, os,fu,fl) => {
    let valueMap = normaliseEdgeIds'(inid,outid,0,eds,[]); 
    (replaceEdgeMap(valueMap, eds), replaceEdgeMapSnd(valueMap,is), replaceEdgeMapSnd(valueMap,os), replaceEdgeMapFst(valueMap,fu), replaceEdgeMapFst(valueMap,fl))
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
        os := [(x,id),...os^]
    };

    for (j in 0 to Array.length(targets) - 1){
        let x = j + o
        is := [(x,id),...is^]
    };

    (List.rev(is^), List.rev(os^))

}

let generateConnections = (edge, is, os) => {

    let targets = edge^.targets;
    let id = edge^.id;

    let ks = ref([]);
    let ous = List.filter((((_,e)) => e == id), is);

    for(j in 0 to Array.length(targets) - 1){
        
        let v1 = fst(List.nth(ous, j));
        let (e, k) = targets[j];

        let ins = List.filter((((_,e')) => e^.id == e'), os);
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

let generateSourcesAndTargets = (eds, is, os) => {

    let sources = List.map((x) => List.filter(((((v,e)) => e == x
    )), os), eds);
    
    let targets = List.map((x) => List.filter(((((v,e)) => e == x
    )), is), eds);

    let sourceSplit = split(List.length(sources) - 1, sources);
    let targetSplit = split(1, targets);

    let sources = (List.map(fst,List.nth(snd(sourceSplit), 0)) , Array.of_list(List.map((xs) => List.map(fst,xs),trim(fst(sourceSplit), 1))));
    let targets = (List.map(fst,List.nth(fst(targetSplit), 0)) , Array.of_list(List.map((xs) => List.map(fst,xs),drop(snd(targetSplit),1))));

    (sources, targets)
}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let allEdges = [net.inputs] @ net.edges @ [net.outputs];
    let (i,eds,is,os,lu,ll,fu,fl) = generateAlgebraicDefinition' (allEdges);
    let ks = generateAllConnections(os,is,allEdges);
    let (eds, is,os,fu,fl) = normaliseEdgeIds(net.inputs^.id, net.outputs^.id, eds, is, os, fu,fl);
    
    let (s,t) = generateSourcesAndTargets(eds, is, os);
    
    {v: i, 
     e: List.length(eds) - 2, 
     i:Array.of_list(List.map(snd,is)), 
     o:Array.of_list(List.map(snd,os)), 
     k:Array.of_list(List.map(snd,ks)), 
     lu:lu,
     ll:ll, 
     fu:Array.of_list(List.map(snd,fu)),
     fl:Array.of_list(List.map(snd,fl)),
     s: s,
     t: t
    }

} and generateAlgebraicDefinition' = (edges) => generateAlgebraicDefinition''(edges, 0, 0, [], [], [], [], [], [], [])
and generateAlgebraicDefinition'' = (edges, i, o, eds, is, os, lu, ll, fu, fl) => {
    switch(edges){
        | [] => (i, List.rev(eds), is, os, List.rev(lu), List.rev(ll), List.rev(fu), List.rev(fl))
        | [e,...es] => let (is', os') = generateInputsAndOutputs(i,o,e);
                       let lu' = List.mem(e^.label, lu) || e^.label == alpha || e^.label == omega ? lu : [e^.label,...lu];
                       let ll' = List.mem(e^.latex, ll) || e^.latex == alphaLatex || e^.latex == omegaLatex ? ll : [e^.latex,...ll];
                       let fu' = e^.label == alpha || e^.label == omega ? fu : [(e^.id, e^.label),...fu];
                       let fl' = e^.latex == alphaLatex || e^.latex == omegaLatex ? fl : [(e^.id, e^.latex),...fl];
                       generateAlgebraicDefinition'' (es, i + Array.length(e^.sources), 
                                                      o + Array.length(e^.targets), 
                                                      [e^.id,...eds], 
                                                      is @ is', 
                                                      os @ os', 
                                                      lu',
                                                      ll',
                                                      fu',
                                                      fl'
                                                     )
    }



}