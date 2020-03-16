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

/* Get the number of edges in a hypernet */
let numberOfEdges = (net) => List.length (net.edges)

let rec normaliseEdgeIds = (net) => normaliseEdgeIds' (net.edges, 2, []) 
and normaliseEdgeIds' = (edges, next, acc) => {
    switch(edges) {
    | [] => List.rev(acc)
    | [e,...es] => let e' = e^;
                   normaliseEdgeIds' (es, next + 1, [{id: next, sources: e'.sources, targets: e'.targets, label: e'.label},...acc])
    }
}

/* Get the number of vertices in a hypernet, i.e. number of target ports */
let rec numberOfVertices = (net) => numberOfVertices' (net.edges, 0, [], [], [])
and numberOfVertices' = (edges, num, is, os, k) => {
    
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

    (is, os)

}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let defs = generateAlgebraicDefinition' ([ref(net.inputs)] @ net.edges @ [ref(net.outputs)])

} and generateAlgebraicDefinition' = (edges) => generateAlgebraicDefinition''(edges, 0, 0, [], [], [], [], [], [])
and generateAlgebraicDefinition'' = (edges, i, o, eds, is, os, k, l, f) => {
    switch(edges){
        | [] => (v, eds, List.rev(is), List.rev(os), List.rev(k), List.rev(l), List.rev(l))
        | [e,...es] => let (is', os') = generateInputsAndOutputs(i,o,e)

                       generateAlgebraicDefinition'' (es, v + Array.length(e^.targets), [e^.id,...eds], is, os, k, l, f)
        }



}