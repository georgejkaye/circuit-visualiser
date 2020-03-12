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
    switch(edges){
    | [] => (num + 2, List.rev(is), List.rev(os), List.rev(k))
    | [e,...es] => numberOfVertices' (es, num + Array.length(e^.targets), is, os, k)
    }
}

let inputsFunction = (net) => {}

/* Generate the algebraic definition of a hypernet */
let rec generateAlgebraicDefinition = (net) => {
    let numEdges = numberOfEdges(net);
    let numVertices = numberOfVertices(net);



}