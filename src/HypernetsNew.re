
type connection = Edge(ref(edge), int) | Vertex(ref(vertex))
and vertex = {
    id: int,
    connin: connection,
    connout:  connection
} and edge = {
    id: int,
    m: int,
    n: int,
    label: string
} and hypergraph = {
    invertices: list(ref(vertex)),
    outvertices: list(ref(vertex)),
    edges: list(ref(edge)),
    input: edge,
    output: edge,
    ins : int,
    outs : int
}

/* Get the sources of an edge */
let rec sources = (edge, hyp) => {
    let placeholder = List.hd(hyp.outvertices);
    sources'(edge, hyp.outvertices, Array.make(edge.m, placeholder))  
} and sources' = (edge, vs, acc) => {
    switch(vs){
    | [] => acc
    | [v,...vs] => switch(v^.connout){
                    | Edge(e,x) => e^.id == edge.id ? Array.set(acc,x,v) : ()
                                            sources'(edge, vs, acc)
                    | Vertex(_) => sources'(edge, vs, acc)
                   }
    }
}

/* Get the targets of an edge */
let rec targets = (edge, hyp) => {
    let placeholder = List.hd(hyp.invertices);
    targets'(edge, hyp.invertices, Array.make(edge.n, placeholder))  
} and targets' = (edge, vs, acc) => {
    switch(vs){
    | [] => acc
    | [v,...vs] => switch(v^.connin){
                    | Edge(e,x) => e^.id == edge.id ? Array.set(acc,x,v) : ()
                                            sources'(edge, vs, acc)
                    | Vertex(_) => targets'(edge, vs, acc)
                   }
    }
}

/* Compose two hypergraphs sequentially */
let rec composeSequential = (f,g) => {
 
    assert(f.outs == g.ins);

    let outputs = sources(f.output, f);
    let inputs = targets(g.input, g); 

    for (i in 0 to f.outs){

        let ov = outputs[i];
        let iv = inputs[i];

        ov := {id: ov^.id, connin: ov^.connin, connout: Vertex(iv)};
        iv := {id: iv^.id, connin: Vertex(ov), connout: iv^.connout};
    };

    {invertices: f.invertices @ g.invertices, outvertices: f.outvertices @ g.outvertices, edges:f.edges @ g.edges, input: f.input, output: g.input, ins: f.ins, outs: g.outs}

}