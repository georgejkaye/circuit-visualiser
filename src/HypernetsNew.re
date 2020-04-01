open Circuits;
open Lattices;

let alpha = {js|α|js}
let omega = {js|ω|js}

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
    input: ref(edge),
    output: ref(edge),
    ins : int,
    outs : int
}

/* Get the sources of an edge */
let rec sources = (edge, hyp) => {
    let placeholder = List.hd(hyp.outvertices);
    sources'(edge^, hyp.outvertices, Array.make(edge^.m, placeholder))  
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
    targets'(edge^, hyp.invertices, Array.make(edge^.n, placeholder))  
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

let rec generateSetOfVertices = (x, i) => {

}

let rec convertCircuitToHypernet = (circuit) => convertCircuitToHypernet'(circuit, 0, 0, 0)
and convertCircuitToHypernet' = (circuit, oi, ii, ei) => {
    switch(circuit.c){
    | Value(x)                  => let ine  = ref({id:ei, label:alpha, m: 0, n: 0});
                                   let vale = ref({id:ei+1, label:circuit.v.print(x), m: 0, n: 1});
                                   let oute = ref({id:ei+1, label:omega, m: 1, n:0});
                                   let rec vo = ref({id: oi, connin: Edge(vale, 0), connout: Vertex(vi)}) and
                                        vi = ref({id: ii, connin: Vertex(vo), connout: Edge(oute, 0)});
                                    {invertices: [vi], outvertices: [vo], edges: [vale], input: ine, output: oute, ins: 0, outs: 1}
    | Identity(n)               => failwith("todo")
    | Composition(f,g)          => failwith("todo")
    | Tensor([x,...xs])         => failwith("todo")
    | Swap(x,y)                 => failwith("todo")
    | Function(id,_,ins,outs,_) => failwith("todo")
    | Delay(x)                  => failwith("todo")
    | Trace(x, f)               => failwith("todo")
    | Iter(_, f)                => failwith("todo")
    | Macro(_,_,f)              => failwith("todo")
    | Inlink(x)                 => failwith("todo")
    | Outlink(x)                => failwith("todo")
    | Link(x,y,f)               => failwith("todo")
                                   failwith("todo")
    | _                         => failwith("todo")
                             
                                    
    }
}