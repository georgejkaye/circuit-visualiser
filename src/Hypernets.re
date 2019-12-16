/* An edge represents a morphism between buses */
type edge = { 
    inputs: array(ref(edge)),                    
    outputs: array(ref(edge)),
    label: string,
    rule: array(ref(edge)) => array(ref(edge))   /* rewrite rule */
} and hypernet = {
    inputs: ref(edge),
    edges: list(ref(edge)),
    outputs: ref(edge)
}

let identity = (array) => array

let compose = (h1,h2) => {

    let l1 = Array.length(h1.outputs);
    let l2 = Array.length(h1.inputs);

}