/**
 * File containing several useful constructs,
 * such as common logic gates and multiplexers
 */

open Lattices;
open Circuits;

let v = simpleLattice;
let t = value(v,t);
let f = value(v,f);
let bot = value(v,bot);
let top = value(v,top);

let andGate = (v) => {v, c:Function({js|AND|js}, "\\wedge", 2, 1, (c) => 
                                            switch(c.c){
                                            | Tensor([{v:_, c:Value(a), l:_}, {v:_, c:Value(b), l:_}]) => value(v, v.andOp(a,b)) 
                                            | Tensor([a, b])                => func(v, printCircuit(a) ++ " AND " ++ printCircuit(b),
                                                                                        printCircuitLatex(a) ++ " \\wedge " ++ printCircuit(b),
                                                                                        inputs(a) + inputs(b), 
                                                                                        1, 
                                                                                        (_) => failwith("not implemented")
                                                                             ) 
                                            | _ => failwith("Bad input")
                                            }
                                     )
                     ,l:[]}

let orGate = (v) => {v:v, c:Function({js|OR|js}, "\\vee", 2, 1, (c) => 
                                          switch(c.c){
                                          | Tensor([{v:_, c:Value(a), l:_}, {v:_, c:Value(b), l:_}]) => value(v, v.orOp(a,b)) 
                                          | Tensor([a, b])                => func(v, printCircuit(a) ++ " OR " ++ printCircuit(b),
                                                                                    printCircuitLatex(a) ++ " \\vee " ++ printCircuit(b),
                                                                                    inputs(a) + inputs(b), 
                                                                                    1, 
                                                                                    (_) => failwith("not implemented")
                                                                             ) 
                                          | _ => failwith("Bad input")
                                          }
                                   )
                     ,l:[]}

let xorGate = (v) => {v:v, c:Function({js|XOR|js}, "\\oplus", 2, 1, (c) => 
                     switch(c.c){
                     | Tensor([{v:_, c:Value(a), l:_}, {v:_, c:Value(b), l:_}]) => value(v, v.andOp(v.notOp(v.andOp(a, b)), v.orOp(a,b)))
                     | Tensor([a, b])                => func(v, printCircuit(a) ++ " OR " ++ printCircuit(b),
                                                               printCircuitLatex(a) ++ " \\vee " ++ printCircuit(b),
                                                               inputs(a) + inputs(b), 
                                                               1, 
                                                               (_) => failwith("not implemented")
                                                        ) 
                     | _ => failwith("Bad input")
                     }
              )
,l:[]}

let notGate = (v) => {v:v, c:Function({js|NOT|js}, "\\neg", 1, 1, (c) =>
                                                 switch(c.c){
                                                 | Value(a) => value(v,v.notOp(a))
                                                 | a        => func(v,"¬" ++ printCircuit(c),
                                                                             "\\neg " ++ printCircuitLatex(c),
                                                                             inputs(c), 1, (_) => failwith("not implemented"))
                                                 }),l:[]}

let id = (v, n) => func(v,"id{" ++ string_of_int(n) ++ "}", "\\text{id}_" ++ string_of_int(n), n,n, (c) => c)

let first = (v) => func(v, "fst", "\\text{fst}", 2, 1, (c) => switch(c.c){
                                          | Tensor([a,b]) => a
                                          | _ => failwith("Bad input")
                                          })

let second = (v) => func(v, "snd", "\\text{snd}", 2, 1, (c) => switch(c.c){
                                                        | Tensor([a,b]) => b
                                                        | _ => failwith("Bad input")
                                                  }
       )

let multiplexer = (v) => func(v, "m", "\\text{m}", 3, 1, (c) => switch(c.c){
                                                        | Tensor([{v:_,c:Value(c),l:_},a,b]) => List.exists(((x) => x == c), v.highValues) ? a : b 
                                                        | _ => failwith("Bad input")
                                                     }
)