// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Circuits$CircuitVisualiser from "./Circuits.bs.js";

var andGate = /* Function */Block.__(4, [
    "AND",
    2,
    1,
    Circuits$CircuitVisualiser.id
  ]);

var orGate = /* Function */Block.__(4, [
    "OR",
    2,
    1,
    Circuits$CircuitVisualiser.id
  ]);

var t = /* Value */Block.__(0, [/* True */2]);

var tensor1 = /* Tensor */Block.__(3, [/* :: */[
      t,
      /* :: */[
        t,
        /* :: */[
          t,
          /* [] */0
        ]
      ]
    ]]);

var tensor2 = /* Tensor */Block.__(3, [/* :: */[
      andGate,
      /* :: */[
        /* Identity */Block.__(1, [1]),
        /* [] */0
      ]
    ]]);

console.log(Circuits$CircuitVisualiser.makeCircuit(Circuits$CircuitVisualiser.composemany(/* :: */[
              t,
              /* :: */[
                t,
                /* :: */[
                  t,
                  /* :: */[
                    t,
                    /* [] */0
                  ]
                ]
              ]
            ]), "test"));

var circuit = Circuits$CircuitVisualiser.composemany(/* :: */[
      tensor1,
      /* :: */[
        tensor2,
        /* :: */[
          orGate,
          /* [] */0
        ]
      ]
    ]);

console.log(Circuits$CircuitVisualiser.printCircuit(/* Circuit */[
          /* [] */0,
          /* [] */0,
          circuit,
          "My first circuit"
        ]));

var f = /* Function */Block.__(4, [
    "f",
    1,
    1,
    Circuits$CircuitVisualiser.id
  ]);

var g = /* Function */Block.__(4, [
    "g",
    1,
    1,
    Circuits$CircuitVisualiser.id
  ]);

var h = /* Function */Block.__(4, [
    "h",
    2,
    1,
    Circuits$CircuitVisualiser.id
  ]);

var tensor3 = /* Tensor */Block.__(3, [/* :: */[
      /* Delay */0,
      /* :: */[
        g,
        /* [] */0
      ]
    ]]);

var tensor4 = /* Tensor */Block.__(3, [/* :: */[
      f,
      /* :: */[
        /* Identity */Block.__(1, [1]),
        /* [] */0
      ]
    ]]);

var circuit2 = Circuits$CircuitVisualiser.composemany(/* :: */[
      Circuits$CircuitVisualiser.fork,
      /* :: */[
        tensor3,
        /* :: */[
          tensor4,
          /* :: */[
            h,
            /* [] */0
          ]
        ]
      ]
    ]);

var circ2 = /* Circuit */[
  /* [] */0,
  /* [] */0,
  circuit2,
  "Second wind"
];

console.log(Circuits$CircuitVisualiser.printCircuit(/* Circuit */[
          /* [] */0,
          /* [] */0,
          circuit2,
          "Second wind"
        ]));

var f$prime = /* Function */Block.__(4, [
    "f",
    3,
    3,
    Circuits$CircuitVisualiser.id
  ]);

var circuit3 = Circuits$CircuitVisualiser.trace(2, f$prime);

console.log(Circuits$CircuitVisualiser.printCircuit(/* Circuit */[
          /* [] */0,
          /* [] */0,
          circuit3,
          "Trace"
        ]));

var circuit4 = Circuits$CircuitVisualiser.traceAsIteration(circuit3);

console.log(Circuits$CircuitVisualiser.printCircuit(Circuits$CircuitVisualiser.makeCircuit(circuit4, "Iteration")));

var swap1 = Circuits$CircuitVisualiser.swap(2, 1);

console.log(String(Circuits$CircuitVisualiser.inputs(swap1)));

console.log(String(Circuits$CircuitVisualiser.outputs(swap1)));

console.log(String(Circuits$CircuitVisualiser.inputs(/* Tensor */Block.__(3, [/* :: */[
                  Circuits$CircuitVisualiser.join,
                  /* :: */[
                    /* Identity */Block.__(1, [1]),
                    /* [] */0
                  ]
                ]]))));

var circuit5 = Circuits$CircuitVisualiser.composemany(/* :: */[
      Circuits$CircuitVisualiser.swap(2, 1),
      /* :: */[
        /* Tensor */Block.__(3, [/* :: */[
              Circuits$CircuitVisualiser.join,
              /* :: */[
                /* Identity */Block.__(1, [1]),
                /* [] */0
              ]
            ]]),
        /* [] */0
      ]
    ]);

var circ5 = Circuits$CircuitVisualiser.makeCircuit(circuit5, "Swap");

console.log(Circuits$CircuitVisualiser.printCircuit(circ5));

var circuit6 = Circuits$CircuitVisualiser.compose(t, Circuits$CircuitVisualiser.fork);

var circ6 = Circuits$CircuitVisualiser.makeCircuit(circuit6, "Fork II");

console.log(Circuits$CircuitVisualiser.printCircuit(circ6));

console.log(Circuits$CircuitVisualiser.printCircuit(Circuits$CircuitVisualiser.makeCircuit(Circuits$CircuitVisualiser.evaluateOneStep(circuit6), "Evaluated Fork II")));

var delay = /* Delay */0;

export {
  andGate ,
  orGate ,
  t ,
  tensor1 ,
  tensor2 ,
  circuit ,
  f ,
  g ,
  h ,
  delay ,
  tensor3 ,
  tensor4 ,
  circuit2 ,
  circ2 ,
  f$prime ,
  circuit3 ,
  circuit4 ,
  swap1 ,
  circuit5 ,
  circ5 ,
  circuit6 ,
  circ6 ,
  
}
/*  Not a pure module */
