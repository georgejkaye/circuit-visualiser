// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Caml_builtin_exceptions from "bs-platform/lib/es6/caml_builtin_exceptions.js";

function printLattice(lattice) {
  switch (lattice) {
    case /* Bottom */0 :
        return "\xe2\x8a\xa5";
    case /* False */1 :
        return "f";
    case /* True */2 :
        return "t";
    case /* Top */3 :
        return "T";
    
  }
}

function inputs(_item) {
  while(true) {
    var item = _item;
    if (typeof item === "number") {
      return 1;
    } else {
      switch (item.tag | 0) {
        case /* Identity */1 :
            return item[0];
        case /* Composition */2 :
            _item = item[0];
            continue ;
        case /* Tensor */3 :
            return List.fold_left((function (no, comp) {
                          return no + inputs(comp) | 0;
                        }), 0, item[0]);
        case /* Function */4 :
            return item[1];
        case /* Trace */5 :
        case /* Iter */6 :
            return inputs(item[1]) - item[0] | 0;
        case /* Value */0 :
        case /* Input */7 :
            return 0;
        case /* Output */8 :
            return 1;
        case /* Link */9 :
            _item = item[2];
            continue ;
        
      }
    }
  };
}

function outputs(_item) {
  while(true) {
    var item = _item;
    if (typeof item === "number") {
      return 1;
    } else {
      switch (item.tag | 0) {
        case /* Identity */1 :
            return item[0];
        case /* Tensor */3 :
            return List.fold_left((function (no, comp) {
                          return no + outputs(comp) | 0;
                        }), 0, item[0]);
        case /* Function */4 :
            return item[2];
        case /* Trace */5 :
            return outputs(item[1]) - item[0] | 0;
        case /* Composition */2 :
        case /* Iter */6 :
            _item = item[1];
            continue ;
        case /* Output */8 :
            return 0;
        case /* Link */9 :
            _item = item[2];
            continue ;
        default:
          return 1;
      }
    }
  };
}

function makeCircuit(component, string) {
  return /* Circuit */[
          /* [] */0,
          /* [] */0,
          component,
          string
        ];
}

function compose(f, g) {
  if (outputs(f) !== inputs(g)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Circuits.re",
            62,
            4
          ]
        ];
  }
  return /* Composition */Block.__(2, [
            f,
            g
          ]);
}

function composemany(list) {
  if (list) {
    return List.fold_left(compose, list[0], list[1]);
  } else {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "Circuits.re",
            66,
            32
          ]
        ];
  }
}

function exp$prime(f, x) {
  if (x === 0) {
    return /* [] */0;
  } else {
    return /* :: */[
            f,
            exp$prime(f, x - 1 | 0)
          ];
  }
}

function exp(f, x) {
  return /* Tensor */Block.__(3, [exp$prime(f, x)]);
}

function trace(x, f) {
  if (!(inputs(f) >= x && outputs(f) >= x)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Circuits.re",
            85,
            4
          ]
        ];
  }
  return /* Trace */Block.__(5, [
            x,
            f
          ]);
}

function printCircuit$prime(component) {
  if (typeof component === "number") {
    return "ẟ";
  } else {
    switch (component.tag | 0) {
      case /* Value */0 :
          return printLattice(component[0]);
      case /* Identity */1 :
          return String(component[0]);
      case /* Composition */2 :
          return printCircuit$prime(component[0]) + (" ⋅ " + printCircuit$prime(component[1]));
      case /* Tensor */3 :
          var match = component[0];
          if (match) {
            return "[" + (List.fold_left((function (string, comp) {
                            return string + (" ⊗ " + printCircuit$prime(comp));
                          }), printCircuit$prime(match[0]), match[1]) + "]");
          } else {
            throw [
                  Caml_builtin_exceptions.match_failure,
                  /* tuple */[
                    "Circuits.re",
                    89,
                    39
                  ]
                ];
          }
      case /* Function */4 :
          return component[0];
      case /* Trace */5 :
          return "Tr[" + (String(component[0]) + ("](" + (printCircuit$prime(component[1]) + ")")));
      case /* Iter */6 :
          return "iter[" + (String(component[0]) + ("](" + (printCircuit$prime(component[1]) + ")")));
      case /* Input */7 :
          return ":" + String(component[0]);
      case /* Output */8 :
          return String(component[0]) + ":";
      case /* Link */9 :
          return "|" + (String(component[0]) + ("-" + (String(component[1]) + ("|" + printCircuit$prime(component[2])))));
      
    }
  }
}

function printCircuit(circuit) {
  var comp = circuit[2];
  return circuit[3] + (" : " + (String(inputs(comp)) + (" → " + (String(outputs(comp)) + ("\n" + printCircuit$prime(comp))))));
}

var stub = /* Function */Block.__(4, [
    "~",
    1,
    0
  ]);

function traceAsIteration(trace) {
  if (typeof trace === "number") {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "Circuits.re",
            118,
            34
          ]
        ];
  } else if (trace.tag === /* Trace */5) {
    var x = trace[0];
    var x$1 = outputs(trace);
    return composemany(/* :: */[
                /* Iter */Block.__(6, [
                    x + outputs(trace) | 0,
                    composemany(/* :: */[
                          /* Tensor */Block.__(3, [/* :: */[
                                /* Identity */Block.__(1, [x]),
                                /* :: */[
                                  /* Tensor */Block.__(3, [exp$prime(stub, x$1)]),
                                  /* :: */[
                                    /* Identity */Block.__(1, [inputs(trace)]),
                                    /* [] */0
                                  ]
                                ]
                              ]]),
                          /* :: */[
                            trace[1],
                            /* [] */0
                          ]
                        ])
                  ]),
                /* :: */[
                  /* Tensor */Block.__(3, [/* :: */[
                        /* Tensor */Block.__(3, [exp$prime(stub, x)]),
                        /* :: */[
                          /* Identity */Block.__(1, [outputs(trace)]),
                          /* [] */0
                        ]
                      ]]),
                  /* [] */0
                ]
              ]);
  } else {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "Circuits.re",
            118,
            34
          ]
        ];
  }
}

console.log("hello!");

var fork = /* Function */Block.__(4, [
    "⋏",
    1,
    2
  ]);

var join = /* Function */Block.__(4, [
    "⋎",
    1,
    2
  ]);

var delay = /* Delay */0;

var hello = /* () */0;

export {
  printLattice ,
  inputs ,
  outputs ,
  makeCircuit ,
  compose ,
  composemany ,
  exp$prime ,
  exp ,
  trace ,
  printCircuit$prime ,
  printCircuit ,
  fork ,
  join ,
  stub ,
  delay ,
  traceAsIteration ,
  hello ,
  
}
/*  Not a pure module */
