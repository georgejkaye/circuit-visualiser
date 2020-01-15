/**
 * File containing lattice definitions 
 * 
 * Lattices are defined as a list of pairs, with the first element
 * representing the 'level' of the lattice, and the second element
 * representing the position of the element in the level.
 * To define a lattice, a number of operations must also be provided
 * such as join and meet. 
 */

type latticeElement = (int, int)

type lattice = {
    elems     : list(latticeElement),                      /* The list of lattice elements */
    highValues : list(latticeElement),                             /* The value in the list that represents true */
    leq       : (latticeElement, latticeElement) => bool,        /* The order relation less than */
    joinOp    : (latticeElement, latticeElement) => latticeElement,   /* The least upper bound */
    meetOp    : (latticeElement, latticeElement) => latticeElement,   /* The greatest lower bound */
    andOp     : (latticeElement, latticeElement) => latticeElement,   /* And operation */
    orOp      : (latticeElement, latticeElement) => latticeElement,   /* Or operation */
    notOp     : (latticeElement) => latticeElement,              /* Not operation */
    print     : (latticeElement) => string,                 /* Print operation */
    parse     : string => (bool, latticeElement)            /* Parse a string and determine if it is a value, and if so which one */
}

/* A simple lattice containing four elements with the order Bottom < True, False < Top */

let simpleLatticeElems = [(0,0), /* Bottom */
                          (1,0), /* False */
                          (1,1), /* True */
                          (2,0), /* Top */
                         ]

let bot = (0,0);
let t = (1,1);
let f = (1,0);
let top = (2,0);

let simpleLeq = (a, b) => {
    switch(a, b){
    | ((0,0), _)    => true
    | (_, (0,0))    => false
    | ((1,0), (1,0)) => true
    | ((1,0), (1,1))  => true
    | ((1,1), (1,0))  => true
    | ((1,1), (1,1))   => true
    | ((2,0), _)       => false
    | (_, (2,0))       => true
    }
}

let printSimpleLattice = (elem) => {
    switch (elem) {
    | (0,0) => "⊥"
    | (1,0)  => "f"
    | (1,1)   => "t"
    | (2,0)    => "T"
    }
}

let simpleJoin = (a,b) => {
    switch(a, b){
    | ((2,0), _)      => (2,0)
    | (_, (2,0))      => (2,0)
    | ((0,0), a)   => a
    | (a, (0,0))   => a
    | ((1,1), _)     => (1,1)
    | ((1,0), _)    => (1,0)
    }
}

let simpleMeet = (a,b) => {
    switch(a, b){
    | ((0,0), _) => (0,0)
    | (_, (0,0)) => (0,0)
    | ((2,0), a)    => a
    | (a, (2,0))    => a
    | ((1,1), _)   => (1,1)
    | ((1,0), _)  => (1,0)
    }
}

let simpleAnd = (a,b) => {
    switch(a,b){
    | ((0,0), _) => (0,0)
    | (_, (0,0)) => (0,0)
    | ((1,0), _)  => (1,0)
    | (_, (1,0))  => (1,0)
    | ((1,1), a)   => a
    | (a, (1,1))   => a
    | ((2,0), (2,0))    => (2,0)
    }
}

let simpleOr = (a,b) => {
    switch(a,b){
    | ((1,1), _)      => (1,1)
    | (_, (1,1))      => (1,1)
    | ((1,0), (1,0)) => (1,0)
    | ((0,0), _) => (0,0)
    | (_, (0,0)) => (0,0)
    | ((2,0), _) => (2,0)
    | (_, (2,0)) => (2,0)
    }
}

let simpleNot = a => {
    switch(a){
    | (2,0) => (2,0)
    | (0,0) => (0,0)
    | (1,1) => (1,0)
    | (1,0) => (1,1)
    }
}

let simpleParse = (str) => {
    switch(str) {
    | "T" => (true, (2,0))
    | "t" => (true, (1,1))
    | "f" => (true, (1,0))
    | "B" => (true, (0,0))
    | _   => (false, (2,0))
    };
    
}

let simpleLattice: lattice = {
    elems: simpleLatticeElems,
    highValues: [(1,1)],
    leq:   simpleLeq,
    joinOp: simpleJoin,
    meetOp: simpleMeet,
    andOp: simpleAnd,
    orOp:  simpleOr,
    notOp: simpleNot,
    print: printSimpleLattice,
    parse: simpleParse
}

type availableLattices = Simple

let getLattice = (lat) => {
    switch(lat){
    | Simple => simpleLatticeElems
    }
}