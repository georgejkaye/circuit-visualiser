type lattice('element) = {
    leq   : ('element, 'element) => bool,       /* The order relation less than */
    lubOp : ('element, 'element) => 'element,   /* The least upper bound */
    andOp : ('element, 'element) => 'element,   /* And operation */
    orOp  : ('element, 'element) => 'element,   /* Or operation */
    notOp : 'element => 'element,               /* Not operation */
    print : 'element => string                  /* Print operation */
}

/* A simple lattice containing four elements with the order Bottom < True, False < Top */

type simpleLatticeElems = Bottom | False | True | Top ;

let simpleLeq = (a, b) => {
    switch(a, b){
    | (Bottom, _)    => true
    | (_, Bottom)    => false
    | (False, False) => true
    | (False, True)  => true
    | (True, False)  => true
    | (True, True)   => true
    | (Top, _)       => false
    | (_, Top)       => true
    }
}

let printSimpleLattice = (lattice) => {
    switch (lattice) {
    | Bottom => "⊥"
    | False  => "f"
    | True   => "t"
    | Top    => "T"
    }
}

let simpleLub = (a,b) => {
    switch(a, b){
    | (Top, _)      => Top
    | (_, Top)      => Top
    | (a, Bottom)   => a
    | (Bottom, b)   => b
    | (True, _)     => True
    | (False, _)    => False
    }
}

let simpleAnd = (a,b) => {
    switch(a,b){
    | (Bottom, _) => Bottom
    | (_, Bottom) => Bottom
    | (False, _)  => False
    | (_, False)  => False
    | (True, a)   => a
    | (a, True)   => a
    | (Top, Top)    => Top
    }
}

let simpleOr = (a,b) => {
    switch(a,b){
    | (True, _)      => True
    | (_, True)      => True
    | (False, False) => False
    | (Bottom, _) => Bottom
    | (_, Bottom) => Bottom
    | (Top, _) => Top
    | (_, Top) => Top
    }
}

let simpleNot = a => {
    switch(a){
    | Top    => Top
    | Bottom => Bottom
    | True   => False
    | False  => True
    }
}

let simpleLattice: lattice(simpleLatticeElems) = {
    leq:   simpleLeq,
    lubOp: simpleLub,
    andOp: simpleAnd,
    orOp:  simpleOr,
    notOp: simpleNot,
    print: printSimpleLattice,
}

Js.log(simpleLattice.print(simpleLattice.andOp(True, True)));
Js.log(simpleLattice.leq(Bottom, True));
Js.log(simpleLattice.leq(False, True));