open Circuits;
open Parser;
open Lattices;

let str = React.string;

type state = {
    lat: lattice,           /* The lattice being used */
    circ: circuit,          /* The current circuit */
    funs: list(component)   /* The library of functions available */
}

type action =
  | ParseNewCircuit(string);


let valueFromEvent = (evt) : string => evt->ReactEvent.Form.target##value;

let generateCircuit = (state, v,funcs,text) => {
    let newCircuit = switch(parseFromString(v,funcs,text)){
                        | item => item
                        | exception e => state.circ
                        };
    Js.log(printCircuitLatex(newCircuit));
    newCircuit;
}

module Input = {
    type state = string;

    [@react.component]
    let make = (~onSubmit) => {
        let (text, setText) = React.useReducer((oldText, newText) => newText, "");
        <input
            value = text
            type_ = "text"
            placeholder = "Type in a circuit!"
            onChange = ((evt) => setText(valueFromEvent(evt)))
            onKeyDown = ((evt) => 
                if(ReactEvent.Keyboard.key(evt) == "Enter") {
                    onSubmit(text);
                    setText("")
                }
            )
        />
    }
}

let newCircuit = () => identity(Constructs.v, 1);

[@react.component]
let make = () => {
    let({lat, circ, funs},dispatch) = React.useReducer((state,action) => {
        switch action {
        | ParseNewCircuit(text) => {circ: generateCircuit(state, state.lat, state.funs, text), lat: state.lat, funs: state.funs}
        }
    }, {
        lat: simpleLattice,
        circ: zero(simpleLattice),
        funs: Examples.exampleFunctions,
    });
    let number = List.length(funs);
    <div className = "main">
        <div className = "title">
            <h1>(str("Circuit visualiser "))</h1>
        </div>
        <div className = "input">
        <Input onSubmit=((text) => dispatch(ParseNewCircuit((text)))) />
        </div>
        <div>
            <h3> (str(printCircuit(circ))) </h3>
        </div>
        <div className = "instructions">
            <div> <span className = "code">(str("a . b"))</span> <b>(str(" Horizontal composition"))</b> (str(" left to right"))</div>
            <div> <span className = "code">(str("a * b"))</span> <b>(str(" Vertical composition (tensor)"))</b></div>
            <div> <span className = "code">(str("a^n"))</span> <b>(str(" Index"))</b> (str(" Composes multiple copies of a circuit together vertically"))</div>
            <div> <span className = "code">(str("/\\"))</span> <b>(str(" Fork"))</b>(str(" a bus of width 1 into a bus of width 2"))</div>
            <div> <span className = "code">(str("\\/"))</span> <b>(str(" Join"))</b>(str(" a bus of width 2 into a bus of width 1"))</div>
            <div> <span className = "code">(str("/\\{n}"))</span> <b>(str(" Diagonal fork"))</b>(str(" a bus of width n into a bus of width 2n"))</div>
            <div> <span className = "code">(str("\\/{n}"))</span> <b>(str(" Diagonal join"))</b>(str(" a bus of width 2n into a bus of width n"))</div>
            <div> <span className = "code">(str("~"))</span> <b>(str(" Stub"))</b>(str(" a bus of width 1"))</div>
            <div> <span className = "code">(str("x{a,b}"))</span> <b>(str(" Swap"))</b> (str(" buses of width a and b"))</div>
            <div> <span className = "code">(str("Tr{n}(a)"))</span> <b>(str(" Trace"))</b> (str(" a circuit, using n of its outputs as inputs"))</div>
            <div> <span className = "code">(str("iter(a)"))</span> <b>(str(" Iterate"))</b> (str(" a circuit, using all of its outputs as inputs"))</div>
            <div> <span className = "code">(str("\xy."))</span> (str(" or ")) <span className = "code">(str("\x,y."))</span><b>(str(" Link"))</b> (str(" outlink x with inlink y"))</div>
        </div>
    </div>
    
}