open Circuits;
open Parser;
open Lattices;
open Hypernets;

module Graphviz = {
    [@bs.module "graphviz-react"][@react.component]
    external make : (~dot : string) => React.element = "Graphviz"
}

module MathJaxNode = {
    [@bs.module "react-mathjax2"][@react.component]
    external make : (~inline : bool, ~children : React.element) => React.element = "Node"
}

module MathJaxContext = {
    [@bs.module "react-mathjax2"][@react.component]
    external make : (~input : string, ~children : React.element) => React.element = "Context"
}

module MathJax = {
    [@react.component]
    let make = (~string) => {
        <MathJaxContext input="tex">
            <div>
                <MathJaxNode inline=true>string</MathJaxNode>
            </div>
        </MathJaxContext>
    }
}

let str = React.string;

type state = {
    old: string,             /* Currently to stop multiple things happening */
    lat: lattice,           /* The lattice being used */
    circ: circuit,          /* The current circuit */
    strn: string,           /* The string of the current circuit, or a parse error message */
    funs: list(circuit),    /* The library of functions available */
    net: hypernet,          /* The corresponding hypernet */
    dot: string,            /* The corresponding dot string */
    error: bool             /* If there's a parse error */
}

/*
 * Prints either a latex representation of a circuit, or an error message
 */
let printLatexOrError = (string, error) => {
    if(error){
        <MathJax string=string />
    } else {
        string
    }
}

/*  
 * Generate a circuit from the state and a string
 * If parsing the string succeeds, returns (true, (circuit, latex string))
 * If not, returns (false, (zero circuit, error message))
 */
let generateCircuit = (state, text) => {
    switch(parseFromString(state.lat, state.funs, text)){ /* hello */
    | item => (true, (item, printCircuitLatex(item)))
    | exception ParseError(e) => (false, (zero(state.lat), e))
    | exception SemanticsError(e) => (false, (zero(state.lat), e))
    }
}

type action =
  | ParseNewCircuit(string);

let valueFromEvent = (evt) : string => evt->ReactEvent.Form.target##value;

/**
 * Component for an input text box that fires an event when enter is pressed
 */
module Input = {
    type state = string;

    [@react.component]
    let make = (~onSubmit) => {
        let (text, setText) = React.useReducer((_, newText) => newText, "");
        <input
            value = text
            type_ = "text"
            placeholder = "Type in a circuit!"
            onChange = ((evt) => setText(valueFromEvent(evt)))
            onKeyDown = ((evt) => 
                if(ReactEvent.Keyboard.key(evt) == "Enter") {
                    onSubmit(text); 
                }
            )
        />
    }
}

[@react.component]
let make = () => {
    let({strn,dot,error},dispatch) = React.useReducer((state,action) => {
        switch(action) {
        | ParseNewCircuit(text) =>  Js.log(state.old ++ " old vs new " ++ text);
                                    if(state.old == text){
                                        state
                                    } else { 
                                        let generatedCircuit = generateCircuit(state, text);
                                        let generatedHypernet = convertCircuitToHypernet(fst(snd(generatedCircuit)));
                                        let generatedDot = generateGraphvizCode(generatedHypernet);
                                        Js.log(generatedDot);
                                        {circ: fst(snd(generatedCircuit)), 
                                        old: text,
                                        lat: state.lat, 
                                        strn: snd(snd(generatedCircuit)), 
                                        funs: state.funs, 
                                        net: generatedHypernet,
                                        dot: generatedDot,
                                        error:fst(generatedCircuit)}
                                    }
                                }
    }, {
        old: "",
        lat: simpleLattice,
        circ: zero(simpleLattice),
        strn: "",
        funs: Examples.exampleFunctions,
        net: zeroNet,
        dot: zeroDot,
        error: false
    });
    <div className = "main">
        <div className = "title">
            <h1>(str("Circuit visualiser "))</h1>
        </div>
        <table>
        <tr>
            <td>
                <div className = "input">
                <Input onSubmit=((text) => dispatch(ParseNewCircuit((text)))) />
                </div>
                <div>
                    (printLatexOrError(str(strn), error))
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
                    <div> <span className = "code">(str("\\xy."))</span> (str(" or ")) <span className = "code">(str("\\x,y."))</span><b>(str(" Link"))</b> (str(" outlink x with inlink y"))</div>
                </div>
            </td>
            <td>
                <div className="right">
                <Graphviz dot=dot/>
                </div>
            </td>
        </tr>
        </table>    
    </div>
    
}