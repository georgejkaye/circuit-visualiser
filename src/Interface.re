open Circuits;
open Parser;
open Lattices;
open Hypernets;
open Algebraic;
open Drawing;

type options = {
    fit: bool,
    height: int,
    width: int,
};

module Graphviz = {
    [@bs.module "graphviz-react"][@react.component]
    external make : (~dot : string, ~options : options) => React.element = "Graphviz"
}

module MathJaxNode = {
    [@bs.module "react-mathjax2"][@react.component]
    external make : (~inline : bool, ~children : React.element) => React.element = "Node"
}

module MathJaxContext = {
    [@bs.module "react-mathjax2"][@react.component]
    external make : (~input : string, ~delay : int, ~children : React.element) => React.element = "Context"
}

module MathJax = {
    [@react.component]
    let make = (~string) => {
        <MathJaxContext input="tex" delay=2>
            <span className="latex">
                <MathJaxNode inline=true>string</MathJaxNode>
            </span>
        </MathJaxContext>
    }
}

let str = React.string;

type state = {
    old: string,            /* Currently to stop multiple things happening */
    lat: lattice,           /* The lattice being used */
    circ: circuit,          /* The current circuit */
    strn: string,           /* The string of the current circuit, or a parse error message */
    funs: list(circuit),    /* The library of functions available */
    macs: list(circuit),    /* The library of macros available */
    net: hypernet,          /* The corresponding hypernet */
    dot: string,            /* The corresponding dot string */
    alg: string,            /* The corresponding algebraic notation */
    form: string,           /* The corresponding formal dot string */
    error: bool,             /* If there's a parse error */

    width: int,             /* width of the window */
    height: int,             /* height of the window */
}

/*
 * Prints either a latex representation of a circuit, or an error message
 */
let printLatexOrError = (string, error) => {
    if(error){
        <MathJax string=string />
    } else {
        <span className= "latex">string</span>
    }
}

/*  
 * Generate a circuit from the state and a string
 * If parsing the string succeeds, returns (true, (circuit, latex string))
 * If not, returns (false, (zero circuit, error message))
 */
let generateCircuit = (state, text) => {

    if(text == ""){
        (true, (zero(state.lat), ""))
    } else {

    switch(parseFromString(state.lat, state.funs, state.macs, text)){ /* hello */
    | item => (true, (item, printCircuitLatex(item)))
    | exception ParseError(e) => (false, (zero(state.lat), e))
    | exception SemanticsError(e) => (false, (zero(state.lat), e))
    }
}
}

let minimiseHypergraph = (state) => {

}

let getWindowSize = () => {
    let window = Webapi.Dom.window;
    let width = Webapi.Dom.Window.innerWidth(window);
    let height = Webapi.Dom.Window.innerHeight(window);

    (width, height)
}

type action =
  | ParseNewCircuit(string)
  | MinimiseHypergraph;

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
            style = (ReactDOMRe.Style.make(~width="400px", ()))
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

    let (width, height) = getWindowSize();

    let({strn,dot,alg,error},dispatch) = React.useReducer((state,action) => {
        switch(action) {
        | ParseNewCircuit(text) =>  if(state.old == text){
                                        state;
                                    } else { 
                                        let generatedCircuit = generateCircuit(state, text);
                                        let generatedHypernet = convertCircuitToHypernet(fst(snd(generatedCircuit)));
                                        let generatedDot = generateGraphvizCode(generatedHypernet);
                                        let generatedAlg = generateAlgebraicDefinition(generatedHypernet);
                                        let algebraicLatex = (fst(generatedCircuit) ? algebraicNetLatex(generatedAlg) : "");
                                        let formalDot = generateFormalGraphvizCode(generatedAlg);
                                        let (width, height) = getWindowSize();
                                        {circ: fst(snd(generatedCircuit)), 
                                        old: text,
                                        lat: state.lat, 
                                        strn: snd(snd(generatedCircuit)), 
                                        funs: state.funs, 
                                        macs: state.macs,
                                        net: generatedHypernet,
                                        dot: generatedDot,
                                        alg: algebraicLatex,
                                        form: formalDot,
                                        error:fst(generatedCircuit),
                                        width:width,
                                        height:height}
                                    }
        | MinimiseHypergraph =>     let minimisedHypernet = minimise(state.net);
                                    let generatedDot = generateGraphvizCode(minimisedHypernet);
                                    let generatedAlg = generateAlgebraicDefinition(minimisedHypernet);
                                    let algebraicLatex = algebraicNetLatex(generatedAlg);
                                    let formalDot = generateFormalGraphvizCode(generatedAlg);
                                    let (width, height) = getWindowSize();
                                    {circ: state.circ, 
                                        old: "",
                                        lat: state.lat, 
                                        strn: state.strn,
                                        funs: state.funs, 
                                        macs: state.macs,
                                        net: minimisedHypernet,
                                        dot: generatedDot,
                                        alg: algebraicLatex,
                                        form: formalDot,
                                        error:state.error,
                                        width: width,
                                        height: height,
                                    }
        }
    }, {
        old: "",
        lat: simpleLattice,
        circ: zero(simpleLattice),
        strn: "",
        funs: Examples.exampleFunctions,
        macs: Examples.exampleMacros,
        net: zeroNet,
        dot: zeroDot,
        alg: zeroAlg,
        form: "",
        error: false,
        width: width,
        height: height,
    });
    <div className = "main">
        <div className = "title">
            <h1>(str("Circuit visualiser "))</h1>
        </div>
        <div className = "darker">
            <span className = "input">
                <Input onSubmit=((text) => dispatch(ParseNewCircuit((text)))) /> /*<button onClick={_ => dispatch(MinimiseHypergraph)}>{str("Minimise")}</button>*/
            </span>
           <span className = "darker-wide">
                (printLatexOrError(str(strn), error))
            </span>
        </div>
        <table>
        <tbody>
        <tr>
            <td>
                <table width="400px">
                <tbody>
                    <tr>
                        <td>
                            <span className = "code">(str("f . g"))</span> 
                        </td>
                        <td>
                            <b>(str("Horizontal composition"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("f * g"))</span> 
                        </td>
                        <td>
                            <b>(str("Vertical composition"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("f^n"))</span> 
                        </td>
                        <td>
                            <b>(str("Indexed tensor"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("/\\"))</span> 
                        </td>
                        <td>
                            <b>(str("Fork"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("\\/"))</span> 
                        </td>
                        <td>
                            <b>(str("Join"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("/\\{n}"))</span> 
                        </td>
                        <td>
                            <b>(str("Diagonal fork"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("\\/{n}"))</span> 
                        </td>
                        <td>
                            <b>(str("Diagonal join"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("~"))</span> 
                        </td>
                        <td>
                            <b>(str("Stub"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("x{m,n}"))</span> 
                        </td>
                        <td>
                            <b>(str("Swap"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("Tr{x}(f)"))</span> 
                        </td>
                        <td>
                            <b>(str("Trace"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("iter(f)"))</span> 
                        </td>
                        <td>
                            <b>(str("Iteration"))</b>
                        </td>
                    </tr>
                    <tr>
                        <td>
                            <span className = "code">(str("\\xy.f"))</span> 
                        </td>
                        <td>
                            <b>(str("Link"))</b>
                        </td>
                    </tr>
                    </tbody>
                </table>
            </td>
            <td>
                <Graphviz dot=dot options = {fit: true, height: height-100, width: width-500}/>   
            </td>
        </tr>
        </tbody>
        </table> 
    </div>
    
}

/* <td width="500px">
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
                <div>
                <textarea rows=15 cols=100 value=dot readOnly=true></textarea>
                </div>
            </td>
            */