let str = React.string;

let log = () =>
    Js.log("Hey there!");

[@react.component]
let make = () => { 
    <div className="app"> 
        <div className="half-adder"> 
            <h3>(str("Half-adder"))</h3>
            <p>(str(Circuits.printCircuit(Examples.halfAdder))) </p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderApplied)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_1)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_2)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_3)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_4)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_5)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_6)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_7)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_8)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_9)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_10)))</p>
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_11)))</p>
        </div>
        <div className="full-adder">
            <h3>(str("Full-adder"))</h3>
            <p>(str(Circuits.printCircuit(Examples.fullAdder))) </p>
            <p>(str(Circuits.printCircuit(Examples.fullAdderApplied)))</p>
            <p>(str(Circuits.printCircuit(Examples.fullAdderReduced)))</p>
        </div>
        <div className="parsing">
            <h3>(str("Parsing examples"))</h3>
            <p><b>(str("Input string: "))</b>(str(Examples.exampleString))</p>
            <p><b>(str("Tokenised input: "))</b>(str(Helpers.printStringList(Examples.exampleTokenised)))</p>
            <p><b>(str("Parsed input: "))</b>(str(Circuits.printComponent(Constructs.v, fst(Examples.exampleParsed))))</p>
            <p><b>(str("Reduced input: "))</b>(str(Circuits.printCircuit(Examples.exampleReduced)))</p>
        </div>
    </div>
}