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
            <p>(str(Circuits.printCircuit(Examples.halfAdderReduced_12)))</p>
        </div>
        <div className="full-adder">
            <h3>(str("Full-adder"))</h3>
            <p>(str(Circuits.printCircuit(Examples.fullAdder))) </p>
            <p>(str(Circuits.printCircuit(Examples.fullAdderApplied)))</p>
            <p>(str(Circuits.printCircuit(Examples.fullAdderReduced)))</p>
        </div>
        <div className="parsing">
            <p>(str(Helpers.printList(Parser.tokenise("(t * f * t * f) . (F * F)"), (x) => x)))</p>
        </div>
    </div>
}