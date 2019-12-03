open Demo;

let str = React.string;

let log = () =>
    Js.log("Hey there!");

[@react.component]
let make = () => {
    <div className="app">
        <div className="title"> 
            <p>(str(Circuits.printCircuit(Demo.halfAdder))) </p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderApplied)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_1)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_2)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_3)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_4)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_5)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_6)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_7)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_8)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_9)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_10)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_11)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_12)))</p>
            <button onClick=((_evt) => log())>(str("next circuit"))</button>
        </div>
    </div>
}