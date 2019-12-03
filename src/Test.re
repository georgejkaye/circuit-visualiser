open Demo;

let str = React.string;

let log = () =>
    Js.log("Hey there!");

[@react.component]
let make = () => {
    <div className="app">
        <div className="title"> 
            <p>(str(Circuits.printCircuit(Demo.halfAdder))) </p>
            <p>(str(Circuits.printCircuit(Demo.basicTrace))) </p>
            <p>(str(Circuits.printCircuit(Demo.basicTraceAsIteration)))</p>
            <p>(str(Circuits.printCircuit(Demo.traceTwo)))</p>
            <p>(str(Circuits.printCircuit(Demo.traceTwoAsIteration)))</p>
            <p>(str(Circuits.printCircuit(Demo.test1)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderApplied)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_1)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_2)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_3)))</p>
            <p>(str(Circuits.printCircuit(Demo.halfAdderReduced_4)))</p>
            <p>(str(Circuits.printCircuit(Demo.test2)))</p>
            <p>(str(Circuits.printCircuit(Demo.test2Reduced)))</p>
            <button onClick=((_evt) => log())>(str("next circuit"))</button>
        </div>
    </div>
}