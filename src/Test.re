let str = React.string;

let log = () =>
    Js.log("Hey there!");

[@react.component]
let make = () => {
    <div className="app">
        <div className="title"> 
            (str("hello! ")) 
            <button onClick=((_evt) => log())>(str("next circuit"))</button>
        </div>
        <div id="circuit">(str(Circuits.printCircuit(Demo.circ2)))</div>
    </div>
}