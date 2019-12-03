open Demo;

let str = React.string;

let log = () =>
    Js.log("Hey there!");

[@react.component]
let make = () => {
    <div className="app">
        <div className="title"> 
            (str(Circuits.printCircuit(Demo.halfAdder)))
            <button onClick=((_evt) => log())>(str("next circuit"))</button>
        </div>
    </div>
}