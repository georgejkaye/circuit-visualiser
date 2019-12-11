open Circuits;
open Parser;

let str = React.string;

let valueFromEvent = (evt) : string => evt->ReactEvent.Form.target##value;

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

[@react.component]
let make = () => {
    <div className = "main">
        <div className = "title">
        <h1>(str("Circuit visualiser "))</h1>
        </div>
        <div className = "input">
        <Input onSubmit=((text) => Js.log(printCircuit(parseFromString(Constructs.v, Examples.exampleFunctions, text)))) />
        </div>
    </div>
}