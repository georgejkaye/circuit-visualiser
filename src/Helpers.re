let assert' = (condition, message) =>
    condition ? () : failwith(message);