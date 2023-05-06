open AST

let default_redirection_descriptor = function
  | Output | OutputDuplicate | OutputAppend | OutputClobber -> 1
  | Input | InputDuplicate | InputOutput -> 0
