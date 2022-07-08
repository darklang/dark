let recover: (~sendToRollbar: bool=?, ~debug: 'd=?, string, 'r) => 'r

let recoverOpt: (~sendToRollbar: bool=?, ~debug: 'd=?, string, ~default: 'r, option<'r>) => 'r

let recoverOption: (~sendToRollbar: bool=?, ~debug: 'd=?, string, option<'r>) => option<'r>

let assert_: (~sendToRollbar: bool=?, ~debug: 'd=?, string, bool, 'r) => 'r

let asserT: (~sendToRollbar: bool=?, ~debug: 'd=?, string, bool) => unit

let assertFn: (~sendToRollbar: bool=?, ~debug: 'd=?, string, ~f: 'r => bool, 'r) => 'r

let asserTFn: (~sendToRollbar: bool=?, ~debug: 'd=?, string, ~f: unit => bool) => unit

let todo: (~sendToRollbar: bool=?, string, 'b) => 'b
