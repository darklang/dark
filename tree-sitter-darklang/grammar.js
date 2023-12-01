module.exports = grammar({
  name: "darklang",

  rules: {
    // TODO: should this be repeat1?
    source_file: $ => repeat($.fn_def),

    // Function definitions
    fn_def: $ =>
      seq(
        "let",
        field("name", $.identifier),
        field("params", $.fn_params_def),
        ":",
        field("return_type", $.type),
        "=",
        field("body", $.expression),
      ),

    // fns need at least one param
    // TODO: should this rule be enforced like this _in the parser_?
    fn_params_def: $ => repeat1($.fn_param_def),

    fn_param_def: $ =>
      choice(
        $.unit,
        seq(
          "(",
          field("identifier", $.identifier),
          ":",
          field("typ", $.type),
          ")",
        ),
      ),

    // Expressions
    expression: $ =>
      choice(
        $.let_expression,
        $.function_call,
        $.infix_operation,
        $.identifier,
        $.string_literal,
        $.unit,
      ),
    let_expression: $ =>
      seq(
        "let",
        field("identifier", $.identifier),
        "=",
        field("expr", $.expression),
        choice("in", "\n"),
        field("body", $.expression),
      ),
    function_call: $ =>
      prec(
        1,
        seq(field("fn", $.identifier), field("args", repeat1($.expression))),
      ),

    string_literal: $ => seq('"', field("value", /.*/), '"'),
    infix_operator: $ => choice("+", "-"),
    infix_operation: $ =>
      prec.left(
        1,
        seq(
          field("left", $.expression),
          field("operator", $.infix_operator),
          field("right", $.expression),
        ),
      ),

    // Basics
    unit: $ => "()",
    type: $ => choice(/Unit/, /Int/, /Bool/, /Float/, /String/, /Char/),
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
  },
});
