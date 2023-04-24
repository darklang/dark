module.exports = grammar({
  name: 'darklang',

  // Annoyingly, it seems to be best to write these 'top-most first'
  rules: {
    source_file: $ => repeat($.fn_def),

    // Function definitions
    fn_def: $ => seq(
      'let',
      field('name', $.identifier),
      field('params', $.fn_params_def),
      ':',
      field('return_type', $.type),
      '=',
      field('body', $.expression)
    ),

    // fns need at least one param
    // TODO: should this rule be enforced like this _in the parser_?
    // TODO: we could just repeat1 here
    fn_params_def: $ => seq(
      field("first", $.fn_param_def),
      field("additional", repeat($.fn_param_def))
    ),

    fn_param_def: $ => choice(
      $.unit,
      seq('(',
        field('identifier', $.identifier),
        ':',
        field('typ', $.type),
        ')'
      )
    ),


    // Expressions
    expression: $ => choice(
      $.let_expression,
      //$.function_call,
      $.infix_operation,
      $.identifier,
      $.string_literal,
      $.unit
    ),
    let_expression: $ => seq(
      'let',
      field('identifier', $.identifier),
      '=',
      field('expr', $.expression),
      choice('in', '\n'),
      field('body', $.expression),
    ),
    function_call: $ => seq(
      field('fn', $.identifier),
      field('args', repeat1($.expression))
    ),
    string_literal: $ => seq(
      '"',
      field('value', /.*/),
      '"'
    ),
    infix_operation: $ => prec.left(1, seq(
      field('left', $.expression),
      field('operator', choice('+', '-')),
      field('right', $.expression),
    )),

    // Basics
    unit: $ => '()',
    type: $ => choice(/Int/, /Bool/, /Float/, /String/, /Char/),
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
  }
});
