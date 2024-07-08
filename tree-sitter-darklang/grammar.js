// TODOs:
// - int64 literals (e.g. `0L`)
// - deal with keywords
// - better support for partially-written code (i.e. `let x =`)
// - consider using Supertype Nodes for rules whose definitions are a simple choice eg. `simple_expression`, `consts`, etc.

const PREC = {
  LOGICAL_OR: 0,
  LOGICAL_AND: 1,
  COMPARISON: 2,
  SUM: 3,
  PRODUCT: 4,
  EXPONENT: 5,
  FIELDACCESS: 6,
  VAR_IDENTIFIER: 7,
  FN_IDENTIFIER: 8,
  PIPE_EXPR: 9,
};

const logicalOperators = choice("&&", "||");
const comparisonOperators = choice("==", "!=", "<", "<=", ">", ">=");
const additiveOperators = choice("+", "-");
const multiplicativeOperators = choice("*", "/", "%");
const exponentOperator = "^";
const stringConcatOperator = "++";

module.exports = grammar({
  name: "darklang",

  externals: $ => [$.indent, $.dedent],

  conflicts: $ => [[$.module_identifier, $.type_identifier]],

  rules: {
    source_file: $ =>
      seq(
        repeat($.module_decl),
        // all type and fn and constant defs first
        repeat(choice($.type_decl, $.fn_decl, $.const_decl)),

        // then the expressions to evaluate, in order
        repeat($.expression),
      ),

    type_params: $ =>
      seq(
        field("symbol_open_angle", alias(token.immediate("<"), $.symbol)),
        field("params", $.type_params_items),
        field("symbol_close_angle", alias(token.immediate(">"), $.symbol)),
      ),
    type_params_items: $ =>
      seq(
        $.variable_type_reference,
        repeat(
          seq(
            field("symbol_comma", alias(",", $.symbol)),
            $.variable_type_reference,
          ),
        ),
      ),

    // ---------------------
    // Module declarations
    // ---------------------
    module_decl: $ =>
      prec.left(
        seq(
          field("keyword_module", alias("module", $.keyword)),
          field("name", $.module_identifier),
          field("symbol_equals", alias("=", $.symbol)),
          seq($.indent, field("content", repeat1($.module_content)), $.dedent),
        ),
      ),
    module_content: $ =>
      seq(
        choice(
          $.module_decl,
          $.fn_decl,
          $.type_decl,
          $.const_decl,
          $.expression,
        ),
      ),

    // ---------------------
    // Constant declarations
    // ---------------------
    const_decl: $ =>
      seq(
        field("keyword_const", alias("const", $.keyword)),
        field("name", $.constant_identifier),
        field("symbol_equals", alias("=", $.symbol)),
        field("value", $.consts),
      ),

    consts: $ =>
      choice(
        $.int8_literal,
        $.uint8_literal,
        $.int16_literal,
        $.uint16_literal,
        $.int32_literal,
        $.uint32_literal,
        $.int64_literal,
        $.uint64_literal,
        $.int128_literal,
        $.uint128_literal,
        $.float_literal,
        $.bool_literal,
        $.string_literal,
        $.char_literal,
        $.const_list_literal,
        $.const_tuple_literal,
        $.const_dict_literal,
        $.const_enum_literal,
        $.unit,
      ),

    const_list_literal: $ => list_literal_base($, $.const_list_content),
    const_list_content: $ => list_content_base($, $.consts),

    const_tuple_literal: $ =>
      tuple_literal_base($, $.consts, $.const_tuple_literal_the_rest),
    const_tuple_literal_the_rest: $ => tuple_literal_the_rest_base($, $.consts),

    const_dict_literal: $ => dict_literal_base($, $.const_dict_content),
    const_dict_content: $ => dict_content_base($, $.const_dict_pair),
    const_dict_pair: $ => dict_pair_base($, $.expression, $.consts),

    const_enum_literal: $ => enum_literal_base($, $.const_enum_fields),
    const_enum_fields: $ => enum_fields_base($, $.consts),

    // ---------------------
    // Function declarations
    // ---------------------
    fn_decl: $ =>
      seq(
        field("keyword_let", alias("let", $.keyword)),
        field("name", $.fn_identifier),
        optional(field("type_params", $.type_params)),
        field("params", $.fn_decl_params),
        field("symbol_colon", alias(":", $.symbol)),
        field("return_type", $.type_reference),
        field("symbol_equals", alias("=", $.symbol)),
        field("body", $.expression),
      ),
    fn_decl_params: $ => repeat1(choice($.unit, $.fn_decl_param)),
    fn_decl_param: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("identifier", $.variable_identifier),
        field("symbol_colon", alias(":", $.symbol)),
        field("typ", $.type_reference),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),

    // ---------------------
    // Type declarations
    // ---------------------
    type_decl: $ =>
      seq(
        field("keyword_type", alias("type", $.keyword)),
        field("name", $.type_identifier),
        optional(field("type_params", $.type_params)),
        field("symbol_equals", alias("=", $.symbol)),
        field("typ", $.type_decl_def),
      ),

    // definition
    type_decl_def: $ =>
      choice(
        $.type_decl_def_alias,
        $.type_decl_def_record,
        $.type_decl_def_enum,
      ),

    //
    // Alias
    type_decl_def_alias: $ => $.type_reference,

    //
    // Record
    // e.g. `type Person = { name: String; age: Int }`
    // TODO: allow multi-line records where a newline is 'interpreted' as a record delimiter (i.e. no ; needed)
    type_decl_def_record: $ =>
      // CLEANUP consider supporting a mixed mode of these choices:
      // ```
      // type MyRecord =
      //   { a: Bool; b: Bool
      //     c: Bool }
      // ```
      // F#, for example, allows this
      choice(
        seq(
          field("symbol_open_brace", alias("{", $.symbol)),
          optional(field("fields", $.type_decl_def_record_fields)),
          field("symbol_close_brace", alias("}", $.symbol)),
        ),
        seq(
          $.indent,
          field("symbol_open_brace", alias("{", $.symbol)),
          optional(field("fields", $.type_decl_def_record_fields)),
          field("symbol_close_brace", alias("}", $.symbol)),
          $.dedent,
        ),
      ),

    type_decl_def_record_fields: $ =>
      choice(
        seq(
          field("first", $.type_decl_def_record_field),
          field(
            "others",
            repeat(
              seq(
                field("symbol_semicolon", alias(";", $.symbol)),
                $.type_decl_def_record_field,
              ),
            ),
          ),
        ),
        seq(
          field("first", $.type_decl_def_record_field),
          field("others", repeat(seq($.newline, $.type_decl_def_record_field))),
          optional($.newline),
        ),
      ),

    type_decl_def_record_field: $ =>
      seq(
        field("field_name", $.variable_identifier),
        field("symbol_colon", alias(":", $.symbol)),
        field("type", $.type_reference),
      ),

    //
    // Enums
    // e.g. `type Color = Red | Green | Blue`
    type_decl_def_enum: $ =>
      field(
        "content",
        choice($.type_decl_enum_single_line, $.type_decl_enum_multi_line),
      ),
    type_decl_enum_single_line: $ => repeat1($.type_decl_enum_case),
    type_decl_enum_multi_line: $ =>
      seq($.indent, repeat1($.type_decl_enum_case), $.dedent),
    type_decl_enum_case: $ =>
      seq(
        field("symbol_pipe", alias("|", $.symbol)),
        field("case_name", $.enum_case_identifier),
        optional(
          seq(
            field("keyword_of", alias("of", $.keyword)),
            seq(
              $.type_decl_enum_field,
              repeat(
                seq(
                  field("symbol_astrisk", alias("*", $.symbol)),
                  $.type_decl_enum_field,
                ),
              ),
            ),
          ),
        ),
      ),

    type_decl_enum_field: $ =>
      seq(
        optional(
          field(
            "type_annotation",
            seq(
              field("identifier", $.variable_identifier),
              field("symbol_colon", alias(":", $.symbol)),
            ),
          ),
        ),
        field("type", $.type_reference),
      ),

    // ---------------------
    // Match patterns
    // ---------------------
    match_pattern: $ =>
      choice(
        $.unit,
        alias($.bool_literal, $.bool),
        alias($.int8_literal, $.int8),
        alias($.uint8_literal, $.uint8),
        alias($.int16_literal, $.int16),
        alias($.uint16_literal, $.uint16),
        alias($.int32_literal, $.int32),
        alias($.uint32_literal, $.uint32),
        alias($.int64_literal, $.int64),
        alias($.uint64_literal, $.uint64),
        alias($.int128_literal, $.int128),
        alias($.uint128_literal, $.uint128),
        alias($.float_literal, $.float),
        alias($.char_literal, $.char),
        alias($.string_literal, $.string),
        alias($.mp_list, $.list),
        alias($.mp_list_cons, $.list_cons),
        alias($.mp_tuple, $.tuple),
        alias($.mp_enum, $.enum),
        alias($.variable_identifier, $.variable),
      ),

    // match pattern - list
    mp_list: $ => list_literal_base($, $.mp_list_content),
    mp_list_content: $ => list_content_base($, $.match_pattern),

    //
    // match pattern - list cons
    mp_list_cons: $ =>
      prec.left(
        seq(
          field("head", $.match_pattern),
          field("symbol_double_colon", alias("::", $.symbol)),
          field("tail", $.match_pattern),
        ),
      ),

    //
    // match pattern - tuple
    mp_tuple: $ => tuple_literal_base($, $.match_pattern, $.mp_tuple_the_rest),
    mp_tuple_the_rest: $ => tuple_literal_the_rest_base($, $.match_pattern),

    //
    // match pattern - enum
    mp_enum: $ =>
      prec.right(
        seq(
          field("case_name", $.enum_case_identifier),
          optional(field("enum_fields", $.mp_enum_fields)),
        ),
      ),

    mp_enum_fields: $ => enum_fields_base($, $.match_pattern),

    // ---------------------
    // Expressions
    // ---------------------

    /**
     * `simple_expression` vs. `expression`:
     *
     * `simple_expression`:
     * includes simple elements that have distinct starting and ending points, typically not requiring parentheses for clarity.
     * e.g. literals, variables, etc.
     *
     * `expression`:
     * Enompasses both simple expressions and more complex expressions (e.g. infix operations, function calls, etc.).
     * These are elements that may require parentheses to disambiguate the order and grouping of elements.
     *
     * e.g.
     * `myFunction myFunction2 arg arg2`
     *
     * without parentheses, It's unclear whether `arg2` is supposed to be a second argument to `myFunction2`:
     * myFunction (myFunction2 arg arg2)
     *
     * or a second argument to `myFunction`:
     * myFunction (myFunction2 arg) (arg2)
     *
     */

    // TODO: reconsider having enum_literal as a simple_expression
    simple_expression: $ =>
      choice(
        $.unit,
        $.bool_literal,
        $.int8_literal,
        $.uint8_literal,
        $.int16_literal,
        $.uint16_literal,
        $.int32_literal,
        $.uint32_literal,
        $.int64_literal,
        $.uint64_literal,
        $.int128_literal,
        $.uint128_literal,
        $.float_literal,
        $.string_literal,
        $.char_literal,
        $.list_literal,
        $.dict_literal,
        $.record_literal,
        $.enum_literal,
        $.variable_identifier,
        $.field_access,
      ),

    expression: $ =>
      choice(
        $.paren_expression,
        $.simple_expression,
        $.tuple_literal,
        $.if_expression,
        $.let_expression,

        $.match_expression,

        $.infix_operation,
        $.apply,

        $.lambda_expression,
        $.pipe_expression,

        $.record_update,

        $.qualified_const_name,
      ),

    qualified_const_name: $ =>
      seq(
        repeat(seq($.module_identifier, alias(".", $.symbol))),
        $.constant_identifier,
      ),

    paren_expression: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("expr", $.expression),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),

    //
    // Booleans
    bool_literal: $ => choice(/true/, /false/),

    //
    // Strings
    // TODO: maybe add support for multiline strings (""")
    string_literal: $ =>
      seq(
        field("symbol_open_quote", alias('"', $.symbol)),
        optional(field("content", $.string_content)),
        field("symbol_close_quote", alias('"', $.symbol)),
      ),

    string_content: $ =>
      repeat1(
        choice(
          // higher precedence than escape sequences
          token.immediate(prec(1, /[^\\"\n]+/)),
          $.char_or_string_escape_sequence,
        ),
      ),

    //
    // Characters
    char_literal: $ =>
      seq(
        field("symbol_open_single_quote", alias("'", $.symbol)),
        field("content", $.character),
        field("symbol_close_single_quote", alias("'", $.symbol)),
      ),
    character: $ =>
      choice(
        // higher precedence than escape sequences
        token.immediate(prec(1, /[^'\\\n]/)),
        $.char_or_string_escape_sequence,
      ),

    char_or_string_escape_sequence: _ =>
      token.immediate(seq("\\", /(\"|\\|\/|b|f|n|r|t|u)/)),

    //
    // Infix operations
    infix_operation: $ =>
      // given `1 + 2 * 3`, this will parse as `1 + (2 * 3)`
      choice(
        // Power
        prec.right(
          PREC.EXPONENT,
          seq(
            field("left", $.expression),
            field("operator", alias(exponentOperator, $.operator)),
            field("right", $.expression),
          ),
        ),

        // multiplication, division, modulo
        prec.left(
          PREC.PRODUCT,
          seq(
            field("left", $.expression),
            field("operator", alias(multiplicativeOperators, $.operator)),
            field("right", $.expression),
          ),
        ),

        // addition, subtraction
        prec.left(
          PREC.SUM,
          seq(
            field("left", $.expression),
            field("operator", alias(additiveOperators, $.operator)),
            field("right", $.expression),
          ),
        ),

        // Comparison
        prec.left(
          PREC.COMPARISON,
          seq(
            field("left", $.expression),
            field("operator", alias(comparisonOperators, $.operator)),
            field("right", $.expression),
          ),
        ),

        // Logical operations
        prec.left(
          PREC.LOGICAL_AND,
          seq(
            field("left", $.expression),
            field("operator", alias(logicalOperators, $.operator)),
            field("right", $.expression),
          ),
        ),
        prec.left(
          PREC.LOGICAL_OR,
          seq(
            field("left", $.expression),
            field("operator", alias(logicalOperators, $.operator)),
            field("right", $.expression),
          ),
        ),

        // String concatenation
        prec.left(
          PREC.SUM,
          seq(
            field("left", $.expression),
            field("operator", alias(stringConcatOperator, $.operator)),
            field("right", $.expression),
          ),
        ),
      ),

    //
    // Integers
    //CLEANUP: we are using .NET suffixes for integers (e.g. `1L` for Int64) temporarily until we remove the old parser.
    int8_literal: $ =>
      seq(field("digits", $.digits), field("suffix", alias("y", $.symbol))),

    uint8_literal: $ =>
      seq(
        field("digits", $.positive_digits),
        field("suffix", alias("uy", $.symbol)),
      ),

    int16_literal: $ =>
      seq(field("digits", $.digits), field("suffix", alias("s", $.symbol))),

    uint16_literal: $ =>
      seq(
        field("digits", $.positive_digits),
        field("suffix", alias("us", $.symbol)),
      ),

    int32_literal: $ =>
      seq(field("digits", $.digits), field("suffix", alias("l", $.symbol))),

    uint32_literal: $ =>
      seq(
        field("digits", $.positive_digits),
        field("suffix", alias("ul", $.symbol)),
      ),

    int64_literal: $ =>
      seq(field("digits", $.digits), field("suffix", alias("L", $.symbol))),

    uint64_literal: $ =>
      seq(
        field("digits", $.positive_digits),
        field("suffix", alias("UL", $.symbol)),
      ),

    int128_literal: $ =>
      seq(field("digits", $.digits), field("suffix", alias("Q", $.symbol))),

    uint128_literal: $ =>
      seq(
        field("digits", $.positive_digits),
        field("suffix", alias("Z", $.symbol)),
      ),

    digits: $ => choice($.positive_digits, $.negative_digits),
    negative_digits: $ => /-\d+/,
    positive_digits: $ => /\d+/,

    //
    // Floats
    float_literal: $ => /[+-]?[0-9]+\.[0-9]+/,

    //
    // List
    // TODO: allow multi-line lists where a newline is 'interpreted' as a list delimiter (i.e. no ; needed)
    list_literal: $ => list_literal_base($, $.list_content),
    list_content: $ => list_content_base($, $.expression),

    //
    // Dict
    dict_literal: $ => dict_literal_base($, $.dict_content),
    dict_content: $ => dict_content_base($, $.dict_pair),
    dict_pair: $ => dict_pair_base($, $.expression, $.expression),

    //
    // Tuples
    tuple_literal: $ =>
      tuple_literal_base($, $.expression, $.tuple_literal_the_rest),
    tuple_literal_the_rest: $ => tuple_literal_the_rest_base($, $.expression),

    //
    // Record
    record_literal: $ =>
      seq(
        field("type_name", $.qualified_type_name),
        field("symbol_open_brace", alias("{", $.symbol)),
        optional(field("content", $.record_content)),
        field("symbol_close_brace", alias("}", $.symbol)),
      ),
    record_content: $ =>
      choice(
        seq(
          $.record_pair,
          repeat(
            seq(field("record_separator", alias(";", $.symbol)), $.record_pair),
          ),
        ),
        seq(
          $.record_pair,
          repeat(seq($.newline, $.record_pair)),
          optional($.newline),
        ),
      ),
    record_pair: $ =>
      seq(
        field("field", $.variable_identifier),
        field("symbol_equals", alias("=", $.symbol)),
        field("value", $.expression),
      ),

    //
    // Record update
    // e.g. { RecordForUpdate { x = 4L; y = 1L } with y = 2L }
    record_update: $ =>
      seq(
        field("symbol_open_brace", alias("{", $.symbol)),
        field("record", $.expression),
        field("keyword_with", alias("with", $.keyword)),
        field("field_updates", $.record_update_fields),
        field("symbol_close_brace", alias("}", $.symbol)),
      ),
    record_update_fields: $ =>
      seq(
        $.record_update_field,
        repeat(
          seq(
            field("symbol_semicolon", alias(";", $.symbol)),
            $.record_update_field,
          ),
        ),
      ),
    record_update_field: $ =>
      seq(
        field("field_name", $.variable_identifier),
        field("symbol_equals", alias("=", $.symbol)),
        field("value", $.expression),
      ),

    //
    // Enum
    enum_literal: $ => enum_literal_base($, $.enum_fields),
    enum_fields: $ => enum_fields_base($, $.expression),

    //
    // If expressions
    if_expression: $ =>
      prec.right(
        seq(
          field("keyword_if", alias("if", $.keyword)),
          field("condition", $.expression),
          field("keyword_then", alias("then", $.keyword)),
          choice(
            seq(
              $.indent,
              repeat1(field("then_expression", $.expression)),
              $.dedent,
            ),
            field("then_expression", $.expression),
          ),
          optional(
            seq(
              field("keyword_else", alias("else", $.keyword)),
              choice(
                seq(
                  $.indent,
                  repeat(field("else_expression", $.expression)),
                  $.dedent,
                ),
                field("else_expression", $.expression),
              ),
            ),
          ),
        ),
      ),

    // field access
    // e.g. `person.name`
    field_access: $ =>
      prec(
        PREC.FIELDACCESS,
        seq(
          field(
            "expr",
            choice(
              $.variable_identifier,
              $.record_literal,
              $.paren_expression,
              $.field_access,
            ),
          ),
          field("symbol_dot", alias(".", $.symbol)),
          field("field_name", $.variable_identifier),
        ),
      ),

    //
    // Lambda expressions
    lambda_expression: $ =>
      seq(
        field("keyword_fun", alias("fun", $.keyword)),
        field("pats", $.lambda_pats),
        field("symbol_arrow", alias("->", $.symbol)),
        field("body", $.expression),
      ),
    lambda_pats: $ => field("pat", repeat1($.let_pattern)),

    lp_tuple: $ => tuple_literal_base($, $.let_pattern, $.let_pattern_the_rest),
    let_pattern_the_rest: $ => tuple_literal_the_rest_base($, $.let_pattern),

    let_pattern: $ => choice($.unit, $.lp_tuple, $.variable_identifier),

    //
    // Match expression
    // TODO : allow one-line match expressions (i.e. match x with | 1 -> ...)
    match_expression: $ =>
      seq(
        field("keyword_match", alias("match", $.keyword)),
        field("arg", $.expression),
        field("keyword_with", alias("with", $.keyword)),
        field("cases", prec.left(repeat1($.match_case))),
      ),
    match_case: $ =>
      seq(
        field("symbol_pipe", alias("|", $.symbol)),
        field("pattern", $.match_pattern),
        optional(
          seq(
            field("when_keyword", alias("when", $.keyword)),
            field("guard_expr", $.expression),
          ),
        ),
        field("symbol_arrow", alias("->", $.symbol)),
        field("rhs", $.expression),
      ),

    //
    // Function call
    // e.g. `Int64.add 1 2
    apply: $ =>
      prec.right(
        seq(
          field("fn", $.qualified_fn_name),
          field(
            "args",
            repeat1(choice($.paren_expression, $.simple_expression)),
          ),
          // the new line is used as a delimiter
          optional($.newline),
        ),
      ),

    //
    // Let expression
    let_expression: $ =>
      seq(
        field("keyword_let", alias("let", $.keyword)),
        field("identifier", $.variable_identifier),
        field("symbol_equals", alias("=", $.symbol)),
        field("expr", $.expression),
        "\n",
        field("body", $.expression),
      ),

    // ---------------------
    // Pipe expressions
    // ---------------------

    //
    // Pipe lambda
    pipe_lambda: $ =>
      choice(
        field("pipe_lambda", $.lambda_expression),
        seq(
          field("symbol_open_paren", alias("(", $.symbol)),
          field("pipe_lambda", $.lambda_expression),
          field("symbol_close_paren", alias(")", $.symbol)),
        ),
      ),

    //
    // Pipe infix
    operator: $ =>
      choice(
        exponentOperator,
        multiplicativeOperators,
        additiveOperators,
        comparisonOperators,
        logicalOperators,
        stringConcatOperator,
      ),

    pipe_infix: $ =>
      prec.right(
        50, // this precedence is important to ensure it will parse `1L |> (-) 2L |> (+) 3L` as ((1L |> (-) 2L) |> (+) 3L) and not `(1L |> ((-) 2L |> (+) 3L))`
        seq(
          field("symbol_open_paren", alias("(", $.symbol)),
          field("operator", $.operator),
          field("symbol_close_paren", alias(")", $.symbol)),
          field("right", $.expression),
        ),
      ),

    //
    // Pipe function call
    pipe_fn_call: $ =>
      prec.right(
        seq(
          field("fn", $.qualified_fn_name),
          field(
            "args",
            repeat(choice($.paren_expression, $.simple_expression)),
          ),
        ),
      ),

    //
    // Pipe enum
    pipe_enum: $ => enum_literal_base($, $.pipe_enum_fields),
    pipe_enum_fields: $ => enum_fields_base($, $.expression),

    pipe_expr: $ =>
      choice($.pipe_lambda, $.pipe_infix, $.pipe_fn_call, $.pipe_enum),

    pipe_exprs: $ =>
      prec.left(
        seq(
          field("symbol_pipe", alias("|>", $.symbol)),
          field("pipe_expr", $.pipe_expr),
        ),
      ),

    pipe_expression: $ =>
      prec.left(
        PREC.PIPE_EXPR,
        seq(
          field("expr", $.expression),
          repeat1(field("pipe_exprs", $.pipe_exprs)),
        ),
      ),

    // ---------------------
    // Common
    // ---------------------
    type_reference: $ => choice($.builtin_type, $.qualified_type_name),

    builtin_type: $ =>
      choice(
        /Unit/,
        /Bool/,
        /Int8/,
        /UInt8/,
        /Int16/,
        /UInt16/,
        /Int32/,
        /UInt32/,
        /Int64/,
        /UInt64/,
        /Int128/,
        /UInt128/,
        /Float/,
        /Char/,
        /String/,
        /DateTime/,
        /Uuid/,
        $.list_type_reference,
        $.tuple_type_reference,
        $.dict_type_reference,
        $.fn_type_reference,
        $.variable_type_reference,

        $.db_type_reference,
      ),

    //
    // List<T>
    list_type_reference: $ =>
      seq(
        field("keyword_type_constructor", alias("List", $.keyword)),
        field("symbol_open_angle", alias("<", $.symbol)),
        field("typ_param", $.type_reference),
        field("symbol_close_angle", alias(">", $.symbol)),
      ),

    //
    // Tuple
    tuple_type_reference: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("first", $.type_reference),
        field("symbol_asterisk", alias("*", $.symbol)),
        field("second", $.type_reference),
        optional(field("rest", $.type_reference_tuple_the_rest)),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),
    type_reference_tuple_the_rest: $ =>
      repeat1(
        seq(
          field("symbol_asterisk", alias("*", $.symbol)),
          field("type", $.type_reference),
        ),
      ),

    //
    // Dict<T>
    dict_type_reference: $ =>
      seq(
        field("keyword_type_constructor", alias("Dict", $.keyword)),
        field("symbol_open_angle", alias("<", $.symbol)),
        field("value_type", $.type_reference),
        field("symbol_close_angle", alias(">", $.symbol)),
      ),

    //
    // Function type reference
    //  'a -> 'b -> 'c
    fn_type_reference: $ =>
      prec.right(
        2,
        seq(
          repeat1(
            prec.left(
              1,
              seq(
                $.type_reference,
                field("symbol_arrow", alias("->", $.symbol)),
              ),
            ),
          ),
          $.type_reference,
        ),
      ),

    // DB type reference
    // DB<'a>
    db_type_reference: $ =>
      seq(
        field("keyword_type_constructor", alias("DB", $.keyword)),
        field("symbol_open_angle", alias("<", $.symbol)),
        field("typ_param", $.type_reference),
        field("symbol_close_angle", alias(">", $.symbol)),
      ),

    //
    // Variable type reference
    // 'a
    variable_type_reference: $ =>
      seq(
        field("symbol_single_quote", alias("'", $.symbol)),
        field("variable", $.variable_identifier),
      ),

    // ---------------------
    // Identifiers
    // ---------------------
    qualified_fn_name: $ =>
      seq(
        seq(
          repeat(seq($.module_identifier, alias(".", $.symbol))),
          $.fn_identifier,
        ),
        optional(field("type_args", $.type_args)),
      ),

    qualified_type_name: $ =>
      seq(
        seq(
          repeat(seq($.module_identifier, alias(".", $.symbol))),
          field("type_identifier", $.type_identifier),
        ),
        optional(field("type_args", $.type_args)),
      ),

    type_args: $ =>
      seq(
        field("symbol_open_angle", alias(token.immediate("<"), $.symbol)),
        field("type_args_items", $.type_args_items),
        field("symbol_close_angle", alias(token.immediate(">"), $.symbol)),
      ),
    type_args_items: $ =>
      seq(
        $.type_reference,
        repeat(
          seq(field("symbol_comma", alias(",", $.symbol)), $.type_reference),
        ),
      ),

    // e.g. `newline` in `const newline = '\n'`
    constant_identifier: $ => /[a-z_][a-zA-Z0-9_]*/,

    /** e.g. `x` in `let double (x: Int) = x + x`
     *
     * for let bindings, params, etc. */
    variable_identifier: $ => prec(PREC.VAR_IDENTIFIER, /[a-z_][a-zA-Z0-9_]*/),

    // e.g. `double` in `let double (x: Int) = x + x`
    fn_identifier: $ => prec(PREC.FN_IDENTIFIER, /[a-z_][a-zA-Z0-9_]*/),

    // e.g. `Person` in `type MyPerson = ...`
    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,

    // e.g. `LanguageTools in `PACKAGE.Darklang.LanguageTools.SomeType`
    module_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,

    //
    enum_case_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,

    newline: $ => /\n/,

    unit: $ => "()",
  },
});

function list_literal_base($, contentRule) {
  return seq(
    field("symbol_open_bracket", alias("[", $.symbol)),
    optional(field("content", contentRule)),
    field("symbol_close_bracket", alias("]", $.symbol)),
  );
}
function list_content_base($, content) {
  return choice(
    seq(
      content,
      repeat(seq(field("list_separator", alias(";", $.symbol)), content)),
      optional(alias(";", $.symbol)),
    ),
    seq(content, repeat(seq($.newline, content)), optional($.newline)),
  );
}

function tuple_literal_base($, firstOrSecond, rest) {
  return seq(
    field("symbol_left_paren", alias("(", $.symbol)),
    field("first", firstOrSecond),
    field("symbol_comma", alias(",", $.symbol)),
    field("second", firstOrSecond),
    optional(field("rest", rest)),
    field("symbol_right_paren", alias(")", $.symbol)),
  );
}
function tuple_literal_the_rest_base($, rest) {
  return repeat1(
    seq(field("symbol_comma", alias(",", $.symbol)), field("expr", rest)),
  );
}

function dict_literal_base($, contentRule) {
  return seq(
    field("keyword_dict", alias("Dict", $.keyword)),
    field("symbol_open_brace", alias("{", $.symbol)),
    optional(field("content", contentRule)),
    field("symbol_close_brace", alias("}", $.symbol)),
  );
}
function dict_content_base($, dict_pair) {
  return seq(
    dict_pair,
    repeat(seq(field("dict_separator", alias(";", $.symbol)), dict_pair)),
  );
}
function dict_pair_base($, key, value) {
  return seq(
    field("key", key),
    field("symbol_equals", alias("=", $.symbol)),
    field("value", value),
  );
}

function enum_literal_base($, enum_fields) {
  return prec.right(
    seq(
      field("type_name", $.qualified_type_name),
      field("symbol_dot", alias(".", $.symbol)),
      field("case_name", $.enum_case_identifier),
      optional(field("enum_fields", enum_fields)),
    ),
  );
}
function enum_fields_base($, fields) {
  return prec(
    1,
    choice(
      seq(
        field("symbol_open_paren", alias("(", $.symbol)),
        seq(
          fields,
          repeat(
            prec.right(
              seq(field("symbol_comma", alias(",", $.symbol)), fields),
            ),
          ),
        ),
        field("symbol_close_paren", alias(")", $.symbol)),
      ),
      prec.right(repeat1(prec.right(fields))),
    ),
  );
}
