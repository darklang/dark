// TODOs:
// - int64 literals (e.g. `0L`)
// - deal with keywords
// - better support for partially-written code (i.e. `let x =`)

const PREC = {
  LOGICAL_OR: 0,
  LOGICAL_AND: 1,
  COMPARISON: 2,
  SUM: 3,
  PRODUCT: 4,
  EXPONENT: 5,
  MATCH_EXPR: 6,
  FIELDACCESS: 7,
  VAR_IDENTIFIER: 8,
  FN_IDENTIFIER: 9,
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
        // all type and fn defs first
        repeat(choice($.type_decl, $.fn_decl)),

        // then the expressions to evaluate, in order
        repeat($.expression),
      ),

    // ---------------------
    // Function declarations
    // ---------------------
    fn_decl: $ =>
      seq(
        field("keyword_let", alias("let", $.keyword)),
        field("name", $.fn_identifier),
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
        field("symbol_equals", alias("=", $.symbol)),
        field("typ", $.type_decl_def),
      ),

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
    type_decl_def_record: $ =>
      seq(
        field("symbol_open_brace", alias("{", $.symbol)),
        field("fields", optional($.type_decl_def_record_fields)),
        field("symbol_close_brace", alias("}", $.symbol)),
      ),

    type_decl_def_record_fields: $ =>
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

    type_decl_def_record_field: $ =>
      seq(
        field("field_name", $.variable_identifier),
        field("symbol_colon", alias(":", $.symbol)),
        field("type", $.type_reference),
      ),

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

    newline: $ => /\n/,

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
    mp_list: $ =>
      seq(
        field("symbol_open_bracket", alias("[", $.symbol)),
        field("content", optional($.mp_list_content)),
        field("symbol_close_bracket", alias("]", $.symbol)),
      ),

    mp_list_content: $ =>
      seq(
        $.match_pattern,
        repeat(
          seq(field("list_separator", alias(";", $.symbol)), $.match_pattern),
        ),
        optional(alias(";", $.symbol)),
      ),

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
    mp_tuple: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("first", $.match_pattern),
        field("symbol_comma", alias(",", $.symbol)),
        field("second", $.match_pattern),
        field("rest", optional($.mp_tuple_the_rest)),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),

    mp_tuple_the_rest: $ =>
      repeat1(
        seq(
          field("symbol_comma", alias(",", $.symbol)),
          field("pat", $.match_pattern),
        ),
      ),

    //
    // match pattern - enum
    mp_enum: $ =>
      prec.right(
        seq(
          field("case_name", $.enum_case_identifier),
          optional(
            seq(
              field("symbol_open_paren", alias("(", $.symbol)),
              field("enum_fields", $.mp_enum_fields),
              field("symbol_close_paren", alias(")", $.symbol)),
            ),
          ),
        ),
      ),

    mp_enum_fields: $ =>
      seq(
        $.match_pattern,
        repeat(
          seq(field("symbol_comma", alias(",", $.symbol)), $.match_pattern),
        ),
      ),

    // ---------------------
    // Expressions
    // ---------------------
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
        $.tuple_literal,
        $.dict_literal,
        $.record_literal,
        $.enum_literal,
        $.variable_identifier,
      ),

    expression: $ =>
      choice(
        $.paren_expression,
        $.simple_expression,
        $.if_expression,
        $.let_expression,

        $.match_expression,

        $.infix_operation,
        $.function_call,

        $.field_access,
        $.lambda_expression,

        $.record_update,
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
      choice(
        seq(
          field("symbol_open_quote", alias('"', $.symbol)),
          field("symbol_close_quote", alias('"', $.symbol)),
        ),
        seq(
          field("symbol_open_quote", alias('"', $.symbol)),
          field("content", $.string_content),
          field("symbol_close_quote", alias('"', $.symbol)),
        ),
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
    list_literal: $ =>
      seq(
        field("symbol_open_bracket", alias("[", $.symbol)),
        field("content", optional($.list_content)),
        field("symbol_close_bracket", alias("]", $.symbol)),
      ),

    list_content: $ =>
      seq(
        $.expression,
        repeat(
          seq(field("list_separator", alias(";", $.symbol)), $.expression),
        ),
        optional(alias(";", $.symbol)),
      ),

    //
    // Dict
    dict_literal: $ =>
      seq(
        field("keyword_dict", alias("Dict", $.keyword)),
        field("symbol_open_brace", alias("{", $.symbol)),
        field("content", optional($.dict_content)),
        field("symbol_close_brace", alias("}", $.symbol)),
      ),

    dict_content: $ =>
      seq(
        $.dict_pair,
        repeat(seq(field("dict_separator", alias(";", $.symbol)), $.dict_pair)),
      ),

    dict_pair: $ =>
      seq(
        field("key", $.expression),
        field("symbol_equals", alias("=", $.symbol)),
        field("value", $.expression),
      ),

    //
    // Tuples
    tuple_literal: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("first", $.expression),
        field("symbol_comma", alias(",", $.symbol)),
        field("second", $.expression),
        field("rest", optional($.tuple_literal_the_rest)),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),

    tuple_literal_the_rest: $ =>
      repeat1(
        seq(
          field("symbol_comma", alias(",", $.symbol)),
          field("expr", $.expression),
        ),
      ),

    //
    // Record
    // TODO: allow multi-line records where a newline is 'interpreted' as a record delimiter (i.e. no ; needed)
    record_literal: $ =>
      seq(
        field("type_name", $.qualified_type_name),
        field("symbol_open_brace", alias("{", $.symbol)),
        field("content", optional($.record_content)),
        field("symbol_close_brace", alias("}", $.symbol)),
      ),

    record_content: $ =>
      seq(
        $.record_pair,
        repeat(
          seq(field("record_separator", alias(";", $.symbol)), $.record_pair),
        ),
      ),

    record_pair: $ =>
      seq(
        field("field", $.variable_identifier),
        field("symbol_equals", alias("=", $.symbol)),
        field("value", $.expression),
      ),

    //
    // Enum
    // TODO: Make parentheses optional when there's only one argument
    enum_literal: $ =>
      prec.right(
        seq(
          field("type_name", $.qualified_type_name),
          field("symbol_dot", alias(".", $.symbol)),
          field("case_name", $.enum_case_identifier),
          optional(
            seq(
              field("symbol_open_paren", alias("(", $.symbol)),
              field("enum_fields", $.enum_fields),
              field("symbol_close_paren", alias(")", $.symbol)),
            ),
          ),
        ),
      ),

    enum_fields: $ =>
      seq(
        $.expression,
        repeat(seq(field("symbol_comma", alias(",", $.symbol)), $.expression)),
      ),

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
          field("expr", $.expression),
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

    lp_tuple: $ =>
      seq(
        field("symbol_left_paren", alias("(", $.symbol)),
        field("first", $.let_pattern),
        field("symbol_comma", alias(",", $.symbol)),
        field("second", $.let_pattern),
        field(
          "rest",
          repeat(
            seq(field("symbol_comma", alias(",", $.symbol)), $.let_pattern),
          ),
        ),
        field("symbol_right_paren", alias(")", $.symbol)),
      ),

    let_pattern: $ => choice($.unit, $.lp_tuple, $.variable_identifier),

    //
    // Match expression
    // TODO : allow one-line match expressions (i.e. match x with | 1 -> ...)
    match_expression: $ =>
      prec(
        PREC.MATCH_EXPR,
        seq(
          field("keyword_match", alias("match", $.keyword)),
          field("arg", $.expression),
          field("keyword_with", alias("with", $.keyword)),
          field("cases", repeat1($.match_case)),
        ),
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

    /**
     * e.g. `Int64.add 1 2
     *
     * This is currently a bit hacky, requiring parens around the function call,
     * in order to help tree-sitter parse it correctly.
     *
     * There's certainly a better way to remove ambiguities. TODO
     *
     * TODO maybe call this "apply" instead
     *      sometimes the thing we are 'applying' is not directly a function
     */
    function_call: $ =>
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
        $.list_type_reference,
        $.tuple_type_reference,
        $.dict_type_reference,
        /DateTime/,
        /Uuid/,
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
        field("rest", optional($.type_reference_tuple_the_rest)),
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

    // ---------------------
    // Identifiers
    // ---------------------
    qualified_fn_name: $ =>
      seq(
        repeat(seq($.module_identifier, alias(".", $.symbol))),
        $.fn_identifier,
      ),

    qualified_type_name: $ =>
      seq(
        repeat(seq($.module_identifier, alias(".", $.symbol))),
        $.type_identifier,
      ),

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

    unit: $ => "()",
  },
});
