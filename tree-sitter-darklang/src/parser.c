#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 140
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 54
#define ALIAS_COUNT 0
#define TOKEN_COUNT 27
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 23
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 11

enum {
  anon_sym_let = 1,
  anon_sym_COLON = 2,
  anon_sym_EQ = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_type = 6,
  aux_sym_bool_literal_token1 = 7,
  aux_sym_bool_literal_token2 = 8,
  anon_sym_LF = 9,
  anon_sym_DQUOTE = 10,
  aux_sym_string_content_token1 = 11,
  sym_string_escape_sequence = 12,
  anon_sym_PLUS = 13,
  anon_sym_DASH = 14,
  anon_sym_L = 15,
  sym_digits = 16,
  aux_sym_builtin_type_token1 = 17,
  aux_sym_builtin_type_token2 = 18,
  aux_sym_builtin_type_token3 = 19,
  aux_sym_builtin_type_token4 = 20,
  aux_sym_builtin_type_token5 = 21,
  aux_sym_builtin_type_token6 = 22,
  anon_sym_DOT = 23,
  aux_sym_variable_identifier_token1 = 24,
  aux_sym_type_identifier_token1 = 25,
  sym_unit = 26,
  sym_source_file = 27,
  sym_fn_decl = 28,
  sym_fn_decl_params = 29,
  sym_fn_decl_param = 30,
  sym_type_decl = 31,
  sym_expression = 32,
  sym_paren_expression = 33,
  sym_bool_literal = 34,
  sym_function_call = 35,
  sym_let_expression = 36,
  sym_string_literal = 37,
  sym_string_content = 38,
  sym_infix_operation = 39,
  sym_int64_literal = 40,
  sym_type_reference = 41,
  sym_builtin_type = 42,
  sym_qualified_fn_name = 43,
  sym_qualified_type_name = 44,
  sym_variable_identifier = 45,
  sym_fn_identifier = 46,
  sym_type_identifier = 47,
  sym_module_identifier = 48,
  aux_sym_source_file_repeat1 = 49,
  aux_sym_source_file_repeat2 = 50,
  aux_sym_fn_decl_params_repeat1 = 51,
  aux_sym_string_content_repeat1 = 52,
  aux_sym_qualified_fn_name_repeat1 = 53,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_let] = "keyword",
  [anon_sym_COLON] = "symbol",
  [anon_sym_EQ] = "symbol",
  [anon_sym_LPAREN] = "symbol",
  [anon_sym_RPAREN] = "symbol",
  [anon_sym_type] = "keyword",
  [aux_sym_bool_literal_token1] = "bool_literal_token1",
  [aux_sym_bool_literal_token2] = "bool_literal_token2",
  [anon_sym_LF] = "\n",
  [anon_sym_DQUOTE] = "symbol",
  [aux_sym_string_content_token1] = "string_content_token1",
  [sym_string_escape_sequence] = "string_escape_sequence",
  [anon_sym_PLUS] = "operator",
  [anon_sym_DASH] = "operator",
  [anon_sym_L] = "symbol",
  [sym_digits] = "digits",
  [aux_sym_builtin_type_token1] = "builtin_type_token1",
  [aux_sym_builtin_type_token2] = "builtin_type_token2",
  [aux_sym_builtin_type_token3] = "builtin_type_token3",
  [aux_sym_builtin_type_token4] = "builtin_type_token4",
  [aux_sym_builtin_type_token5] = "builtin_type_token5",
  [aux_sym_builtin_type_token6] = "builtin_type_token6",
  [anon_sym_DOT] = "symbol",
  [aux_sym_variable_identifier_token1] = "variable_identifier_token1",
  [aux_sym_type_identifier_token1] = "type_identifier_token1",
  [sym_unit] = "unit",
  [sym_source_file] = "source_file",
  [sym_fn_decl] = "fn_decl",
  [sym_fn_decl_params] = "fn_decl_params",
  [sym_fn_decl_param] = "fn_decl_param",
  [sym_type_decl] = "type_decl",
  [sym_expression] = "expression",
  [sym_paren_expression] = "paren_expression",
  [sym_bool_literal] = "bool_literal",
  [sym_function_call] = "function_call",
  [sym_let_expression] = "let_expression",
  [sym_string_literal] = "string_literal",
  [sym_string_content] = "string_content",
  [sym_infix_operation] = "infix_operation",
  [sym_int64_literal] = "int64_literal",
  [sym_type_reference] = "type_reference",
  [sym_builtin_type] = "builtin_type",
  [sym_qualified_fn_name] = "qualified_fn_name",
  [sym_qualified_type_name] = "qualified_type_name",
  [sym_variable_identifier] = "variable_identifier",
  [sym_fn_identifier] = "fn_identifier",
  [sym_type_identifier] = "type_identifier",
  [sym_module_identifier] = "module_identifier",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_source_file_repeat2] = "source_file_repeat2",
  [aux_sym_fn_decl_params_repeat1] = "fn_decl_params_repeat1",
  [aux_sym_string_content_repeat1] = "string_content_repeat1",
  [aux_sym_qualified_fn_name_repeat1] = "qualified_fn_name_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_EQ] = anon_sym_COLON,
  [anon_sym_LPAREN] = anon_sym_COLON,
  [anon_sym_RPAREN] = anon_sym_COLON,
  [anon_sym_type] = anon_sym_let,
  [aux_sym_bool_literal_token1] = aux_sym_bool_literal_token1,
  [aux_sym_bool_literal_token2] = aux_sym_bool_literal_token2,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_DQUOTE] = anon_sym_COLON,
  [aux_sym_string_content_token1] = aux_sym_string_content_token1,
  [sym_string_escape_sequence] = sym_string_escape_sequence,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_PLUS,
  [anon_sym_L] = anon_sym_COLON,
  [sym_digits] = sym_digits,
  [aux_sym_builtin_type_token1] = aux_sym_builtin_type_token1,
  [aux_sym_builtin_type_token2] = aux_sym_builtin_type_token2,
  [aux_sym_builtin_type_token3] = aux_sym_builtin_type_token3,
  [aux_sym_builtin_type_token4] = aux_sym_builtin_type_token4,
  [aux_sym_builtin_type_token5] = aux_sym_builtin_type_token5,
  [aux_sym_builtin_type_token6] = aux_sym_builtin_type_token6,
  [anon_sym_DOT] = anon_sym_COLON,
  [aux_sym_variable_identifier_token1] = aux_sym_variable_identifier_token1,
  [aux_sym_type_identifier_token1] = aux_sym_type_identifier_token1,
  [sym_unit] = sym_unit,
  [sym_source_file] = sym_source_file,
  [sym_fn_decl] = sym_fn_decl,
  [sym_fn_decl_params] = sym_fn_decl_params,
  [sym_fn_decl_param] = sym_fn_decl_param,
  [sym_type_decl] = sym_type_decl,
  [sym_expression] = sym_expression,
  [sym_paren_expression] = sym_paren_expression,
  [sym_bool_literal] = sym_bool_literal,
  [sym_function_call] = sym_function_call,
  [sym_let_expression] = sym_let_expression,
  [sym_string_literal] = sym_string_literal,
  [sym_string_content] = sym_string_content,
  [sym_infix_operation] = sym_infix_operation,
  [sym_int64_literal] = sym_int64_literal,
  [sym_type_reference] = sym_type_reference,
  [sym_builtin_type] = sym_builtin_type,
  [sym_qualified_fn_name] = sym_qualified_fn_name,
  [sym_qualified_type_name] = sym_qualified_type_name,
  [sym_variable_identifier] = sym_variable_identifier,
  [sym_fn_identifier] = sym_fn_identifier,
  [sym_type_identifier] = sym_type_identifier,
  [sym_module_identifier] = sym_module_identifier,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_source_file_repeat2] = aux_sym_source_file_repeat2,
  [aux_sym_fn_decl_params_repeat1] = aux_sym_fn_decl_params_repeat1,
  [aux_sym_string_content_repeat1] = aux_sym_string_content_repeat1,
  [aux_sym_qualified_fn_name_repeat1] = aux_sym_qualified_fn_name_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_bool_literal_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_bool_literal_token2] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_string_content_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_string_escape_sequence] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_L] = {
    .visible = true,
    .named = true,
  },
  [sym_digits] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_builtin_type_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token4] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token5] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token6] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_variable_identifier_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_identifier_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_unit] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_decl] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_decl_params] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_decl_param] = {
    .visible = true,
    .named = true,
  },
  [sym_type_decl] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_paren_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_bool_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_function_call] = {
    .visible = true,
    .named = true,
  },
  [sym_let_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_string_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_string_content] = {
    .visible = true,
    .named = true,
  },
  [sym_infix_operation] = {
    .visible = true,
    .named = true,
  },
  [sym_int64_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_type_reference] = {
    .visible = true,
    .named = true,
  },
  [sym_builtin_type] = {
    .visible = true,
    .named = true,
  },
  [sym_qualified_fn_name] = {
    .visible = true,
    .named = true,
  },
  [sym_qualified_type_name] = {
    .visible = true,
    .named = true,
  },
  [sym_variable_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_type_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_module_identifier] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_source_file_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_fn_decl_params_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_content_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_qualified_fn_name_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_args = 1,
  field_body = 2,
  field_content = 3,
  field_digits = 4,
  field_expr = 5,
  field_fn = 6,
  field_identifier = 7,
  field_keyword_let = 8,
  field_keyword_type = 9,
  field_left = 10,
  field_name = 11,
  field_operator = 12,
  field_params = 13,
  field_return_type = 14,
  field_right = 15,
  field_suffix = 16,
  field_symbol_close_quote = 17,
  field_symbol_colon = 18,
  field_symbol_equals = 19,
  field_symbol_left_paren = 20,
  field_symbol_open_quote = 21,
  field_symbol_right_paren = 22,
  field_typ = 23,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_args] = "args",
  [field_body] = "body",
  [field_content] = "content",
  [field_digits] = "digits",
  [field_expr] = "expr",
  [field_fn] = "fn",
  [field_identifier] = "identifier",
  [field_keyword_let] = "keyword_let",
  [field_keyword_type] = "keyword_type",
  [field_left] = "left",
  [field_name] = "name",
  [field_operator] = "operator",
  [field_params] = "params",
  [field_return_type] = "return_type",
  [field_right] = "right",
  [field_suffix] = "suffix",
  [field_symbol_close_quote] = "symbol_close_quote",
  [field_symbol_colon] = "symbol_colon",
  [field_symbol_equals] = "symbol_equals",
  [field_symbol_left_paren] = "symbol_left_paren",
  [field_symbol_open_quote] = "symbol_open_quote",
  [field_symbol_right_paren] = "symbol_right_paren",
  [field_typ] = "typ",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 2},
  [3] = {.index = 4, .length = 3},
  [4] = {.index = 7, .length = 3},
  [5] = {.index = 10, .length = 3},
  [6] = {.index = 13, .length = 4},
  [7] = {.index = 17, .length = 4},
  [8] = {.index = 21, .length = 5},
  [9] = {.index = 26, .length = 5},
  [10] = {.index = 31, .length = 7},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_symbol_close_quote, 1},
    {field_symbol_open_quote, 0},
  [2] =
    {field_digits, 0},
    {field_suffix, 1},
  [4] =
    {field_expr, 1},
    {field_symbol_left_paren, 0},
    {field_symbol_right_paren, 2},
  [7] =
    {field_content, 1},
    {field_symbol_close_quote, 2},
    {field_symbol_open_quote, 0},
  [10] =
    {field_left, 0},
    {field_operator, 1},
    {field_right, 2},
  [13] =
    {field_args, 2},
    {field_fn, 1},
    {field_symbol_left_paren, 0},
    {field_symbol_right_paren, 3},
  [17] =
    {field_keyword_type, 0},
    {field_name, 1},
    {field_symbol_equals, 2},
    {field_typ, 3},
  [21] =
    {field_body, 5},
    {field_expr, 3},
    {field_identifier, 1},
    {field_keyword_let, 0},
    {field_symbol_equals, 2},
  [26] =
    {field_identifier, 1},
    {field_symbol_colon, 2},
    {field_symbol_left_paren, 0},
    {field_symbol_right_paren, 4},
    {field_typ, 3},
  [31] =
    {field_body, 6},
    {field_keyword_let, 0},
    {field_name, 1},
    {field_params, 2},
    {field_return_type, 4},
    {field_symbol_colon, 3},
    {field_symbol_equals, 5},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 3,
  [5] = 3,
  [6] = 3,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 7,
  [11] = 9,
  [12] = 9,
  [13] = 9,
  [14] = 14,
  [15] = 15,
  [16] = 15,
  [17] = 15,
  [18] = 15,
  [19] = 19,
  [20] = 19,
  [21] = 21,
  [22] = 22,
  [23] = 21,
  [24] = 19,
  [25] = 21,
  [26] = 22,
  [27] = 21,
  [28] = 28,
  [29] = 22,
  [30] = 22,
  [31] = 19,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 32,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 42,
  [44] = 44,
  [45] = 45,
  [46] = 45,
  [47] = 47,
  [48] = 48,
  [49] = 47,
  [50] = 50,
  [51] = 44,
  [52] = 52,
  [53] = 50,
  [54] = 54,
  [55] = 48,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 52,
  [60] = 60,
  [61] = 40,
  [62] = 41,
  [63] = 32,
  [64] = 47,
  [65] = 65,
  [66] = 42,
  [67] = 41,
  [68] = 65,
  [69] = 48,
  [70] = 40,
  [71] = 50,
  [72] = 52,
  [73] = 44,
  [74] = 45,
  [75] = 75,
  [76] = 76,
  [77] = 77,
  [78] = 78,
  [79] = 79,
  [80] = 80,
  [81] = 81,
  [82] = 81,
  [83] = 81,
  [84] = 84,
  [85] = 85,
  [86] = 81,
  [87] = 87,
  [88] = 88,
  [89] = 89,
  [90] = 90,
  [91] = 40,
  [92] = 45,
  [93] = 47,
  [94] = 50,
  [95] = 52,
  [96] = 96,
  [97] = 97,
  [98] = 98,
  [99] = 44,
  [100] = 96,
  [101] = 98,
  [102] = 48,
  [103] = 41,
  [104] = 98,
  [105] = 96,
  [106] = 32,
  [107] = 98,
  [108] = 108,
  [109] = 96,
  [110] = 42,
  [111] = 111,
  [112] = 112,
  [113] = 113,
  [114] = 113,
  [115] = 113,
  [116] = 116,
  [117] = 113,
  [118] = 118,
  [119] = 119,
  [120] = 120,
  [121] = 121,
  [122] = 119,
  [123] = 123,
  [124] = 124,
  [125] = 125,
  [126] = 120,
  [127] = 119,
  [128] = 128,
  [129] = 129,
  [130] = 130,
  [131] = 120,
  [132] = 132,
  [133] = 133,
  [134] = 128,
  [135] = 119,
  [136] = 128,
  [137] = 120,
  [138] = 128,
  [139] = 139,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(31);
      if (lookahead == '"') ADVANCE(41);
      if (lookahead == '(') ADVANCE(35);
      if (lookahead == ')') ADVANCE(36);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == '-') ADVANCE(46);
      if (lookahead == '.') ADVANCE(61);
      if (lookahead == ':') ADVANCE(33);
      if (lookahead == '=') ADVANCE(34);
      if (lookahead == 'B') ADVANCE(18);
      if (lookahead == 'C') ADVANCE(10);
      if (lookahead == 'F') ADVANCE(14);
      if (lookahead == 'I') ADVANCE(17);
      if (lookahead == 'L') ADVANCE(47);
      if (lookahead == 'S') ADVANCE(23);
      if (lookahead == 'U') ADVANCE(15);
      if (lookahead == '\\') ADVANCE(28);
      if (lookahead == 'f') ADVANCE(62);
      if (lookahead == 'l') ADVANCE(63);
      if (lookahead == 't') ADVANCE(69);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(29)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(48);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 1:
      if (lookahead == '\n') SKIP(3)
      if (lookahead == '"') ADVANCE(41);
      if (lookahead == '\\') ADVANCE(28);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(42);
      if (lookahead != 0) ADVANCE(43);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(40);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == '-') ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(41);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      END_STATE();
    case 4:
      if (lookahead == '4') ADVANCE(53);
      END_STATE();
    case 5:
      if (lookahead == '6') ADVANCE(4);
      END_STATE();
    case 6:
      if (lookahead == 'B') ADVANCE(88);
      if (lookahead == 'C') ADVANCE(80);
      if (lookahead == 'F') ADVANCE(84);
      if (lookahead == 'I') ADVANCE(87);
      if (lookahead == 'S') ADVANCE(93);
      if (lookahead == 'U') ADVANCE(85);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(6)
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(97);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 7:
      if (lookahead == 'a') ADVANCE(21);
      END_STATE();
    case 8:
      if (lookahead == 'a') ADVANCE(26);
      END_STATE();
    case 9:
      if (lookahead == 'g') ADVANCE(59);
      END_STATE();
    case 10:
      if (lookahead == 'h') ADVANCE(7);
      END_STATE();
    case 11:
      if (lookahead == 'i') ADVANCE(16);
      END_STATE();
    case 12:
      if (lookahead == 'i') ADVANCE(25);
      END_STATE();
    case 13:
      if (lookahead == 'l') ADVANCE(51);
      END_STATE();
    case 14:
      if (lookahead == 'l') ADVANCE(20);
      END_STATE();
    case 15:
      if (lookahead == 'n') ADVANCE(12);
      END_STATE();
    case 16:
      if (lookahead == 'n') ADVANCE(9);
      END_STATE();
    case 17:
      if (lookahead == 'n') ADVANCE(24);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(19);
      END_STATE();
    case 19:
      if (lookahead == 'o') ADVANCE(13);
      END_STATE();
    case 20:
      if (lookahead == 'o') ADVANCE(8);
      END_STATE();
    case 21:
      if (lookahead == 'r') ADVANCE(57);
      END_STATE();
    case 22:
      if (lookahead == 'r') ADVANCE(11);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(22);
      END_STATE();
    case 24:
      if (lookahead == 't') ADVANCE(5);
      END_STATE();
    case 25:
      if (lookahead == 't') ADVANCE(49);
      END_STATE();
    case 26:
      if (lookahead == 't') ADVANCE(55);
      END_STATE();
    case 27:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(27)
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(97);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 28:
      if (lookahead == '"' ||
          lookahead == '/' ||
          lookahead == '\\' ||
          lookahead == 'b' ||
          lookahead == 'f' ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't' ||
          lookahead == 'u') ADVANCE(44);
      END_STATE();
    case 29:
      if (eof) ADVANCE(31);
      if (lookahead == '"') ADVANCE(41);
      if (lookahead == '(') ADVANCE(35);
      if (lookahead == ')') ADVANCE(36);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == '-') ADVANCE(46);
      if (lookahead == '.') ADVANCE(61);
      if (lookahead == ':') ADVANCE(33);
      if (lookahead == '=') ADVANCE(34);
      if (lookahead == 'B') ADVANCE(18);
      if (lookahead == 'C') ADVANCE(10);
      if (lookahead == 'F') ADVANCE(14);
      if (lookahead == 'I') ADVANCE(17);
      if (lookahead == 'L') ADVANCE(47);
      if (lookahead == 'S') ADVANCE(23);
      if (lookahead == 'U') ADVANCE(15);
      if (lookahead == 'f') ADVANCE(62);
      if (lookahead == 'l') ADVANCE(63);
      if (lookahead == 't') ADVANCE(69);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(29)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(48);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 30:
      if (eof) ADVANCE(31);
      if (lookahead == '"') ADVANCE(41);
      if (lookahead == '(') ADVANCE(35);
      if (lookahead == ')') ADVANCE(36);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == '-') ADVANCE(46);
      if (lookahead == ':') ADVANCE(33);
      if (lookahead == '=') ADVANCE(34);
      if (lookahead == 'f') ADVANCE(62);
      if (lookahead == 'l') ADVANCE(63);
      if (lookahead == 't') ADVANCE(70);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(30)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(48);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(97);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')') ADVANCE(98);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_type);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(aux_sym_bool_literal_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(aux_sym_bool_literal_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(40);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(aux_sym_string_content_token1);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(42);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(aux_sym_string_content_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym_string_escape_sequence);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_L);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_digits);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(48);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(aux_sym_builtin_type_token1);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(aux_sym_builtin_type_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(aux_sym_builtin_type_token2);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(aux_sym_builtin_type_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(aux_sym_builtin_type_token3);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(aux_sym_builtin_type_token3);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(aux_sym_builtin_type_token4);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(aux_sym_builtin_type_token4);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(aux_sym_builtin_type_token5);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(aux_sym_builtin_type_token5);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(aux_sym_builtin_type_token6);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(aux_sym_builtin_type_token6);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'a') ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(38);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(37);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(39);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'l') ADVANCE(71);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'p') ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'r') ADVANCE(73);
      if (lookahead == 'y') ADVANCE(68);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'r') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 's') ADVANCE(66);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 't') ADVANCE(32);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'u') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '4') ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '6') ADVANCE(75);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'a') ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'a') ADVANCE(96);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'g') ADVANCE(60);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'h') ADVANCE(77);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(86);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(95);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'l') ADVANCE(52);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'l') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(79);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(83);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(78);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'r') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'r') ADVANCE(81);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(76);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(50);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_unit);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 30},
  [4] = {.lex_state = 30},
  [5] = {.lex_state = 30},
  [6] = {.lex_state = 30},
  [7] = {.lex_state = 30},
  [8] = {.lex_state = 30},
  [9] = {.lex_state = 30},
  [10] = {.lex_state = 30},
  [11] = {.lex_state = 30},
  [12] = {.lex_state = 30},
  [13] = {.lex_state = 30},
  [14] = {.lex_state = 30},
  [15] = {.lex_state = 30},
  [16] = {.lex_state = 30},
  [17] = {.lex_state = 30},
  [18] = {.lex_state = 30},
  [19] = {.lex_state = 30},
  [20] = {.lex_state = 30},
  [21] = {.lex_state = 30},
  [22] = {.lex_state = 30},
  [23] = {.lex_state = 30},
  [24] = {.lex_state = 30},
  [25] = {.lex_state = 30},
  [26] = {.lex_state = 30},
  [27] = {.lex_state = 30},
  [28] = {.lex_state = 30},
  [29] = {.lex_state = 30},
  [30] = {.lex_state = 30},
  [31] = {.lex_state = 30},
  [32] = {.lex_state = 30},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 6},
  [35] = {.lex_state = 6},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 6},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 30},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 30},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 30},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 30},
  [48] = {.lex_state = 30},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 30},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 30},
  [54] = {.lex_state = 0},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 0},
  [57] = {.lex_state = 0},
  [58] = {.lex_state = 0},
  [59] = {.lex_state = 30},
  [60] = {.lex_state = 30},
  [61] = {.lex_state = 0},
  [62] = {.lex_state = 30},
  [63] = {.lex_state = 30},
  [64] = {.lex_state = 30},
  [65] = {.lex_state = 30},
  [66] = {.lex_state = 30},
  [67] = {.lex_state = 30},
  [68] = {.lex_state = 30},
  [69] = {.lex_state = 30},
  [70] = {.lex_state = 30},
  [71] = {.lex_state = 30},
  [72] = {.lex_state = 30},
  [73] = {.lex_state = 30},
  [74] = {.lex_state = 30},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 30},
  [77] = {.lex_state = 30},
  [78] = {.lex_state = 30},
  [79] = {.lex_state = 0},
  [80] = {.lex_state = 0},
  [81] = {.lex_state = 1},
  [82] = {.lex_state = 1},
  [83] = {.lex_state = 1},
  [84] = {.lex_state = 27},
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 1},
  [87] = {.lex_state = 1},
  [88] = {.lex_state = 1},
  [89] = {.lex_state = 30},
  [90] = {.lex_state = 27},
  [91] = {.lex_state = 2},
  [92] = {.lex_state = 2},
  [93] = {.lex_state = 2},
  [94] = {.lex_state = 2},
  [95] = {.lex_state = 2},
  [96] = {.lex_state = 2},
  [97] = {.lex_state = 6},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 2},
  [100] = {.lex_state = 2},
  [101] = {.lex_state = 0},
  [102] = {.lex_state = 2},
  [103] = {.lex_state = 2},
  [104] = {.lex_state = 0},
  [105] = {.lex_state = 2},
  [106] = {.lex_state = 2},
  [107] = {.lex_state = 0},
  [108] = {.lex_state = 0},
  [109] = {.lex_state = 2},
  [110] = {.lex_state = 2},
  [111] = {.lex_state = 6},
  [112] = {.lex_state = 30},
  [113] = {.lex_state = 6},
  [114] = {.lex_state = 6},
  [115] = {.lex_state = 6},
  [116] = {.lex_state = 6},
  [117] = {.lex_state = 6},
  [118] = {.lex_state = 27},
  [119] = {.lex_state = 0},
  [120] = {.lex_state = 0},
  [121] = {.lex_state = 0},
  [122] = {.lex_state = 0},
  [123] = {.lex_state = 0},
  [124] = {.lex_state = 0},
  [125] = {.lex_state = 0},
  [126] = {.lex_state = 0},
  [127] = {.lex_state = 0},
  [128] = {.lex_state = 0},
  [129] = {.lex_state = 0},
  [130] = {.lex_state = 0},
  [131] = {.lex_state = 0},
  [132] = {.lex_state = 0},
  [133] = {.lex_state = 0},
  [134] = {.lex_state = 0},
  [135] = {.lex_state = 0},
  [136] = {.lex_state = 0},
  [137] = {.lex_state = 0},
  [138] = {.lex_state = 0},
  [139] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [aux_sym_bool_literal_token1] = ACTIONS(1),
    [aux_sym_bool_literal_token2] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [sym_string_escape_sequence] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_L] = ACTIONS(1),
    [sym_digits] = ACTIONS(1),
    [aux_sym_builtin_type_token1] = ACTIONS(1),
    [aux_sym_builtin_type_token2] = ACTIONS(1),
    [aux_sym_builtin_type_token3] = ACTIONS(1),
    [aux_sym_builtin_type_token4] = ACTIONS(1),
    [aux_sym_builtin_type_token5] = ACTIONS(1),
    [aux_sym_builtin_type_token6] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [aux_sym_variable_identifier_token1] = ACTIONS(1),
    [sym_unit] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(130),
    [sym_fn_decl] = STATE(2),
    [sym_type_decl] = STATE(2),
    [sym_expression] = STATE(65),
    [sym_paren_expression] = STATE(62),
    [sym_bool_literal] = STATE(62),
    [sym_function_call] = STATE(62),
    [sym_let_expression] = STATE(62),
    [sym_string_literal] = STATE(62),
    [sym_infix_operation] = STATE(62),
    [sym_int64_literal] = STATE(62),
    [sym_variable_identifier] = STATE(62),
    [aux_sym_source_file_repeat1] = STATE(2),
    [aux_sym_source_file_repeat2] = STATE(14),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_LPAREN] = ACTIONS(7),
    [anon_sym_type] = ACTIONS(9),
    [aux_sym_bool_literal_token1] = ACTIONS(11),
    [aux_sym_bool_literal_token2] = ACTIONS(11),
    [anon_sym_DQUOTE] = ACTIONS(13),
    [sym_digits] = ACTIONS(15),
    [aux_sym_variable_identifier_token1] = ACTIONS(17),
    [sym_unit] = ACTIONS(19),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 13,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 1,
      anon_sym_type,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(17), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(21), 1,
      ts_builtin_sym_end,
    STATE(8), 1,
      aux_sym_source_file_repeat2,
    STATE(65), 1,
      sym_expression,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(33), 3,
      sym_fn_decl,
      sym_type_decl,
      aux_sym_source_file_repeat1,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [50] = 14,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(25), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(27), 1,
      aux_sym_type_identifier_token1,
    STATE(17), 1,
      sym_qualified_fn_name,
    STATE(76), 1,
      sym_fn_identifier,
    STATE(84), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(98), 1,
      sym_expression,
    STATE(121), 1,
      sym_module_identifier,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [101] = 14,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(25), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(27), 1,
      aux_sym_type_identifier_token1,
    STATE(18), 1,
      sym_qualified_fn_name,
    STATE(76), 1,
      sym_fn_identifier,
    STATE(84), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(101), 1,
      sym_expression,
    STATE(121), 1,
      sym_module_identifier,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [152] = 14,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(25), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(27), 1,
      aux_sym_type_identifier_token1,
    STATE(15), 1,
      sym_qualified_fn_name,
    STATE(76), 1,
      sym_fn_identifier,
    STATE(84), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(104), 1,
      sym_expression,
    STATE(121), 1,
      sym_module_identifier,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [203] = 14,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(25), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(27), 1,
      aux_sym_type_identifier_token1,
    STATE(16), 1,
      sym_qualified_fn_name,
    STATE(76), 1,
      sym_fn_identifier,
    STATE(84), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(107), 1,
      sym_expression,
    STATE(121), 1,
      sym_module_identifier,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [254] = 11,
    ACTIONS(29), 1,
      ts_builtin_sym_end,
    ACTIONS(31), 1,
      anon_sym_let,
    ACTIONS(34), 1,
      anon_sym_LPAREN,
    ACTIONS(40), 1,
      anon_sym_DQUOTE,
    ACTIONS(43), 1,
      sym_digits,
    ACTIONS(46), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(49), 1,
      sym_unit,
    STATE(7), 1,
      aux_sym_source_file_repeat2,
    STATE(65), 1,
      sym_expression,
    ACTIONS(37), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [296] = 11,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(17), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(52), 1,
      ts_builtin_sym_end,
    STATE(7), 1,
      aux_sym_source_file_repeat2,
    STATE(65), 1,
      sym_expression,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [338] = 11,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(58), 1,
      anon_sym_RPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [380] = 11,
    ACTIONS(29), 1,
      anon_sym_RPAREN,
    ACTIONS(70), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_LPAREN,
    ACTIONS(79), 1,
      anon_sym_DQUOTE,
    ACTIONS(82), 1,
      sym_digits,
    ACTIONS(85), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(88), 1,
      sym_unit,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(76), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [422] = 11,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    ACTIONS(91), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [464] = 11,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    ACTIONS(93), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [506] = 11,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    ACTIONS(95), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [548] = 11,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(17), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(21), 1,
      ts_builtin_sym_end,
    ACTIONS(23), 1,
      anon_sym_let,
    STATE(7), 1,
      aux_sym_source_file_repeat2,
    STATE(65), 1,
      sym_expression,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [590] = 10,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(11), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [629] = 10,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(12), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [668] = 10,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(9), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [707] = 10,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(13), 1,
      aux_sym_source_file_repeat2,
    STATE(68), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [746] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(105), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [782] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(100), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [818] = 9,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(17), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    STATE(59), 1,
      sym_expression,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [854] = 9,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(64), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [890] = 9,
    ACTIONS(111), 1,
      anon_sym_let,
    ACTIONS(113), 1,
      anon_sym_LPAREN,
    ACTIONS(117), 1,
      anon_sym_DQUOTE,
    ACTIONS(119), 1,
      sym_digits,
    ACTIONS(121), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(123), 1,
      sym_unit,
    STATE(52), 1,
      sym_expression,
    ACTIONS(115), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(41), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [926] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(109), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [962] = 9,
    ACTIONS(54), 1,
      anon_sym_let,
    ACTIONS(56), 1,
      anon_sym_LPAREN,
    ACTIONS(62), 1,
      anon_sym_DQUOTE,
    ACTIONS(64), 1,
      sym_digits,
    ACTIONS(66), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(68), 1,
      sym_unit,
    STATE(72), 1,
      sym_expression,
    ACTIONS(60), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(67), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [998] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(93), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1034] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(95), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1070] = 9,
    ACTIONS(111), 1,
      anon_sym_let,
    ACTIONS(113), 1,
      anon_sym_LPAREN,
    ACTIONS(117), 1,
      anon_sym_DQUOTE,
    ACTIONS(119), 1,
      sym_digits,
    ACTIONS(121), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(123), 1,
      sym_unit,
    STATE(56), 1,
      sym_expression,
    ACTIONS(115), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(41), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1106] = 9,
    ACTIONS(111), 1,
      anon_sym_let,
    ACTIONS(113), 1,
      anon_sym_LPAREN,
    ACTIONS(117), 1,
      anon_sym_DQUOTE,
    ACTIONS(119), 1,
      sym_digits,
    ACTIONS(121), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(123), 1,
      sym_unit,
    STATE(49), 1,
      sym_expression,
    ACTIONS(115), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(41), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1142] = 9,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_digits,
    ACTIONS(17), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(23), 1,
      anon_sym_let,
    STATE(47), 1,
      sym_expression,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(62), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1178] = 9,
    ACTIONS(97), 1,
      anon_sym_let,
    ACTIONS(99), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      sym_digits,
    ACTIONS(107), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(109), 1,
      sym_unit,
    STATE(96), 1,
      sym_expression,
    ACTIONS(101), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    STATE(103), 8,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int64_literal,
      sym_variable_identifier,
  [1214] = 2,
    ACTIONS(127), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(125), 9,
      ts_builtin_sym_end,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1233] = 5,
    ACTIONS(131), 1,
      anon_sym_let,
    ACTIONS(136), 1,
      anon_sym_type,
    STATE(33), 3,
      sym_fn_decl,
      sym_type_decl,
      aux_sym_source_file_repeat1,
    ACTIONS(129), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(134), 4,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1257] = 7,
    ACTIONS(141), 1,
      aux_sym_type_identifier_token1,
    STATE(58), 1,
      sym_type_identifier,
    STATE(75), 1,
      sym_type_reference,
    STATE(89), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
    STATE(57), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(139), 6,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
  [1285] = 7,
    ACTIONS(141), 1,
      aux_sym_type_identifier_token1,
    STATE(58), 1,
      sym_type_identifier,
    STATE(89), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
    STATE(132), 1,
      sym_type_reference,
    STATE(57), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(139), 6,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
  [1313] = 3,
    ACTIONS(147), 1,
      anon_sym_DOT,
    ACTIONS(143), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(145), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1333] = 7,
    ACTIONS(141), 1,
      aux_sym_type_identifier_token1,
    STATE(58), 1,
      sym_type_identifier,
    STATE(89), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
    STATE(123), 1,
      sym_type_reference,
    STATE(57), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(139), 6,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
  [1361] = 2,
    ACTIONS(125), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(127), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1378] = 2,
    ACTIONS(149), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(151), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1395] = 2,
    ACTIONS(155), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(153), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1412] = 2,
    ACTIONS(157), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(159), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1429] = 2,
    ACTIONS(163), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(161), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1446] = 2,
    ACTIONS(161), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(163), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1463] = 2,
    ACTIONS(165), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(167), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1480] = 2,
    ACTIONS(171), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(169), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1497] = 2,
    ACTIONS(169), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(171), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1514] = 2,
    ACTIONS(175), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(173), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1531] = 2,
    ACTIONS(179), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(177), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1548] = 2,
    ACTIONS(173), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(175), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1565] = 2,
    ACTIONS(181), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(183), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1582] = 2,
    ACTIONS(167), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(165), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1599] = 3,
    ACTIONS(189), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(185), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(187), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1618] = 2,
    ACTIONS(183), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(181), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1635] = 2,
    ACTIONS(191), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(193), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1652] = 2,
    ACTIONS(177), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(179), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1669] = 3,
    ACTIONS(189), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(195), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(197), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1688] = 2,
    ACTIONS(199), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(201), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1705] = 2,
    ACTIONS(203), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(205), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1722] = 3,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(185), 5,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(187), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1741] = 3,
    ACTIONS(211), 3,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(125), 4,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(209), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1760] = 2,
    ACTIONS(153), 6,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
    ACTIONS(155), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1777] = 2,
    ACTIONS(159), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(157), 7,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1794] = 2,
    ACTIONS(127), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(125), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1810] = 2,
    ACTIONS(175), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(173), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1826] = 3,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(213), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(215), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1844] = 2,
    ACTIONS(163), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(161), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1860] = 2,
    ACTIONS(159), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(157), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1876] = 3,
    ACTIONS(217), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(213), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(215), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1894] = 2,
    ACTIONS(179), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(177), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1910] = 2,
    ACTIONS(155), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(153), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1926] = 2,
    ACTIONS(183), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(181), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1942] = 3,
    ACTIONS(217), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(185), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(187), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [1960] = 2,
    ACTIONS(167), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(165), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1976] = 2,
    ACTIONS(171), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
    ACTIONS(169), 6,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_digits,
      sym_unit,
  [1992] = 2,
    ACTIONS(219), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(221), 6,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [2007] = 2,
    ACTIONS(225), 3,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(223), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [2020] = 2,
    ACTIONS(211), 3,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(209), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [2033] = 2,
    ACTIONS(229), 3,
      anon_sym_DQUOTE,
      sym_digits,
      sym_unit,
    ACTIONS(227), 5,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      aux_sym_variable_identifier_token1,
  [2046] = 4,
    ACTIONS(231), 1,
      anon_sym_COLON,
    ACTIONS(233), 1,
      anon_sym_LPAREN,
    ACTIONS(236), 1,
      sym_unit,
    STATE(79), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [2060] = 4,
    ACTIONS(239), 1,
      anon_sym_LPAREN,
    ACTIONS(241), 1,
      sym_unit,
    STATE(139), 1,
      sym_fn_decl_params,
    STATE(85), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [2074] = 4,
    ACTIONS(243), 1,
      anon_sym_DQUOTE,
    STATE(88), 1,
      aux_sym_string_content_repeat1,
    STATE(122), 1,
      sym_string_content,
    ACTIONS(245), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2088] = 4,
    ACTIONS(247), 1,
      anon_sym_DQUOTE,
    STATE(88), 1,
      aux_sym_string_content_repeat1,
    STATE(127), 1,
      sym_string_content,
    ACTIONS(245), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2102] = 4,
    ACTIONS(249), 1,
      anon_sym_DQUOTE,
    STATE(88), 1,
      aux_sym_string_content_repeat1,
    STATE(119), 1,
      sym_string_content,
    ACTIONS(245), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2116] = 5,
    ACTIONS(27), 1,
      aux_sym_type_identifier_token1,
    ACTIONS(251), 1,
      aux_sym_variable_identifier_token1,
    STATE(78), 1,
      sym_fn_identifier,
    STATE(90), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
  [2132] = 4,
    ACTIONS(239), 1,
      anon_sym_LPAREN,
    ACTIONS(253), 1,
      anon_sym_COLON,
    ACTIONS(255), 1,
      sym_unit,
    STATE(79), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [2146] = 4,
    ACTIONS(257), 1,
      anon_sym_DQUOTE,
    STATE(88), 1,
      aux_sym_string_content_repeat1,
    STATE(135), 1,
      sym_string_content,
    ACTIONS(245), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2160] = 3,
    ACTIONS(259), 1,
      anon_sym_DQUOTE,
    STATE(87), 1,
      aux_sym_string_content_repeat1,
    ACTIONS(261), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2171] = 3,
    ACTIONS(264), 1,
      anon_sym_DQUOTE,
    STATE(87), 1,
      aux_sym_string_content_repeat1,
    ACTIONS(266), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [2182] = 4,
    ACTIONS(268), 1,
      aux_sym_type_identifier_token1,
    STATE(39), 1,
      sym_type_identifier,
    STATE(90), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
  [2195] = 4,
    ACTIONS(270), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(272), 1,
      aux_sym_type_identifier_token1,
    STATE(90), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(121), 1,
      sym_module_identifier,
  [2208] = 2,
    ACTIONS(153), 1,
      anon_sym_LF,
    ACTIONS(155), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2216] = 2,
    ACTIONS(169), 1,
      anon_sym_LF,
    ACTIONS(171), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2224] = 2,
    ACTIONS(173), 1,
      anon_sym_LF,
    ACTIONS(175), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2232] = 2,
    ACTIONS(181), 1,
      anon_sym_LF,
    ACTIONS(183), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2240] = 2,
    ACTIONS(185), 1,
      anon_sym_LF,
    ACTIONS(275), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2248] = 2,
    ACTIONS(277), 1,
      anon_sym_LF,
    ACTIONS(275), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2256] = 3,
    ACTIONS(279), 1,
      aux_sym_variable_identifier_token1,
    STATE(80), 1,
      sym_fn_identifier,
    STATE(128), 1,
      sym_variable_identifier,
  [2266] = 2,
    ACTIONS(281), 1,
      anon_sym_RPAREN,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2274] = 2,
    ACTIONS(165), 1,
      anon_sym_LF,
    ACTIONS(167), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2282] = 2,
    ACTIONS(283), 1,
      anon_sym_LF,
    ACTIONS(275), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2290] = 2,
    ACTIONS(285), 1,
      anon_sym_RPAREN,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2298] = 2,
    ACTIONS(177), 1,
      anon_sym_LF,
    ACTIONS(179), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2306] = 2,
    ACTIONS(157), 1,
      anon_sym_LF,
    ACTIONS(159), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2314] = 2,
    ACTIONS(287), 1,
      anon_sym_RPAREN,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2322] = 2,
    ACTIONS(289), 1,
      anon_sym_LF,
    ACTIONS(275), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2330] = 2,
    ACTIONS(125), 1,
      anon_sym_LF,
    ACTIONS(127), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2338] = 2,
    ACTIONS(291), 1,
      anon_sym_RPAREN,
    ACTIONS(207), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2346] = 2,
    ACTIONS(295), 1,
      anon_sym_LPAREN,
    ACTIONS(293), 2,
      anon_sym_COLON,
      sym_unit,
  [2354] = 2,
    ACTIONS(297), 1,
      anon_sym_LF,
    ACTIONS(275), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2362] = 2,
    ACTIONS(161), 1,
      anon_sym_LF,
    ACTIONS(163), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [2370] = 2,
    ACTIONS(299), 1,
      aux_sym_variable_identifier_token1,
    STATE(129), 1,
      sym_variable_identifier,
  [2377] = 2,
    ACTIONS(301), 1,
      aux_sym_type_identifier_token1,
    STATE(124), 1,
      sym_type_identifier,
  [2384] = 2,
    ACTIONS(299), 1,
      aux_sym_variable_identifier_token1,
    STATE(138), 1,
      sym_variable_identifier,
  [2391] = 2,
    ACTIONS(299), 1,
      aux_sym_variable_identifier_token1,
    STATE(136), 1,
      sym_variable_identifier,
  [2398] = 2,
    ACTIONS(299), 1,
      aux_sym_variable_identifier_token1,
    STATE(134), 1,
      sym_variable_identifier,
  [2405] = 2,
    ACTIONS(251), 1,
      aux_sym_variable_identifier_token1,
    STATE(80), 1,
      sym_fn_identifier,
  [2412] = 2,
    ACTIONS(299), 1,
      aux_sym_variable_identifier_token1,
    STATE(128), 1,
      sym_variable_identifier,
  [2419] = 1,
    ACTIONS(270), 2,
      aux_sym_variable_identifier_token1,
      aux_sym_type_identifier_token1,
  [2424] = 1,
    ACTIONS(303), 1,
      anon_sym_DQUOTE,
  [2428] = 1,
    ACTIONS(305), 1,
      anon_sym_L,
  [2432] = 1,
    ACTIONS(307), 1,
      anon_sym_DOT,
  [2436] = 1,
    ACTIONS(309), 1,
      anon_sym_DQUOTE,
  [2440] = 1,
    ACTIONS(311), 1,
      anon_sym_EQ,
  [2444] = 1,
    ACTIONS(313), 1,
      anon_sym_EQ,
  [2448] = 1,
    ACTIONS(147), 1,
      anon_sym_DOT,
  [2452] = 1,
    ACTIONS(315), 1,
      anon_sym_L,
  [2456] = 1,
    ACTIONS(317), 1,
      anon_sym_DQUOTE,
  [2460] = 1,
    ACTIONS(319), 1,
      anon_sym_EQ,
  [2464] = 1,
    ACTIONS(321), 1,
      anon_sym_COLON,
  [2468] = 1,
    ACTIONS(323), 1,
      ts_builtin_sym_end,
  [2472] = 1,
    ACTIONS(325), 1,
      anon_sym_L,
  [2476] = 1,
    ACTIONS(327), 1,
      anon_sym_RPAREN,
  [2480] = 1,
    ACTIONS(143), 1,
      anon_sym_EQ,
  [2484] = 1,
    ACTIONS(329), 1,
      anon_sym_EQ,
  [2488] = 1,
    ACTIONS(331), 1,
      anon_sym_DQUOTE,
  [2492] = 1,
    ACTIONS(333), 1,
      anon_sym_EQ,
  [2496] = 1,
    ACTIONS(335), 1,
      anon_sym_L,
  [2500] = 1,
    ACTIONS(337), 1,
      anon_sym_EQ,
  [2504] = 1,
    ACTIONS(339), 1,
      anon_sym_COLON,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 50,
  [SMALL_STATE(4)] = 101,
  [SMALL_STATE(5)] = 152,
  [SMALL_STATE(6)] = 203,
  [SMALL_STATE(7)] = 254,
  [SMALL_STATE(8)] = 296,
  [SMALL_STATE(9)] = 338,
  [SMALL_STATE(10)] = 380,
  [SMALL_STATE(11)] = 422,
  [SMALL_STATE(12)] = 464,
  [SMALL_STATE(13)] = 506,
  [SMALL_STATE(14)] = 548,
  [SMALL_STATE(15)] = 590,
  [SMALL_STATE(16)] = 629,
  [SMALL_STATE(17)] = 668,
  [SMALL_STATE(18)] = 707,
  [SMALL_STATE(19)] = 746,
  [SMALL_STATE(20)] = 782,
  [SMALL_STATE(21)] = 818,
  [SMALL_STATE(22)] = 854,
  [SMALL_STATE(23)] = 890,
  [SMALL_STATE(24)] = 926,
  [SMALL_STATE(25)] = 962,
  [SMALL_STATE(26)] = 998,
  [SMALL_STATE(27)] = 1034,
  [SMALL_STATE(28)] = 1070,
  [SMALL_STATE(29)] = 1106,
  [SMALL_STATE(30)] = 1142,
  [SMALL_STATE(31)] = 1178,
  [SMALL_STATE(32)] = 1214,
  [SMALL_STATE(33)] = 1233,
  [SMALL_STATE(34)] = 1257,
  [SMALL_STATE(35)] = 1285,
  [SMALL_STATE(36)] = 1313,
  [SMALL_STATE(37)] = 1333,
  [SMALL_STATE(38)] = 1361,
  [SMALL_STATE(39)] = 1378,
  [SMALL_STATE(40)] = 1395,
  [SMALL_STATE(41)] = 1412,
  [SMALL_STATE(42)] = 1429,
  [SMALL_STATE(43)] = 1446,
  [SMALL_STATE(44)] = 1463,
  [SMALL_STATE(45)] = 1480,
  [SMALL_STATE(46)] = 1497,
  [SMALL_STATE(47)] = 1514,
  [SMALL_STATE(48)] = 1531,
  [SMALL_STATE(49)] = 1548,
  [SMALL_STATE(50)] = 1565,
  [SMALL_STATE(51)] = 1582,
  [SMALL_STATE(52)] = 1599,
  [SMALL_STATE(53)] = 1618,
  [SMALL_STATE(54)] = 1635,
  [SMALL_STATE(55)] = 1652,
  [SMALL_STATE(56)] = 1669,
  [SMALL_STATE(57)] = 1688,
  [SMALL_STATE(58)] = 1705,
  [SMALL_STATE(59)] = 1722,
  [SMALL_STATE(60)] = 1741,
  [SMALL_STATE(61)] = 1760,
  [SMALL_STATE(62)] = 1777,
  [SMALL_STATE(63)] = 1794,
  [SMALL_STATE(64)] = 1810,
  [SMALL_STATE(65)] = 1826,
  [SMALL_STATE(66)] = 1844,
  [SMALL_STATE(67)] = 1860,
  [SMALL_STATE(68)] = 1876,
  [SMALL_STATE(69)] = 1894,
  [SMALL_STATE(70)] = 1910,
  [SMALL_STATE(71)] = 1926,
  [SMALL_STATE(72)] = 1942,
  [SMALL_STATE(73)] = 1960,
  [SMALL_STATE(74)] = 1976,
  [SMALL_STATE(75)] = 1992,
  [SMALL_STATE(76)] = 2007,
  [SMALL_STATE(77)] = 2020,
  [SMALL_STATE(78)] = 2033,
  [SMALL_STATE(79)] = 2046,
  [SMALL_STATE(80)] = 2060,
  [SMALL_STATE(81)] = 2074,
  [SMALL_STATE(82)] = 2088,
  [SMALL_STATE(83)] = 2102,
  [SMALL_STATE(84)] = 2116,
  [SMALL_STATE(85)] = 2132,
  [SMALL_STATE(86)] = 2146,
  [SMALL_STATE(87)] = 2160,
  [SMALL_STATE(88)] = 2171,
  [SMALL_STATE(89)] = 2182,
  [SMALL_STATE(90)] = 2195,
  [SMALL_STATE(91)] = 2208,
  [SMALL_STATE(92)] = 2216,
  [SMALL_STATE(93)] = 2224,
  [SMALL_STATE(94)] = 2232,
  [SMALL_STATE(95)] = 2240,
  [SMALL_STATE(96)] = 2248,
  [SMALL_STATE(97)] = 2256,
  [SMALL_STATE(98)] = 2266,
  [SMALL_STATE(99)] = 2274,
  [SMALL_STATE(100)] = 2282,
  [SMALL_STATE(101)] = 2290,
  [SMALL_STATE(102)] = 2298,
  [SMALL_STATE(103)] = 2306,
  [SMALL_STATE(104)] = 2314,
  [SMALL_STATE(105)] = 2322,
  [SMALL_STATE(106)] = 2330,
  [SMALL_STATE(107)] = 2338,
  [SMALL_STATE(108)] = 2346,
  [SMALL_STATE(109)] = 2354,
  [SMALL_STATE(110)] = 2362,
  [SMALL_STATE(111)] = 2370,
  [SMALL_STATE(112)] = 2377,
  [SMALL_STATE(113)] = 2384,
  [SMALL_STATE(114)] = 2391,
  [SMALL_STATE(115)] = 2398,
  [SMALL_STATE(116)] = 2405,
  [SMALL_STATE(117)] = 2412,
  [SMALL_STATE(118)] = 2419,
  [SMALL_STATE(119)] = 2424,
  [SMALL_STATE(120)] = 2428,
  [SMALL_STATE(121)] = 2432,
  [SMALL_STATE(122)] = 2436,
  [SMALL_STATE(123)] = 2440,
  [SMALL_STATE(124)] = 2444,
  [SMALL_STATE(125)] = 2448,
  [SMALL_STATE(126)] = 2452,
  [SMALL_STATE(127)] = 2456,
  [SMALL_STATE(128)] = 2460,
  [SMALL_STATE(129)] = 2464,
  [SMALL_STATE(130)] = 2468,
  [SMALL_STATE(131)] = 2472,
  [SMALL_STATE(132)] = 2476,
  [SMALL_STATE(133)] = 2480,
  [SMALL_STATE(134)] = 2484,
  [SMALL_STATE(135)] = 2488,
  [SMALL_STATE(136)] = 2492,
  [SMALL_STATE(137)] = 2496,
  [SMALL_STATE(138)] = 2500,
  [SMALL_STATE(139)] = 2504,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(97),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(112),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(131),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(117),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(60),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(125),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2),
  [31] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(117),
  [34] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(3),
  [37] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(42),
  [40] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(86),
  [43] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(131),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(32),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(62),
  [52] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2),
  [54] = {.entry = {.count = 1, .reusable = false}}, SHIFT(115),
  [56] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [58] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [60] = {.entry = {.count = 1, .reusable = false}}, SHIFT(66),
  [62] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [64] = {.entry = {.count = 1, .reusable = true}}, SHIFT(126),
  [66] = {.entry = {.count = 1, .reusable = false}}, SHIFT(63),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [70] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(115),
  [73] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(5),
  [76] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(66),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(83),
  [82] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(126),
  [85] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(63),
  [88] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(67),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [97] = {.entry = {.count = 1, .reusable = false}}, SHIFT(114),
  [99] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [101] = {.entry = {.count = 1, .reusable = false}}, SHIFT(110),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(137),
  [107] = {.entry = {.count = 1, .reusable = false}}, SHIFT(106),
  [109] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [111] = {.entry = {.count = 1, .reusable = false}}, SHIFT(113),
  [113] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [115] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [117] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [119] = {.entry = {.count = 1, .reusable = true}}, SHIFT(120),
  [121] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [123] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_identifier, 1),
  [127] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_identifier, 1),
  [129] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [131] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(116),
  [134] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [136] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(112),
  [139] = {.entry = {.count = 1, .reusable = false}}, SHIFT(54),
  [141] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_identifier, 1),
  [145] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_identifier, 1),
  [147] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_identifier, 1),
  [149] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_type_name, 2),
  [151] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_type_name, 2),
  [153] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int64_literal, 2, .production_id = 2),
  [155] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int64_literal, 2, .production_id = 2),
  [157] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [159] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1),
  [161] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bool_literal, 1),
  [163] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_bool_literal, 1),
  [165] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_paren_expression, 3, .production_id = 3),
  [167] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_paren_expression, 3, .production_id = 3),
  [169] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [171] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [173] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [175] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [177] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 2, .production_id = 1),
  [179] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 2, .production_id = 1),
  [181] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 4, .production_id = 6),
  [183] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_call, 4, .production_id = 6),
  [185] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_expression, 6, .production_id = 8),
  [187] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_expression, 6, .production_id = 8),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [191] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_builtin_type, 1),
  [193] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_builtin_type, 1),
  [195] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl, 7, .production_id = 10),
  [197] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_decl, 7, .production_id = 10),
  [199] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_reference, 1),
  [201] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_reference, 1),
  [203] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_type_name, 1),
  [205] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_type_name, 1),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [209] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_identifier, 1),
  [211] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_identifier, 1),
  [213] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 1),
  [215] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 1),
  [217] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [219] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_decl, 4, .production_id = 7),
  [221] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_decl, 4, .production_id = 7),
  [223] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_fn_name, 1),
  [225] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_fn_name, 1),
  [227] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_fn_name, 2),
  [229] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_fn_name, 2),
  [231] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2),
  [233] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2), SHIFT_REPEAT(111),
  [236] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2), SHIFT_REPEAT(79),
  [239] = {.entry = {.count = 1, .reusable = false}}, SHIFT(111),
  [241] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [243] = {.entry = {.count = 1, .reusable = false}}, SHIFT(55),
  [245] = {.entry = {.count = 1, .reusable = true}}, SHIFT(88),
  [247] = {.entry = {.count = 1, .reusable = false}}, SHIFT(102),
  [249] = {.entry = {.count = 1, .reusable = false}}, SHIFT(69),
  [251] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [253] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl_params, 1),
  [255] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [257] = {.entry = {.count = 1, .reusable = false}}, SHIFT(48),
  [259] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_content_repeat1, 2),
  [261] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_content_repeat1, 2), SHIFT_REPEAT(87),
  [264] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_content, 1),
  [266] = {.entry = {.count = 1, .reusable = true}}, SHIFT(87),
  [268] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [270] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_fn_name_repeat1, 2),
  [272] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_fn_name_repeat1, 2), SHIFT_REPEAT(125),
  [275] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [277] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [279] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [281] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [283] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [285] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [287] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [289] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [291] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [293] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl_param, 5, .production_id = 9),
  [295] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_decl_param, 5, .production_id = 9),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [301] = {.entry = {.count = 1, .reusable = true}}, SHIFT(133),
  [303] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [305] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [307] = {.entry = {.count = 1, .reusable = true}}, SHIFT(118),
  [309] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [311] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [313] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [315] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [317] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [319] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [321] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [323] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [325] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [327] = {.entry = {.count = 1, .reusable = true}}, SHIFT(108),
  [329] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [331] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [333] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [335] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [337] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [339] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_darklang(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
