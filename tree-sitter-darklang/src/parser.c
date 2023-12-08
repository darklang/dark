#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 113
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 44
#define ALIAS_COUNT 0
#define TOKEN_COUNT 24
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 13
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 8

enum {
  anon_sym_let = 1,
  anon_sym_COLON = 2,
  anon_sym_EQ = 3,
  anon_sym_type = 4,
  anon_sym_LPAREN = 5,
  anon_sym_RPAREN = 6,
  anon_sym_in = 7,
  anon_sym_LF = 8,
  anon_sym_DQUOTE = 9,
  aux_sym_string_literal_token1 = 10,
  anon_sym_PLUS = 11,
  anon_sym_DASH = 12,
  anon_sym_ = 13,
  anon_sym_DOT = 14,
  sym_unit = 15,
  aux_sym_type_token1 = 16,
  aux_sym_type_token2 = 17,
  aux_sym_type_token3 = 18,
  aux_sym_type_token4 = 19,
  aux_sym_type_token5 = 20,
  aux_sym_type_token6 = 21,
  sym_identifier = 22,
  sym_type_identifier = 23,
  sym_source_file = 24,
  sym_fn_def = 25,
  sym_type_def = 26,
  sym_fn_params_def = 27,
  sym_fn_param_def = 28,
  sym_expression = 29,
  sym_let_expression = 30,
  sym_function_call = 31,
  sym_string_literal = 32,
  sym_infix_operator = 33,
  sym_infix_operation = 34,
  sym_function_args = 35,
  sym_qualified_name = 36,
  sym_type = 37,
  sym_fn_call_identifier = 38,
  aux_sym_source_file_repeat1 = 39,
  aux_sym_fn_params_def_repeat1 = 40,
  aux_sym_function_call_repeat1 = 41,
  aux_sym_function_args_repeat1 = 42,
  aux_sym_qualified_name_repeat1 = 43,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_let] = "let",
  [anon_sym_COLON] = ":",
  [anon_sym_EQ] = "=",
  [anon_sym_type] = "type",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_in] = "in",
  [anon_sym_LF] = "\n",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_literal_token1] = "string_literal_token1",
  [anon_sym_PLUS] = "+",
  [anon_sym_DASH] = "-",
  [anon_sym_] = " ",
  [anon_sym_DOT] = ".",
  [sym_unit] = "unit",
  [aux_sym_type_token1] = "type_token1",
  [aux_sym_type_token2] = "type_token2",
  [aux_sym_type_token3] = "type_token3",
  [aux_sym_type_token4] = "type_token4",
  [aux_sym_type_token5] = "type_token5",
  [aux_sym_type_token6] = "type_token6",
  [sym_identifier] = "identifier",
  [sym_type_identifier] = "type_identifier",
  [sym_source_file] = "source_file",
  [sym_fn_def] = "fn_def",
  [sym_type_def] = "type_def",
  [sym_fn_params_def] = "fn_params_def",
  [sym_fn_param_def] = "fn_param_def",
  [sym_expression] = "expression",
  [sym_let_expression] = "let_expression",
  [sym_function_call] = "function_call",
  [sym_string_literal] = "string_literal",
  [sym_infix_operator] = "infix_operator",
  [sym_infix_operation] = "infix_operation",
  [sym_function_args] = "function_args",
  [sym_qualified_name] = "qualified_name",
  [sym_type] = "type",
  [sym_fn_call_identifier] = "fn_call_identifier",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_fn_params_def_repeat1] = "fn_params_def_repeat1",
  [aux_sym_function_call_repeat1] = "function_call_repeat1",
  [aux_sym_function_args_repeat1] = "function_args_repeat1",
  [aux_sym_qualified_name_repeat1] = "qualified_name_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_in] = anon_sym_in,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_literal_token1] = aux_sym_string_literal_token1,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_] = anon_sym_,
  [anon_sym_DOT] = anon_sym_DOT,
  [sym_unit] = sym_unit,
  [aux_sym_type_token1] = aux_sym_type_token1,
  [aux_sym_type_token2] = aux_sym_type_token2,
  [aux_sym_type_token3] = aux_sym_type_token3,
  [aux_sym_type_token4] = aux_sym_type_token4,
  [aux_sym_type_token5] = aux_sym_type_token5,
  [aux_sym_type_token6] = aux_sym_type_token6,
  [sym_identifier] = sym_identifier,
  [sym_type_identifier] = sym_type_identifier,
  [sym_source_file] = sym_source_file,
  [sym_fn_def] = sym_fn_def,
  [sym_type_def] = sym_type_def,
  [sym_fn_params_def] = sym_fn_params_def,
  [sym_fn_param_def] = sym_fn_param_def,
  [sym_expression] = sym_expression,
  [sym_let_expression] = sym_let_expression,
  [sym_function_call] = sym_function_call,
  [sym_string_literal] = sym_string_literal,
  [sym_infix_operator] = sym_infix_operator,
  [sym_infix_operation] = sym_infix_operation,
  [sym_function_args] = sym_function_args,
  [sym_qualified_name] = sym_qualified_name,
  [sym_type] = sym_type,
  [sym_fn_call_identifier] = sym_fn_call_identifier,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_fn_params_def_repeat1] = aux_sym_fn_params_def_repeat1,
  [aux_sym_function_call_repeat1] = aux_sym_function_call_repeat1,
  [aux_sym_function_args_repeat1] = aux_sym_function_args_repeat1,
  [aux_sym_qualified_name_repeat1] = aux_sym_qualified_name_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_in] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_literal_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [sym_unit] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_type_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_token4] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_token5] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_type_token6] = {
    .visible = false,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_type_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_def] = {
    .visible = true,
    .named = true,
  },
  [sym_type_def] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_params_def] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_param_def] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_let_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_function_call] = {
    .visible = true,
    .named = true,
  },
  [sym_string_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_infix_operator] = {
    .visible = true,
    .named = true,
  },
  [sym_infix_operation] = {
    .visible = true,
    .named = true,
  },
  [sym_function_args] = {
    .visible = true,
    .named = true,
  },
  [sym_qualified_name] = {
    .visible = true,
    .named = true,
  },
  [sym_type] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_call_identifier] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_fn_params_def_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_function_call_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_function_args_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_qualified_name_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_args = 1,
  field_body = 2,
  field_expr = 3,
  field_fn = 4,
  field_identifier = 5,
  field_left = 6,
  field_name = 7,
  field_operator = 8,
  field_params = 9,
  field_return_type = 10,
  field_right = 11,
  field_typ = 12,
  field_value = 13,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_args] = "args",
  [field_body] = "body",
  [field_expr] = "expr",
  [field_fn] = "fn",
  [field_identifier] = "identifier",
  [field_left] = "left",
  [field_name] = "name",
  [field_operator] = "operator",
  [field_params] = "params",
  [field_return_type] = "return_type",
  [field_right] = "right",
  [field_typ] = "typ",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 2},
  [3] = {.index = 4, .length = 4},
  [4] = {.index = 8, .length = 2},
  [5] = {.index = 10, .length = 1},
  [6] = {.index = 11, .length = 3},
  [7] = {.index = 14, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 1},
    {field_typ, 3},
  [2] =
    {field_identifier, 1},
    {field_typ, 3},
  [4] =
    {field_body, 6},
    {field_name, 1},
    {field_params, 2},
    {field_return_type, 4},
  [8] =
    {field_args, 1},
    {field_fn, 0},
  [10] =
    {field_value, 1},
  [11] =
    {field_left, 0},
    {field_operator, 1},
    {field_right, 2},
  [14] =
    {field_body, 5},
    {field_expr, 3},
    {field_identifier, 1},
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
  [4] = 2,
  [5] = 3,
  [6] = 2,
  [7] = 2,
  [8] = 3,
  [9] = 3,
  [10] = 10,
  [11] = 10,
  [12] = 10,
  [13] = 10,
  [14] = 14,
  [15] = 15,
  [16] = 14,
  [17] = 14,
  [18] = 15,
  [19] = 14,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 22,
  [24] = 24,
  [25] = 22,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 24,
  [31] = 28,
  [32] = 32,
  [33] = 33,
  [34] = 28,
  [35] = 24,
  [36] = 15,
  [37] = 21,
  [38] = 32,
  [39] = 15,
  [40] = 27,
  [41] = 28,
  [42] = 32,
  [43] = 22,
  [44] = 33,
  [45] = 20,
  [46] = 26,
  [47] = 24,
  [48] = 32,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 51,
  [53] = 50,
  [54] = 54,
  [55] = 55,
  [56] = 20,
  [57] = 57,
  [58] = 27,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 51,
  [68] = 60,
  [69] = 69,
  [70] = 63,
  [71] = 71,
  [72] = 72,
  [73] = 63,
  [74] = 20,
  [75] = 75,
  [76] = 63,
  [77] = 50,
  [78] = 27,
  [79] = 51,
  [80] = 50,
  [81] = 75,
  [82] = 82,
  [83] = 83,
  [84] = 84,
  [85] = 85,
  [86] = 86,
  [87] = 87,
  [88] = 88,
  [89] = 89,
  [90] = 90,
  [91] = 91,
  [92] = 86,
  [93] = 93,
  [94] = 94,
  [95] = 95,
  [96] = 86,
  [97] = 97,
  [98] = 98,
  [99] = 86,
  [100] = 100,
  [101] = 94,
  [102] = 90,
  [103] = 94,
  [104] = 104,
  [105] = 94,
  [106] = 106,
  [107] = 100,
  [108] = 100,
  [109] = 100,
  [110] = 93,
  [111] = 93,
  [112] = 93,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(38);
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(45);
      if (lookahead == ')') ADVANCE(46);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == ':') ADVANCE(41);
      if (lookahead == '=') ADVANCE(42);
      if (lookahead == 'B') ADVANCE(86);
      if (lookahead == 'C') ADVANCE(77);
      if (lookahead == 'F') ADVANCE(81);
      if (lookahead == 'I') ADVANCE(85);
      if (lookahead == 'S') ADVANCE(92);
      if (lookahead == 'U') ADVANCE(82);
      if (lookahead == 'i') ADVANCE(83);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == 't') ADVANCE(97);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == ' ') ADVANCE(56);
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(5);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == 'i') ADVANCE(83);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == '\t' ||
          lookahead == '\r') SKIP(1)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(50);
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(5);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == 'i') ADVANCE(83);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 3:
      if (lookahead == '\n') ADVANCE(50);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == 'i') ADVANCE(19);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      END_STATE();
    case 4:
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(5);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 5:
      if (lookahead == ')') ADVANCE(59);
      END_STATE();
    case 6:
      if (lookahead == 'B') ADVANCE(86);
      if (lookahead == 'C') ADVANCE(77);
      if (lookahead == 'F') ADVANCE(81);
      if (lookahead == 'I') ADVANCE(85);
      if (lookahead == 'S') ADVANCE(92);
      if (lookahead == 'U') ADVANCE(82);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(6)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 7:
      if (lookahead == 'a') ADVANCE(25);
      END_STATE();
    case 8:
      if (lookahead == 'a') ADVANCE(31);
      END_STATE();
    case 9:
      if (lookahead == 'e') ADVANCE(43);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(29);
      END_STATE();
    case 11:
      if (lookahead == 'g') ADVANCE(68);
      END_STATE();
    case 12:
      if (lookahead == 'h') ADVANCE(7);
      END_STATE();
    case 13:
      if (lookahead == 'i') ADVANCE(18);
      END_STATE();
    case 14:
      if (lookahead == 'i') ADVANCE(30);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(64);
      END_STATE();
    case 16:
      if (lookahead == 'l') ADVANCE(23);
      END_STATE();
    case 17:
      if (lookahead == 'n') ADVANCE(14);
      END_STATE();
    case 18:
      if (lookahead == 'n') ADVANCE(11);
      END_STATE();
    case 19:
      if (lookahead == 'n') ADVANCE(47);
      END_STATE();
    case 20:
      if (lookahead == 'n') ADVANCE(28);
      END_STATE();
    case 21:
      if (lookahead == 'o') ADVANCE(22);
      END_STATE();
    case 22:
      if (lookahead == 'o') ADVANCE(15);
      END_STATE();
    case 23:
      if (lookahead == 'o') ADVANCE(8);
      END_STATE();
    case 24:
      if (lookahead == 'p') ADVANCE(9);
      END_STATE();
    case 25:
      if (lookahead == 'r') ADVANCE(70);
      END_STATE();
    case 26:
      if (lookahead == 'r') ADVANCE(13);
      END_STATE();
    case 27:
      if (lookahead == 't') ADVANCE(26);
      END_STATE();
    case 28:
      if (lookahead == 't') ADVANCE(62);
      END_STATE();
    case 29:
      if (lookahead == 't') ADVANCE(39);
      END_STATE();
    case 30:
      if (lookahead == 't') ADVANCE(60);
      END_STATE();
    case 31:
      if (lookahead == 't') ADVANCE(66);
      END_STATE();
    case 32:
      if (lookahead == 'y') ADVANCE(24);
      END_STATE();
    case 33:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(33)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 34:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(34)
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(99);
      END_STATE();
    case 35:
      if (eof) ADVANCE(38);
      if (lookahead == ' ') ADVANCE(57);
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(5);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == 't') ADVANCE(97);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r') SKIP(35)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 36:
      if (eof) ADVANCE(38);
      if (lookahead == '"') ADVANCE(51);
      if (lookahead == '(') ADVANCE(5);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == 'l') ADVANCE(75);
      if (lookahead == 't') ADVANCE(97);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(36)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 37:
      if (eof) ADVANCE(38);
      if (lookahead == ')') ADVANCE(46);
      if (lookahead == '+') ADVANCE(54);
      if (lookahead == '-') ADVANCE(55);
      if (lookahead == '.') ADVANCE(58);
      if (lookahead == '=') ADVANCE(42);
      if (lookahead == 'B') ADVANCE(21);
      if (lookahead == 'C') ADVANCE(12);
      if (lookahead == 'F') ADVANCE(16);
      if (lookahead == 'I') ADVANCE(20);
      if (lookahead == 'S') ADVANCE(27);
      if (lookahead == 'U') ADVANCE(17);
      if (lookahead == 'l') ADVANCE(10);
      if (lookahead == 't') ADVANCE(32);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(37)
      END_STATE();
    case 38:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_type);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')') ADVANCE(59);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_in);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_in);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == ' ') ADVANCE(56);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(50);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(52);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(53);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(53);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_);
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == ' ') ADVANCE(56);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_);
      if (lookahead == ' ') ADVANCE(57);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(sym_unit);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(aux_sym_type_token1);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(aux_sym_type_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(aux_sym_type_token2);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(aux_sym_type_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(aux_sym_type_token3);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(aux_sym_type_token3);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(aux_sym_type_token4);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(aux_sym_type_token4);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(aux_sym_type_token5);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(aux_sym_type_token5);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(aux_sym_type_token6);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(aux_sym_type_token6);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(96);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'g') ADVANCE(69);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(84);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(95);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(88);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(79);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(48);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(76);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(87);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(74);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(71);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(78);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(40);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(61);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'y') ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_type_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(99);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 37},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 35},
  [5] = {.lex_state = 35},
  [6] = {.lex_state = 36},
  [7] = {.lex_state = 2},
  [8] = {.lex_state = 2},
  [9] = {.lex_state = 36},
  [10] = {.lex_state = 4},
  [11] = {.lex_state = 4},
  [12] = {.lex_state = 4},
  [13] = {.lex_state = 4},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 35},
  [18] = {.lex_state = 35},
  [19] = {.lex_state = 35},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 4},
  [23] = {.lex_state = 4},
  [24] = {.lex_state = 35},
  [25] = {.lex_state = 4},
  [26] = {.lex_state = 4},
  [27] = {.lex_state = 1},
  [28] = {.lex_state = 4},
  [29] = {.lex_state = 4},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 4},
  [32] = {.lex_state = 4},
  [33] = {.lex_state = 35},
  [34] = {.lex_state = 4},
  [35] = {.lex_state = 1},
  [36] = {.lex_state = 36},
  [37] = {.lex_state = 35},
  [38] = {.lex_state = 4},
  [39] = {.lex_state = 2},
  [40] = {.lex_state = 35},
  [41] = {.lex_state = 4},
  [42] = {.lex_state = 4},
  [43] = {.lex_state = 4},
  [44] = {.lex_state = 1},
  [45] = {.lex_state = 35},
  [46] = {.lex_state = 4},
  [47] = {.lex_state = 35},
  [48] = {.lex_state = 4},
  [49] = {.lex_state = 6},
  [50] = {.lex_state = 1},
  [51] = {.lex_state = 1},
  [52] = {.lex_state = 35},
  [53] = {.lex_state = 35},
  [54] = {.lex_state = 37},
  [55] = {.lex_state = 37},
  [56] = {.lex_state = 37},
  [57] = {.lex_state = 4},
  [58] = {.lex_state = 37},
  [59] = {.lex_state = 37},
  [60] = {.lex_state = 4},
  [61] = {.lex_state = 37},
  [62] = {.lex_state = 37},
  [63] = {.lex_state = 3},
  [64] = {.lex_state = 37},
  [65] = {.lex_state = 37},
  [66] = {.lex_state = 37},
  [67] = {.lex_state = 37},
  [68] = {.lex_state = 37},
  [69] = {.lex_state = 0},
  [70] = {.lex_state = 3},
  [71] = {.lex_state = 0},
  [72] = {.lex_state = 0},
  [73] = {.lex_state = 3},
  [74] = {.lex_state = 3},
  [75] = {.lex_state = 4},
  [76] = {.lex_state = 3},
  [77] = {.lex_state = 37},
  [78] = {.lex_state = 3},
  [79] = {.lex_state = 3},
  [80] = {.lex_state = 3},
  [81] = {.lex_state = 37},
  [82] = {.lex_state = 4},
  [83] = {.lex_state = 0},
  [84] = {.lex_state = 37},
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 0},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 33},
  [89] = {.lex_state = 0},
  [90] = {.lex_state = 33},
  [91] = {.lex_state = 33},
  [92] = {.lex_state = 0},
  [93] = {.lex_state = 33},
  [94] = {.lex_state = 52},
  [95] = {.lex_state = 0},
  [96] = {.lex_state = 0},
  [97] = {.lex_state = 0},
  [98] = {.lex_state = 34},
  [99] = {.lex_state = 0},
  [100] = {.lex_state = 0},
  [101] = {.lex_state = 52},
  [102] = {.lex_state = 33},
  [103] = {.lex_state = 52},
  [104] = {.lex_state = 0},
  [105] = {.lex_state = 52},
  [106] = {.lex_state = 0},
  [107] = {.lex_state = 0},
  [108] = {.lex_state = 0},
  [109] = {.lex_state = 0},
  [110] = {.lex_state = 33},
  [111] = {.lex_state = 33},
  [112] = {.lex_state = 33},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_in] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [sym_unit] = ACTIONS(1),
    [aux_sym_type_token1] = ACTIONS(1),
    [aux_sym_type_token2] = ACTIONS(1),
    [aux_sym_type_token3] = ACTIONS(1),
    [aux_sym_type_token4] = ACTIONS(1),
    [aux_sym_type_token5] = ACTIONS(1),
    [aux_sym_type_token6] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(95),
    [sym_fn_def] = STATE(61),
    [sym_type_def] = STATE(61),
    [aux_sym_source_file_repeat1] = STATE(61),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_type] = ACTIONS(7),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(9), 1,
      anon_sym_let,
    ACTIONS(14), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_unit,
    ACTIONS(20), 1,
      sym_identifier,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(14), 1,
      sym_expression,
    STATE(2), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
    ACTIONS(12), 5,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
  [36] = 5,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(14), 1,
      sym_expression,
    STATE(2), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
    ACTIONS(23), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [64] = 10,
    ACTIONS(27), 1,
      anon_sym_let,
    ACTIONS(30), 1,
      anon_sym_DQUOTE,
    ACTIONS(33), 1,
      sym_unit,
    ACTIONS(36), 1,
      sym_identifier,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(17), 1,
      sym_expression,
    ACTIONS(25), 2,
      ts_builtin_sym_end,
      anon_sym_,
    STATE(4), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    ACTIONS(12), 3,
      anon_sym_type,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [102] = 6,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(17), 1,
      sym_expression,
    ACTIONS(39), 2,
      ts_builtin_sym_end,
      anon_sym_,
    STATE(4), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
    ACTIONS(23), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [132] = 10,
    ACTIONS(12), 1,
      anon_sym_type,
    ACTIONS(27), 1,
      anon_sym_let,
    ACTIONS(36), 1,
      sym_identifier,
    ACTIONS(41), 1,
      anon_sym_DQUOTE,
    ACTIONS(44), 1,
      sym_unit,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(19), 1,
      sym_expression,
    STATE(6), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    ACTIONS(25), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [169] = 10,
    ACTIONS(9), 1,
      anon_sym_let,
    ACTIONS(14), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_unit,
    ACTIONS(20), 1,
      sym_identifier,
    ACTIONS(25), 1,
      anon_sym_LF,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(16), 1,
      sym_expression,
    STATE(7), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    ACTIONS(12), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [206] = 10,
    ACTIONS(39), 1,
      anon_sym_LF,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(49), 1,
      anon_sym_DQUOTE,
    ACTIONS(51), 1,
      sym_unit,
    ACTIONS(53), 1,
      sym_identifier,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(16), 1,
      sym_expression,
    STATE(7), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    ACTIONS(23), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [243] = 9,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(19), 1,
      sym_expression,
    ACTIONS(23), 2,
      anon_sym_let,
      anon_sym_type,
    STATE(6), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    ACTIONS(39), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [278] = 8,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(53), 1,
      sym_identifier,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    ACTIONS(63), 1,
      sym_unit,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(14), 1,
      sym_expression,
    STATE(3), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [307] = 8,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(53), 1,
      sym_identifier,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    ACTIONS(63), 1,
      sym_unit,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(16), 1,
      sym_expression,
    STATE(8), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [336] = 8,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    ACTIONS(65), 1,
      anon_sym_let,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(17), 1,
      sym_expression,
    STATE(5), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [365] = 8,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    ACTIONS(65), 1,
      anon_sym_let,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(19), 1,
      sym_expression,
    STATE(9), 2,
      sym_function_args,
      aux_sym_function_call_repeat1,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [394] = 3,
    STATE(30), 1,
      aux_sym_function_args_repeat1,
    STATE(32), 1,
      sym_infix_operator,
    ACTIONS(67), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [412] = 4,
    ACTIONS(73), 1,
      anon_sym_DOT,
    STATE(57), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(69), 4,
      anon_sym_let,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
    ACTIONS(71), 5,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
  [432] = 4,
    ACTIONS(75), 1,
      anon_sym_,
    STATE(32), 1,
      sym_infix_operator,
    STATE(35), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(67), 8,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [452] = 4,
    STATE(24), 1,
      aux_sym_function_args_repeat1,
    STATE(48), 1,
      sym_infix_operator,
    ACTIONS(77), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(67), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [472] = 5,
    ACTIONS(73), 1,
      anon_sym_DOT,
    STATE(57), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(79), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(71), 3,
      anon_sym_type,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(69), 4,
      anon_sym_let,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [494] = 5,
    ACTIONS(77), 1,
      ts_builtin_sym_end,
    ACTIONS(81), 1,
      anon_sym_,
    STATE(47), 1,
      aux_sym_function_args_repeat1,
    STATE(48), 1,
      sym_infix_operator,
    ACTIONS(67), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [516] = 2,
    STATE(32), 1,
      sym_infix_operator,
    ACTIONS(83), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [531] = 3,
    STATE(32), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(85), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [548] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(73), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [573] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(63), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [598] = 3,
    STATE(33), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(97), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(99), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [615] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(76), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [640] = 7,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(53), 1,
      sym_identifier,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    ACTIONS(63), 1,
      sym_unit,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(21), 1,
      sym_expression,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [665] = 3,
    STATE(32), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [682] = 7,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(53), 1,
      sym_identifier,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    ACTIONS(63), 1,
      sym_unit,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(27), 1,
      sym_expression,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [707] = 7,
    ACTIONS(103), 1,
      anon_sym_let,
    ACTIONS(105), 1,
      anon_sym_DQUOTE,
    ACTIONS(107), 1,
      sym_unit,
    ACTIONS(109), 1,
      sym_identifier,
    STATE(13), 1,
      sym_fn_call_identifier,
    STATE(62), 1,
      sym_expression,
    STATE(77), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [732] = 2,
    STATE(44), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(99), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [747] = 7,
    ACTIONS(103), 1,
      anon_sym_let,
    ACTIONS(105), 1,
      anon_sym_DQUOTE,
    ACTIONS(107), 1,
      sym_unit,
    ACTIONS(109), 1,
      sym_identifier,
    STATE(13), 1,
      sym_fn_call_identifier,
    STATE(58), 1,
      sym_expression,
    STATE(77), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [772] = 7,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(53), 1,
      sym_identifier,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    ACTIONS(63), 1,
      sym_unit,
    STATE(10), 1,
      sym_fn_call_identifier,
    STATE(20), 1,
      sym_expression,
    STATE(50), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [797] = 4,
    ACTIONS(111), 1,
      ts_builtin_sym_end,
    ACTIONS(113), 1,
      anon_sym_,
    STATE(33), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(85), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [816] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(78), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [841] = 3,
    ACTIONS(75), 1,
      anon_sym_,
    STATE(44), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(99), 8,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [858] = 6,
    ACTIONS(71), 1,
      anon_sym_type,
    ACTIONS(118), 1,
      anon_sym_DOT,
    STATE(57), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(69), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(116), 2,
      anon_sym_DQUOTE,
      sym_unit,
    ACTIONS(79), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
  [881] = 4,
    STATE(48), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(111), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(85), 5,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [900] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(74), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [925] = 5,
    ACTIONS(73), 1,
      anon_sym_DOT,
    ACTIONS(79), 1,
      anon_sym_LF,
    STATE(57), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(71), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(69), 4,
      anon_sym_let,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [946] = 4,
    STATE(48), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(120), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(101), 5,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [965] = 7,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    ACTIONS(65), 1,
      anon_sym_let,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(40), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [990] = 7,
    ACTIONS(103), 1,
      anon_sym_let,
    ACTIONS(105), 1,
      anon_sym_DQUOTE,
    ACTIONS(107), 1,
      sym_unit,
    ACTIONS(109), 1,
      sym_identifier,
    STATE(13), 1,
      sym_fn_call_identifier,
    STATE(56), 1,
      sym_expression,
    STATE(77), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [1015] = 7,
    ACTIONS(89), 1,
      anon_sym_let,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      sym_unit,
    ACTIONS(95), 1,
      sym_identifier,
    STATE(11), 1,
      sym_fn_call_identifier,
    STATE(70), 1,
      sym_expression,
    STATE(80), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [1040] = 3,
    ACTIONS(122), 1,
      anon_sym_,
    STATE(44), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(85), 8,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [1057] = 3,
    STATE(48), 1,
      sym_infix_operator,
    ACTIONS(125), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(83), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [1074] = 7,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    ACTIONS(65), 1,
      anon_sym_let,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(37), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [1099] = 4,
    ACTIONS(81), 1,
      anon_sym_,
    ACTIONS(97), 1,
      ts_builtin_sym_end,
    STATE(33), 1,
      aux_sym_function_args_repeat1,
    ACTIONS(99), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [1118] = 7,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(57), 1,
      sym_unit,
    ACTIONS(59), 1,
      sym_identifier,
    ACTIONS(65), 1,
      anon_sym_let,
    STATE(12), 1,
      sym_fn_call_identifier,
    STATE(45), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [1143] = 3,
    ACTIONS(129), 1,
      sym_identifier,
    STATE(84), 2,
      sym_qualified_name,
      sym_type,
    ACTIONS(127), 6,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
      aux_sym_type_token6,
  [1159] = 1,
    ACTIONS(71), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [1171] = 1,
    ACTIONS(131), 9,
      anon_sym_let,
      anon_sym_in,
      anon_sym_LF,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_,
      sym_unit,
      sym_identifier,
  [1183] = 2,
    ACTIONS(133), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(131), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [1197] = 2,
    ACTIONS(79), 2,
      ts_builtin_sym_end,
      anon_sym_,
    ACTIONS(71), 7,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [1211] = 2,
    STATE(89), 1,
      sym_type,
    ACTIONS(135), 6,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
      aux_sym_type_token6,
  [1223] = 2,
    STATE(104), 1,
      sym_type,
    ACTIONS(135), 6,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
      aux_sym_type_token6,
  [1235] = 2,
    STATE(42), 1,
      sym_infix_operator,
    ACTIONS(125), 5,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1246] = 4,
    ACTIONS(118), 1,
      anon_sym_DOT,
    STATE(60), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(137), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(139), 2,
      anon_sym_DQUOTE,
      sym_unit,
  [1261] = 3,
    STATE(42), 1,
      sym_infix_operator,
    ACTIONS(141), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(120), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1274] = 4,
    ACTIONS(143), 1,
      ts_builtin_sym_end,
    ACTIONS(145), 1,
      anon_sym_let,
    ACTIONS(148), 1,
      anon_sym_type,
    STATE(59), 3,
      sym_fn_def,
      sym_type_def,
      aux_sym_source_file_repeat1,
  [1289] = 4,
    ACTIONS(155), 1,
      anon_sym_DOT,
    STATE(60), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(151), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(153), 2,
      anon_sym_DQUOTE,
      sym_unit,
  [1304] = 4,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(7), 1,
      anon_sym_type,
    ACTIONS(158), 1,
      ts_builtin_sym_end,
    STATE(59), 3,
      sym_fn_def,
      sym_type_def,
      aux_sym_source_file_repeat1,
  [1319] = 3,
    STATE(42), 1,
      sym_infix_operator,
    ACTIONS(141), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(160), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1332] = 4,
    ACTIONS(162), 1,
      anon_sym_in,
    ACTIONS(164), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1346] = 3,
    ACTIONS(168), 1,
      anon_sym_DOT,
    STATE(68), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(166), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1358] = 3,
    ACTIONS(168), 1,
      anon_sym_DOT,
    STATE(64), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(170), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1370] = 1,
    ACTIONS(172), 5,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_EQ,
      anon_sym_type,
      anon_sym_RPAREN,
  [1378] = 1,
    ACTIONS(133), 5,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1386] = 3,
    ACTIONS(174), 1,
      anon_sym_DOT,
    STATE(68), 1,
      aux_sym_qualified_name_repeat1,
    ACTIONS(153), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1398] = 4,
    ACTIONS(177), 1,
      anon_sym_COLON,
    ACTIONS(179), 1,
      anon_sym_LPAREN,
    ACTIONS(182), 1,
      sym_unit,
    STATE(69), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [1412] = 4,
    ACTIONS(185), 1,
      anon_sym_in,
    ACTIONS(187), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1426] = 4,
    ACTIONS(189), 1,
      anon_sym_LPAREN,
    ACTIONS(191), 1,
      sym_unit,
    STATE(97), 1,
      sym_fn_params_def,
    STATE(72), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [1440] = 4,
    ACTIONS(189), 1,
      anon_sym_LPAREN,
    ACTIONS(191), 1,
      sym_unit,
    ACTIONS(193), 1,
      anon_sym_COLON,
    STATE(69), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [1454] = 4,
    ACTIONS(195), 1,
      anon_sym_in,
    ACTIONS(197), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1468] = 3,
    ACTIONS(125), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(83), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1480] = 2,
    ACTIONS(151), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(153), 3,
      anon_sym_DQUOTE,
      anon_sym_DOT,
      sym_unit,
  [1490] = 4,
    ACTIONS(199), 1,
      anon_sym_in,
    ACTIONS(201), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1504] = 1,
    ACTIONS(79), 5,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1512] = 4,
    ACTIONS(101), 1,
      anon_sym_in,
    ACTIONS(120), 1,
      anon_sym_LF,
    STATE(38), 1,
      sym_infix_operator,
    ACTIONS(87), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1526] = 2,
    ACTIONS(133), 1,
      anon_sym_LF,
    ACTIONS(131), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1535] = 2,
    ACTIONS(79), 1,
      anon_sym_LF,
    ACTIONS(71), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1544] = 1,
    ACTIONS(153), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
      anon_sym_DOT,
  [1551] = 2,
    ACTIONS(203), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(205), 2,
      anon_sym_DQUOTE,
      sym_unit,
  [1560] = 2,
    ACTIONS(209), 1,
      anon_sym_LPAREN,
    ACTIONS(207), 2,
      anon_sym_COLON,
      sym_unit,
  [1568] = 1,
    ACTIONS(211), 3,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_type,
  [1574] = 2,
    ACTIONS(215), 1,
      anon_sym_LPAREN,
    ACTIONS(213), 2,
      anon_sym_COLON,
      sym_unit,
  [1582] = 1,
    ACTIONS(217), 1,
      anon_sym_DQUOTE,
  [1586] = 1,
    ACTIONS(219), 1,
      anon_sym_EQ,
  [1590] = 1,
    ACTIONS(221), 1,
      sym_identifier,
  [1594] = 1,
    ACTIONS(223), 1,
      anon_sym_RPAREN,
  [1598] = 1,
    ACTIONS(225), 1,
      sym_identifier,
  [1602] = 1,
    ACTIONS(227), 1,
      sym_identifier,
  [1606] = 1,
    ACTIONS(229), 1,
      anon_sym_DQUOTE,
  [1610] = 1,
    ACTIONS(231), 1,
      sym_identifier,
  [1614] = 1,
    ACTIONS(233), 1,
      aux_sym_string_literal_token1,
  [1618] = 1,
    ACTIONS(235), 1,
      ts_builtin_sym_end,
  [1622] = 1,
    ACTIONS(237), 1,
      anon_sym_DQUOTE,
  [1626] = 1,
    ACTIONS(239), 1,
      anon_sym_COLON,
  [1630] = 1,
    ACTIONS(241), 1,
      sym_type_identifier,
  [1634] = 1,
    ACTIONS(243), 1,
      anon_sym_DQUOTE,
  [1638] = 1,
    ACTIONS(245), 1,
      anon_sym_EQ,
  [1642] = 1,
    ACTIONS(247), 1,
      aux_sym_string_literal_token1,
  [1646] = 1,
    ACTIONS(249), 1,
      sym_identifier,
  [1650] = 1,
    ACTIONS(251), 1,
      aux_sym_string_literal_token1,
  [1654] = 1,
    ACTIONS(253), 1,
      anon_sym_EQ,
  [1658] = 1,
    ACTIONS(255), 1,
      aux_sym_string_literal_token1,
  [1662] = 1,
    ACTIONS(257), 1,
      anon_sym_COLON,
  [1666] = 1,
    ACTIONS(259), 1,
      anon_sym_EQ,
  [1670] = 1,
    ACTIONS(261), 1,
      anon_sym_EQ,
  [1674] = 1,
    ACTIONS(263), 1,
      anon_sym_EQ,
  [1678] = 1,
    ACTIONS(265), 1,
      sym_identifier,
  [1682] = 1,
    ACTIONS(267), 1,
      sym_identifier,
  [1686] = 1,
    ACTIONS(269), 1,
      sym_identifier,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 36,
  [SMALL_STATE(4)] = 64,
  [SMALL_STATE(5)] = 102,
  [SMALL_STATE(6)] = 132,
  [SMALL_STATE(7)] = 169,
  [SMALL_STATE(8)] = 206,
  [SMALL_STATE(9)] = 243,
  [SMALL_STATE(10)] = 278,
  [SMALL_STATE(11)] = 307,
  [SMALL_STATE(12)] = 336,
  [SMALL_STATE(13)] = 365,
  [SMALL_STATE(14)] = 394,
  [SMALL_STATE(15)] = 412,
  [SMALL_STATE(16)] = 432,
  [SMALL_STATE(17)] = 452,
  [SMALL_STATE(18)] = 472,
  [SMALL_STATE(19)] = 494,
  [SMALL_STATE(20)] = 516,
  [SMALL_STATE(21)] = 531,
  [SMALL_STATE(22)] = 548,
  [SMALL_STATE(23)] = 573,
  [SMALL_STATE(24)] = 598,
  [SMALL_STATE(25)] = 615,
  [SMALL_STATE(26)] = 640,
  [SMALL_STATE(27)] = 665,
  [SMALL_STATE(28)] = 682,
  [SMALL_STATE(29)] = 707,
  [SMALL_STATE(30)] = 732,
  [SMALL_STATE(31)] = 747,
  [SMALL_STATE(32)] = 772,
  [SMALL_STATE(33)] = 797,
  [SMALL_STATE(34)] = 816,
  [SMALL_STATE(35)] = 841,
  [SMALL_STATE(36)] = 858,
  [SMALL_STATE(37)] = 881,
  [SMALL_STATE(38)] = 900,
  [SMALL_STATE(39)] = 925,
  [SMALL_STATE(40)] = 946,
  [SMALL_STATE(41)] = 965,
  [SMALL_STATE(42)] = 990,
  [SMALL_STATE(43)] = 1015,
  [SMALL_STATE(44)] = 1040,
  [SMALL_STATE(45)] = 1057,
  [SMALL_STATE(46)] = 1074,
  [SMALL_STATE(47)] = 1099,
  [SMALL_STATE(48)] = 1118,
  [SMALL_STATE(49)] = 1143,
  [SMALL_STATE(50)] = 1159,
  [SMALL_STATE(51)] = 1171,
  [SMALL_STATE(52)] = 1183,
  [SMALL_STATE(53)] = 1197,
  [SMALL_STATE(54)] = 1211,
  [SMALL_STATE(55)] = 1223,
  [SMALL_STATE(56)] = 1235,
  [SMALL_STATE(57)] = 1246,
  [SMALL_STATE(58)] = 1261,
  [SMALL_STATE(59)] = 1274,
  [SMALL_STATE(60)] = 1289,
  [SMALL_STATE(61)] = 1304,
  [SMALL_STATE(62)] = 1319,
  [SMALL_STATE(63)] = 1332,
  [SMALL_STATE(64)] = 1346,
  [SMALL_STATE(65)] = 1358,
  [SMALL_STATE(66)] = 1370,
  [SMALL_STATE(67)] = 1378,
  [SMALL_STATE(68)] = 1386,
  [SMALL_STATE(69)] = 1398,
  [SMALL_STATE(70)] = 1412,
  [SMALL_STATE(71)] = 1426,
  [SMALL_STATE(72)] = 1440,
  [SMALL_STATE(73)] = 1454,
  [SMALL_STATE(74)] = 1468,
  [SMALL_STATE(75)] = 1480,
  [SMALL_STATE(76)] = 1490,
  [SMALL_STATE(77)] = 1504,
  [SMALL_STATE(78)] = 1512,
  [SMALL_STATE(79)] = 1526,
  [SMALL_STATE(80)] = 1535,
  [SMALL_STATE(81)] = 1544,
  [SMALL_STATE(82)] = 1551,
  [SMALL_STATE(83)] = 1560,
  [SMALL_STATE(84)] = 1568,
  [SMALL_STATE(85)] = 1574,
  [SMALL_STATE(86)] = 1582,
  [SMALL_STATE(87)] = 1586,
  [SMALL_STATE(88)] = 1590,
  [SMALL_STATE(89)] = 1594,
  [SMALL_STATE(90)] = 1598,
  [SMALL_STATE(91)] = 1602,
  [SMALL_STATE(92)] = 1606,
  [SMALL_STATE(93)] = 1610,
  [SMALL_STATE(94)] = 1614,
  [SMALL_STATE(95)] = 1618,
  [SMALL_STATE(96)] = 1622,
  [SMALL_STATE(97)] = 1626,
  [SMALL_STATE(98)] = 1630,
  [SMALL_STATE(99)] = 1634,
  [SMALL_STATE(100)] = 1638,
  [SMALL_STATE(101)] = 1642,
  [SMALL_STATE(102)] = 1646,
  [SMALL_STATE(103)] = 1650,
  [SMALL_STATE(104)] = 1654,
  [SMALL_STATE(105)] = 1658,
  [SMALL_STATE(106)] = 1662,
  [SMALL_STATE(107)] = 1666,
  [SMALL_STATE(108)] = 1670,
  [SMALL_STATE(109)] = 1674,
  [SMALL_STATE(110)] = 1678,
  [SMALL_STATE(111)] = 1682,
  [SMALL_STATE(112)] = 1686,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(88),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [9] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(112),
  [12] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2),
  [14] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(105),
  [17] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(50),
  [20] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(15),
  [23] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_call, 2, .production_id = 4),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2),
  [27] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(110),
  [30] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(101),
  [33] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(53),
  [36] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(18),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 2, .production_id = 4),
  [41] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(101),
  [44] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(53),
  [47] = {.entry = {.count = 1, .reusable = false}}, SHIFT(112),
  [49] = {.entry = {.count = 1, .reusable = false}}, SHIFT(105),
  [51] = {.entry = {.count = 1, .reusable = false}}, SHIFT(50),
  [53] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(101),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [59] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(105),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [65] = {.entry = {.count = 1, .reusable = false}}, SHIFT(110),
  [67] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_args, 1),
  [69] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_call_identifier, 1),
  [71] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1),
  [73] = {.entry = {.count = 1, .reusable = false}}, SHIFT(90),
  [75] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_args, 1),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [83] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operation, 3, .production_id = 6),
  [85] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_function_args_repeat1, 2),
  [87] = {.entry = {.count = 1, .reusable = false}}, SHIFT(82),
  [89] = {.entry = {.count = 1, .reusable = false}}, SHIFT(111),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_args, 2),
  [99] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_args, 2),
  [101] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_expression, 6, .production_id = 7),
  [103] = {.entry = {.count = 1, .reusable = false}}, SHIFT(93),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [109] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_args_repeat1, 2),
  [113] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_args_repeat1, 2), SHIFT_REPEAT(46),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_call_identifier, 1),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_expression, 6, .production_id = 7),
  [122] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_args_repeat1, 2), SHIFT_REPEAT(26),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operation, 3, .production_id = 6),
  [127] = {.entry = {.count = 1, .reusable = false}}, SHIFT(66),
  [129] = {.entry = {.count = 1, .reusable = false}}, SHIFT(65),
  [131] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 3, .production_id = 5),
  [133] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 3, .production_id = 5),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [137] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_call_identifier, 2),
  [139] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_call_identifier, 2),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [145] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(88),
  [148] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(98),
  [151] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_qualified_name_repeat1, 2),
  [153] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_name_repeat1, 2),
  [155] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_name_repeat1, 2), SHIFT_REPEAT(90),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_def, 7, .production_id = 3),
  [162] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_name, 2),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_name, 1),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1),
  [174] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_name_repeat1, 2), SHIFT_REPEAT(102),
  [177] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2),
  [179] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(91),
  [182] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(83),
  [185] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [187] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [189] = {.entry = {.count = 1, .reusable = false}}, SHIFT(91),
  [191] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [193] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_params_def, 1),
  [195] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [197] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [199] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [203] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operator, 1),
  [205] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operator, 1),
  [207] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 1),
  [209] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 1),
  [211] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_def, 4, .production_id = 1),
  [213] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 5, .production_id = 2),
  [215] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 5, .production_id = 2),
  [217] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [219] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [221] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [223] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [225] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [227] = {.entry = {.count = 1, .reusable = true}}, SHIFT(106),
  [229] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [231] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [233] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [235] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [237] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [239] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [241] = {.entry = {.count = 1, .reusable = true}}, SHIFT(87),
  [243] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [245] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [247] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [249] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [251] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [253] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [255] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [257] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [259] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [261] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [263] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [265] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [267] = {.entry = {.count = 1, .reusable = true}}, SHIFT(108),
  [269] = {.entry = {.count = 1, .reusable = true}}, SHIFT(109),
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
