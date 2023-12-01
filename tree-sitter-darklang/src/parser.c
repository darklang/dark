#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 82
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 34
#define ALIAS_COUNT 0
#define TOKEN_COUNT 20
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 13
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 7

enum {
  anon_sym_let = 1,
  anon_sym_COLON = 2,
  anon_sym_EQ = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_in = 6,
  anon_sym_LF = 7,
  anon_sym_DQUOTE = 8,
  aux_sym_string_literal_token1 = 9,
  anon_sym_PLUS = 10,
  anon_sym_DASH = 11,
  sym_unit = 12,
  aux_sym_type_token1 = 13,
  aux_sym_type_token2 = 14,
  aux_sym_type_token3 = 15,
  aux_sym_type_token4 = 16,
  aux_sym_type_token5 = 17,
  aux_sym_type_token6 = 18,
  sym_identifier = 19,
  sym_source_file = 20,
  sym_fn_def = 21,
  sym_fn_params_def = 22,
  sym_fn_param_def = 23,
  sym_expression = 24,
  sym_let_expression = 25,
  sym_function_call = 26,
  sym_string_literal = 27,
  sym_infix_operator = 28,
  sym_infix_operation = 29,
  sym_type = 30,
  aux_sym_source_file_repeat1 = 31,
  aux_sym_fn_params_def_repeat1 = 32,
  aux_sym_function_call_repeat1 = 33,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_let] = "let",
  [anon_sym_COLON] = ":",
  [anon_sym_EQ] = "=",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_in] = "in",
  [anon_sym_LF] = "\n",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_literal_token1] = "string_literal_token1",
  [anon_sym_PLUS] = "+",
  [anon_sym_DASH] = "-",
  [sym_unit] = "unit",
  [aux_sym_type_token1] = "type_token1",
  [aux_sym_type_token2] = "type_token2",
  [aux_sym_type_token3] = "type_token3",
  [aux_sym_type_token4] = "type_token4",
  [aux_sym_type_token5] = "type_token5",
  [aux_sym_type_token6] = "type_token6",
  [sym_identifier] = "identifier",
  [sym_source_file] = "source_file",
  [sym_fn_def] = "fn_def",
  [sym_fn_params_def] = "fn_params_def",
  [sym_fn_param_def] = "fn_param_def",
  [sym_expression] = "expression",
  [sym_let_expression] = "let_expression",
  [sym_function_call] = "function_call",
  [sym_string_literal] = "string_literal",
  [sym_infix_operator] = "infix_operator",
  [sym_infix_operation] = "infix_operation",
  [sym_type] = "type",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_fn_params_def_repeat1] = "fn_params_def_repeat1",
  [aux_sym_function_call_repeat1] = "function_call_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_in] = anon_sym_in,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_literal_token1] = aux_sym_string_literal_token1,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_DASH,
  [sym_unit] = sym_unit,
  [aux_sym_type_token1] = aux_sym_type_token1,
  [aux_sym_type_token2] = aux_sym_type_token2,
  [aux_sym_type_token3] = aux_sym_type_token3,
  [aux_sym_type_token4] = aux_sym_type_token4,
  [aux_sym_type_token5] = aux_sym_type_token5,
  [aux_sym_type_token6] = aux_sym_type_token6,
  [sym_identifier] = sym_identifier,
  [sym_source_file] = sym_source_file,
  [sym_fn_def] = sym_fn_def,
  [sym_fn_params_def] = sym_fn_params_def,
  [sym_fn_param_def] = sym_fn_param_def,
  [sym_expression] = sym_expression,
  [sym_let_expression] = sym_let_expression,
  [sym_function_call] = sym_function_call,
  [sym_string_literal] = sym_string_literal,
  [sym_infix_operator] = sym_infix_operator,
  [sym_infix_operation] = sym_infix_operation,
  [sym_type] = sym_type,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_fn_params_def_repeat1] = aux_sym_fn_params_def_repeat1,
  [aux_sym_function_call_repeat1] = aux_sym_function_call_repeat1,
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
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_fn_def] = {
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
  [sym_type] = {
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
  [2] = {.index = 2, .length = 4},
  [3] = {.index = 6, .length = 2},
  [4] = {.index = 8, .length = 1},
  [5] = {.index = 9, .length = 3},
  [6] = {.index = 12, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_identifier, 1},
    {field_typ, 3},
  [2] =
    {field_body, 6},
    {field_name, 1},
    {field_params, 2},
    {field_return_type, 4},
  [6] =
    {field_args, 1},
    {field_fn, 0},
  [8] =
    {field_value, 1},
  [9] =
    {field_left, 0},
    {field_operator, 1},
    {field_right, 2},
  [12] =
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
  [4] = 3,
  [5] = 5,
  [6] = 2,
  [7] = 3,
  [8] = 2,
  [9] = 3,
  [10] = 5,
  [11] = 2,
  [12] = 12,
  [13] = 12,
  [14] = 14,
  [15] = 14,
  [16] = 14,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 12,
  [21] = 21,
  [22] = 19,
  [23] = 12,
  [24] = 19,
  [25] = 25,
  [26] = 19,
  [27] = 14,
  [28] = 17,
  [29] = 29,
  [30] = 30,
  [31] = 25,
  [32] = 18,
  [33] = 30,
  [34] = 34,
  [35] = 35,
  [36] = 29,
  [37] = 37,
  [38] = 37,
  [39] = 17,
  [40] = 18,
  [41] = 37,
  [42] = 17,
  [43] = 43,
  [44] = 44,
  [45] = 18,
  [46] = 37,
  [47] = 47,
  [48] = 48,
  [49] = 30,
  [50] = 50,
  [51] = 29,
  [52] = 29,
  [53] = 30,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 62,
  [64] = 64,
  [65] = 65,
  [66] = 62,
  [67] = 67,
  [68] = 62,
  [69] = 60,
  [70] = 70,
  [71] = 71,
  [72] = 60,
  [73] = 73,
  [74] = 60,
  [75] = 75,
  [76] = 71,
  [77] = 71,
  [78] = 71,
  [79] = 67,
  [80] = 67,
  [81] = 67,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(30);
      if (lookahead == '"') ADVANCE(40);
      if (lookahead == '(') ADVANCE(35);
      if (lookahead == ')') ADVANCE(36);
      if (lookahead == '+') ADVANCE(43);
      if (lookahead == '-') ADVANCE(44);
      if (lookahead == ':') ADVANCE(33);
      if (lookahead == '=') ADVANCE(34);
      if (lookahead == 'B') ADVANCE(71);
      if (lookahead == 'C') ADVANCE(62);
      if (lookahead == 'F') ADVANCE(66);
      if (lookahead == 'I') ADVANCE(70);
      if (lookahead == 'S') ADVANCE(76);
      if (lookahead == 'U') ADVANCE(67);
      if (lookahead == 'i') ADVANCE(68);
      if (lookahead == 'l') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(39);
      if (lookahead == '"') ADVANCE(40);
      if (lookahead == '(') ADVANCE(3);
      if (lookahead == '+') ADVANCE(43);
      if (lookahead == '-') ADVANCE(44);
      if (lookahead == 'i') ADVANCE(68);
      if (lookahead == 'l') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(39);
      if (lookahead == '+') ADVANCE(43);
      if (lookahead == '-') ADVANCE(44);
      if (lookahead == 'i') ADVANCE(15);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == ')') ADVANCE(45);
      END_STATE();
    case 4:
      if (lookahead == 'a') ADVANCE(20);
      END_STATE();
    case 5:
      if (lookahead == 'a') ADVANCE(26);
      END_STATE();
    case 6:
      if (lookahead == 'e') ADVANCE(24);
      END_STATE();
    case 7:
      if (lookahead == 'g') ADVANCE(54);
      END_STATE();
    case 8:
      if (lookahead == 'h') ADVANCE(4);
      END_STATE();
    case 9:
      if (lookahead == 'i') ADVANCE(14);
      END_STATE();
    case 10:
      if (lookahead == 'i') ADVANCE(25);
      END_STATE();
    case 11:
      if (lookahead == 'l') ADVANCE(50);
      END_STATE();
    case 12:
      if (lookahead == 'l') ADVANCE(19);
      END_STATE();
    case 13:
      if (lookahead == 'n') ADVANCE(10);
      END_STATE();
    case 14:
      if (lookahead == 'n') ADVANCE(7);
      END_STATE();
    case 15:
      if (lookahead == 'n') ADVANCE(37);
      END_STATE();
    case 16:
      if (lookahead == 'n') ADVANCE(23);
      END_STATE();
    case 17:
      if (lookahead == 'o') ADVANCE(18);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(11);
      END_STATE();
    case 19:
      if (lookahead == 'o') ADVANCE(5);
      END_STATE();
    case 20:
      if (lookahead == 'r') ADVANCE(56);
      END_STATE();
    case 21:
      if (lookahead == 'r') ADVANCE(9);
      END_STATE();
    case 22:
      if (lookahead == 't') ADVANCE(21);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(48);
      END_STATE();
    case 24:
      if (lookahead == 't') ADVANCE(31);
      END_STATE();
    case 25:
      if (lookahead == 't') ADVANCE(46);
      END_STATE();
    case 26:
      if (lookahead == 't') ADVANCE(52);
      END_STATE();
    case 27:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(27)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 28:
      if (eof) ADVANCE(30);
      if (lookahead == '"') ADVANCE(40);
      if (lookahead == '(') ADVANCE(3);
      if (lookahead == '+') ADVANCE(43);
      if (lookahead == '-') ADVANCE(44);
      if (lookahead == 'l') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(28)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 29:
      if (eof) ADVANCE(30);
      if (lookahead == '+') ADVANCE(43);
      if (lookahead == '-') ADVANCE(44);
      if (lookahead == 'B') ADVANCE(17);
      if (lookahead == 'C') ADVANCE(8);
      if (lookahead == 'F') ADVANCE(12);
      if (lookahead == 'I') ADVANCE(16);
      if (lookahead == 'S') ADVANCE(22);
      if (lookahead == 'U') ADVANCE(13);
      if (lookahead == 'l') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(29)
      END_STATE();
    case 30:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')') ADVANCE(45);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_in);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_in);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(39);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(41);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(42);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(42);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_unit);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(aux_sym_type_token1);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(aux_sym_type_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(aux_sym_type_token2);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(aux_sym_type_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(aux_sym_type_token3);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(aux_sym_type_token3);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(aux_sym_type_token4);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(aux_sym_type_token4);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(aux_sym_type_token5);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(aux_sym_type_token5);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(aux_sym_type_token6);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(aux_sym_type_token6);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(74);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(78);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'g') ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(69);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(79);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(51);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(38);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(61);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(77);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(59);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(57);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(75);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(49);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(32);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(47);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 29},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 1},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 28},
  [8] = {.lex_state = 28},
  [9] = {.lex_state = 28},
  [10] = {.lex_state = 28},
  [11] = {.lex_state = 28},
  [12] = {.lex_state = 28},
  [13] = {.lex_state = 28},
  [14] = {.lex_state = 28},
  [15] = {.lex_state = 28},
  [16] = {.lex_state = 28},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 28},
  [20] = {.lex_state = 28},
  [21] = {.lex_state = 28},
  [22] = {.lex_state = 28},
  [23] = {.lex_state = 28},
  [24] = {.lex_state = 28},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 28},
  [27] = {.lex_state = 28},
  [28] = {.lex_state = 28},
  [29] = {.lex_state = 1},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 28},
  [32] = {.lex_state = 28},
  [33] = {.lex_state = 28},
  [34] = {.lex_state = 29},
  [35] = {.lex_state = 29},
  [36] = {.lex_state = 28},
  [37] = {.lex_state = 2},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 2},
  [40] = {.lex_state = 29},
  [41] = {.lex_state = 2},
  [42] = {.lex_state = 29},
  [43] = {.lex_state = 29},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 2},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 28},
  [51] = {.lex_state = 2},
  [52] = {.lex_state = 29},
  [53] = {.lex_state = 29},
  [54] = {.lex_state = 29},
  [55] = {.lex_state = 29},
  [56] = {.lex_state = 0},
  [57] = {.lex_state = 0},
  [58] = {.lex_state = 0},
  [59] = {.lex_state = 27},
  [60] = {.lex_state = 41},
  [61] = {.lex_state = 27},
  [62] = {.lex_state = 0},
  [63] = {.lex_state = 0},
  [64] = {.lex_state = 0},
  [65] = {.lex_state = 0},
  [66] = {.lex_state = 0},
  [67] = {.lex_state = 27},
  [68] = {.lex_state = 0},
  [69] = {.lex_state = 41},
  [70] = {.lex_state = 0},
  [71] = {.lex_state = 0},
  [72] = {.lex_state = 41},
  [73] = {.lex_state = 0},
  [74] = {.lex_state = 41},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 0},
  [77] = {.lex_state = 0},
  [78] = {.lex_state = 0},
  [79] = {.lex_state = 27},
  [80] = {.lex_state = 27},
  [81] = {.lex_state = 27},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_in] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
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
    [sym_source_file] = STATE(65),
    [sym_fn_def] = STATE(55),
    [aux_sym_source_file_repeat1] = STATE(55),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(11), 1,
      anon_sym_LF,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_unit,
    ACTIONS(17), 1,
      sym_identifier,
    STATE(3), 1,
      aux_sym_function_call_repeat1,
    STATE(25), 1,
      sym_expression,
    ACTIONS(9), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [33] = 5,
    ACTIONS(21), 1,
      anon_sym_LF,
    STATE(5), 1,
      aux_sym_function_call_repeat1,
    STATE(25), 1,
      sym_expression,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
    ACTIONS(19), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [58] = 9,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_unit,
    ACTIONS(17), 1,
      sym_identifier,
    ACTIONS(21), 1,
      anon_sym_LF,
    STATE(5), 1,
      aux_sym_function_call_repeat1,
    STATE(25), 1,
      sym_expression,
    ACTIONS(19), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [91] = 9,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(28), 1,
      anon_sym_LF,
    ACTIONS(30), 1,
      anon_sym_DQUOTE,
    ACTIONS(33), 1,
      sym_unit,
    ACTIONS(36), 1,
      sym_identifier,
    STATE(5), 1,
      aux_sym_function_call_repeat1,
    STATE(25), 1,
      sym_expression,
    ACTIONS(26), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [124] = 9,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(11), 1,
      anon_sym_LF,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      sym_unit,
    ACTIONS(17), 1,
      sym_identifier,
    STATE(4), 1,
      aux_sym_function_call_repeat1,
    STATE(25), 1,
      sym_expression,
    ACTIONS(9), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [157] = 8,
    ACTIONS(19), 1,
      anon_sym_let,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      sym_unit,
    ACTIONS(43), 1,
      sym_identifier,
    STATE(10), 1,
      aux_sym_function_call_repeat1,
    STATE(31), 1,
      sym_expression,
    ACTIONS(21), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [187] = 8,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      sym_unit,
    ACTIONS(43), 1,
      sym_identifier,
    ACTIONS(45), 1,
      anon_sym_let,
    STATE(9), 1,
      aux_sym_function_call_repeat1,
    STATE(31), 1,
      sym_expression,
    ACTIONS(11), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [217] = 5,
    STATE(10), 1,
      aux_sym_function_call_repeat1,
    STATE(31), 1,
      sym_expression,
    ACTIONS(19), 2,
      anon_sym_let,
      sym_identifier,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
    ACTIONS(21), 5,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
  [241] = 8,
    ACTIONS(47), 1,
      anon_sym_let,
    ACTIONS(50), 1,
      anon_sym_DQUOTE,
    ACTIONS(53), 1,
      sym_unit,
    ACTIONS(56), 1,
      sym_identifier,
    STATE(10), 1,
      aux_sym_function_call_repeat1,
    STATE(31), 1,
      sym_expression,
    ACTIONS(28), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [271] = 8,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      sym_unit,
    ACTIONS(43), 1,
      sym_identifier,
    ACTIONS(45), 1,
      anon_sym_let,
    STATE(7), 1,
      aux_sym_function_call_repeat1,
    STATE(31), 1,
      sym_expression,
    ACTIONS(11), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [301] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      sym_identifier,
    ACTIONS(59), 1,
      anon_sym_DQUOTE,
    ACTIONS(61), 1,
      sym_unit,
    STATE(17), 1,
      sym_expression,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [323] = 6,
    ACTIONS(63), 1,
      anon_sym_let,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    ACTIONS(67), 1,
      sym_unit,
    ACTIONS(69), 1,
      sym_identifier,
    STATE(42), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [345] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(37), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [367] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(46), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [389] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(38), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [411] = 4,
    ACTIONS(81), 1,
      anon_sym_LF,
    STATE(19), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(79), 5,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [429] = 3,
    ACTIONS(87), 1,
      anon_sym_LF,
    STATE(19), 1,
      sym_infix_operator,
    ACTIONS(85), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [445] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      sym_identifier,
    ACTIONS(59), 1,
      anon_sym_DQUOTE,
    ACTIONS(61), 1,
      sym_unit,
    STATE(18), 1,
      sym_expression,
    STATE(30), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [467] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(39), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [489] = 6,
    ACTIONS(63), 1,
      anon_sym_let,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    ACTIONS(67), 1,
      sym_unit,
    ACTIONS(69), 1,
      sym_identifier,
    STATE(43), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [511] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(45), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [533] = 6,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      sym_unit,
    ACTIONS(43), 1,
      sym_identifier,
    ACTIONS(45), 1,
      anon_sym_let,
    STATE(28), 1,
      sym_expression,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [555] = 6,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      sym_unit,
    ACTIONS(43), 1,
      sym_identifier,
    ACTIONS(45), 1,
      anon_sym_let,
    STATE(32), 1,
      sym_expression,
    STATE(33), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [577] = 4,
    ACTIONS(91), 1,
      anon_sym_LF,
    STATE(19), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(89), 5,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      sym_unit,
      sym_identifier,
  [595] = 6,
    ACTIONS(63), 1,
      anon_sym_let,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    ACTIONS(67), 1,
      sym_unit,
    ACTIONS(69), 1,
      sym_identifier,
    STATE(40), 1,
      sym_expression,
    STATE(53), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [617] = 6,
    ACTIONS(71), 1,
      anon_sym_let,
    ACTIONS(73), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_unit,
    ACTIONS(77), 1,
      sym_identifier,
    STATE(41), 1,
      sym_expression,
    STATE(49), 4,
      sym_let_expression,
      sym_function_call,
      sym_string_literal,
      sym_infix_operation,
  [639] = 4,
    STATE(24), 1,
      sym_infix_operator,
    ACTIONS(79), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(93), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(81), 3,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_unit,
  [656] = 2,
    ACTIONS(97), 1,
      anon_sym_LF,
    ACTIONS(95), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [669] = 2,
    ACTIONS(11), 1,
      anon_sym_LF,
    ACTIONS(9), 7,
      anon_sym_let,
      anon_sym_in,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
      sym_identifier,
  [682] = 4,
    STATE(24), 1,
      sym_infix_operator,
    ACTIONS(89), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(93), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(91), 3,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_unit,
  [699] = 3,
    STATE(24), 1,
      sym_infix_operator,
    ACTIONS(85), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(87), 5,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
  [714] = 2,
    ACTIONS(9), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(11), 5,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
  [726] = 2,
    STATE(70), 1,
      sym_type,
    ACTIONS(99), 6,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
      aux_sym_type_token6,
  [738] = 2,
    STATE(64), 1,
      sym_type,
    ACTIONS(99), 6,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
      aux_sym_type_token6,
  [750] = 2,
    ACTIONS(95), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(97), 5,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_unit,
  [762] = 4,
    ACTIONS(101), 1,
      anon_sym_in,
    ACTIONS(103), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [776] = 4,
    ACTIONS(105), 1,
      anon_sym_in,
    ACTIONS(107), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [790] = 4,
    ACTIONS(79), 1,
      anon_sym_in,
    ACTIONS(81), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [804] = 2,
    STATE(26), 1,
      sym_infix_operator,
    ACTIONS(87), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [814] = 4,
    ACTIONS(109), 1,
      anon_sym_in,
    ACTIONS(111), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [828] = 3,
    STATE(26), 1,
      sym_infix_operator,
    ACTIONS(81), 2,
      ts_builtin_sym_end,
      anon_sym_let,
    ACTIONS(93), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [840] = 3,
    STATE(26), 1,
      sym_infix_operator,
    ACTIONS(93), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(113), 2,
      ts_builtin_sym_end,
      anon_sym_let,
  [852] = 4,
    ACTIONS(115), 1,
      anon_sym_COLON,
    ACTIONS(117), 1,
      anon_sym_LPAREN,
    ACTIONS(120), 1,
      sym_unit,
    STATE(44), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [866] = 3,
    ACTIONS(87), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(85), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [878] = 4,
    ACTIONS(123), 1,
      anon_sym_in,
    ACTIONS(125), 1,
      anon_sym_LF,
    STATE(22), 1,
      sym_infix_operator,
    ACTIONS(83), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [892] = 4,
    ACTIONS(127), 1,
      anon_sym_COLON,
    ACTIONS(129), 1,
      anon_sym_LPAREN,
    ACTIONS(131), 1,
      sym_unit,
    STATE(44), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [906] = 4,
    ACTIONS(129), 1,
      anon_sym_LPAREN,
    ACTIONS(131), 1,
      sym_unit,
    STATE(75), 1,
      sym_fn_params_def,
    STATE(47), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [920] = 2,
    ACTIONS(11), 1,
      anon_sym_LF,
    ACTIONS(9), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [929] = 2,
    ACTIONS(133), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(135), 2,
      anon_sym_DQUOTE,
      sym_unit,
  [938] = 2,
    ACTIONS(97), 1,
      anon_sym_LF,
    ACTIONS(95), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [947] = 1,
    ACTIONS(97), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [954] = 1,
    ACTIONS(11), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [961] = 3,
    ACTIONS(137), 1,
      ts_builtin_sym_end,
    ACTIONS(139), 1,
      anon_sym_let,
    STATE(54), 2,
      sym_fn_def,
      aux_sym_source_file_repeat1,
  [972] = 3,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(142), 1,
      ts_builtin_sym_end,
    STATE(54), 2,
      sym_fn_def,
      aux_sym_source_file_repeat1,
  [983] = 2,
    ACTIONS(146), 1,
      anon_sym_LPAREN,
    ACTIONS(144), 2,
      anon_sym_COLON,
      sym_unit,
  [991] = 2,
    ACTIONS(150), 1,
      anon_sym_LPAREN,
    ACTIONS(148), 2,
      anon_sym_COLON,
      sym_unit,
  [999] = 1,
    ACTIONS(152), 2,
      anon_sym_EQ,
      anon_sym_RPAREN,
  [1004] = 1,
    ACTIONS(154), 1,
      sym_identifier,
  [1008] = 1,
    ACTIONS(156), 1,
      aux_sym_string_literal_token1,
  [1012] = 1,
    ACTIONS(158), 1,
      sym_identifier,
  [1016] = 1,
    ACTIONS(160), 1,
      anon_sym_DQUOTE,
  [1020] = 1,
    ACTIONS(162), 1,
      anon_sym_DQUOTE,
  [1024] = 1,
    ACTIONS(164), 1,
      anon_sym_RPAREN,
  [1028] = 1,
    ACTIONS(166), 1,
      ts_builtin_sym_end,
  [1032] = 1,
    ACTIONS(168), 1,
      anon_sym_DQUOTE,
  [1036] = 1,
    ACTIONS(170), 1,
      sym_identifier,
  [1040] = 1,
    ACTIONS(172), 1,
      anon_sym_DQUOTE,
  [1044] = 1,
    ACTIONS(174), 1,
      aux_sym_string_literal_token1,
  [1048] = 1,
    ACTIONS(176), 1,
      anon_sym_EQ,
  [1052] = 1,
    ACTIONS(178), 1,
      anon_sym_EQ,
  [1056] = 1,
    ACTIONS(180), 1,
      aux_sym_string_literal_token1,
  [1060] = 1,
    ACTIONS(182), 1,
      anon_sym_COLON,
  [1064] = 1,
    ACTIONS(184), 1,
      aux_sym_string_literal_token1,
  [1068] = 1,
    ACTIONS(186), 1,
      anon_sym_COLON,
  [1072] = 1,
    ACTIONS(188), 1,
      anon_sym_EQ,
  [1076] = 1,
    ACTIONS(190), 1,
      anon_sym_EQ,
  [1080] = 1,
    ACTIONS(192), 1,
      anon_sym_EQ,
  [1084] = 1,
    ACTIONS(194), 1,
      sym_identifier,
  [1088] = 1,
    ACTIONS(196), 1,
      sym_identifier,
  [1092] = 1,
    ACTIONS(198), 1,
      sym_identifier,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 33,
  [SMALL_STATE(4)] = 58,
  [SMALL_STATE(5)] = 91,
  [SMALL_STATE(6)] = 124,
  [SMALL_STATE(7)] = 157,
  [SMALL_STATE(8)] = 187,
  [SMALL_STATE(9)] = 217,
  [SMALL_STATE(10)] = 241,
  [SMALL_STATE(11)] = 271,
  [SMALL_STATE(12)] = 301,
  [SMALL_STATE(13)] = 323,
  [SMALL_STATE(14)] = 345,
  [SMALL_STATE(15)] = 367,
  [SMALL_STATE(16)] = 389,
  [SMALL_STATE(17)] = 411,
  [SMALL_STATE(18)] = 429,
  [SMALL_STATE(19)] = 445,
  [SMALL_STATE(20)] = 467,
  [SMALL_STATE(21)] = 489,
  [SMALL_STATE(22)] = 511,
  [SMALL_STATE(23)] = 533,
  [SMALL_STATE(24)] = 555,
  [SMALL_STATE(25)] = 577,
  [SMALL_STATE(26)] = 595,
  [SMALL_STATE(27)] = 617,
  [SMALL_STATE(28)] = 639,
  [SMALL_STATE(29)] = 656,
  [SMALL_STATE(30)] = 669,
  [SMALL_STATE(31)] = 682,
  [SMALL_STATE(32)] = 699,
  [SMALL_STATE(33)] = 714,
  [SMALL_STATE(34)] = 726,
  [SMALL_STATE(35)] = 738,
  [SMALL_STATE(36)] = 750,
  [SMALL_STATE(37)] = 762,
  [SMALL_STATE(38)] = 776,
  [SMALL_STATE(39)] = 790,
  [SMALL_STATE(40)] = 804,
  [SMALL_STATE(41)] = 814,
  [SMALL_STATE(42)] = 828,
  [SMALL_STATE(43)] = 840,
  [SMALL_STATE(44)] = 852,
  [SMALL_STATE(45)] = 866,
  [SMALL_STATE(46)] = 878,
  [SMALL_STATE(47)] = 892,
  [SMALL_STATE(48)] = 906,
  [SMALL_STATE(49)] = 920,
  [SMALL_STATE(50)] = 929,
  [SMALL_STATE(51)] = 938,
  [SMALL_STATE(52)] = 947,
  [SMALL_STATE(53)] = 954,
  [SMALL_STATE(54)] = 961,
  [SMALL_STATE(55)] = 972,
  [SMALL_STATE(56)] = 983,
  [SMALL_STATE(57)] = 991,
  [SMALL_STATE(58)] = 999,
  [SMALL_STATE(59)] = 1004,
  [SMALL_STATE(60)] = 1008,
  [SMALL_STATE(61)] = 1012,
  [SMALL_STATE(62)] = 1016,
  [SMALL_STATE(63)] = 1020,
  [SMALL_STATE(64)] = 1024,
  [SMALL_STATE(65)] = 1028,
  [SMALL_STATE(66)] = 1032,
  [SMALL_STATE(67)] = 1036,
  [SMALL_STATE(68)] = 1040,
  [SMALL_STATE(69)] = 1044,
  [SMALL_STATE(70)] = 1048,
  [SMALL_STATE(71)] = 1052,
  [SMALL_STATE(72)] = 1056,
  [SMALL_STATE(73)] = 1060,
  [SMALL_STATE(74)] = 1064,
  [SMALL_STATE(75)] = 1068,
  [SMALL_STATE(76)] = 1072,
  [SMALL_STATE(77)] = 1076,
  [SMALL_STATE(78)] = 1080,
  [SMALL_STATE(79)] = 1084,
  [SMALL_STATE(80)] = 1088,
  [SMALL_STATE(81)] = 1092,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(81),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(74),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(2),
  [19] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_call, 2, .production_id = 3),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 2, .production_id = 3),
  [23] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(81),
  [26] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2),
  [28] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2),
  [30] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(74),
  [33] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(30),
  [36] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(2),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [45] = {.entry = {.count = 1, .reusable = false}}, SHIFT(79),
  [47] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(79),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(60),
  [53] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(33),
  [56] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 2), SHIFT_REPEAT(8),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [63] = {.entry = {.count = 1, .reusable = false}}, SHIFT(67),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [69] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [71] = {.entry = {.count = 1, .reusable = false}}, SHIFT(80),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [77] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [79] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_expression, 6, .production_id = 6),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_expression, 6, .production_id = 6),
  [83] = {.entry = {.count = 1, .reusable = false}}, SHIFT(50),
  [85] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [87] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [89] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_function_call_repeat1, 1),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_call_repeat1, 1),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [95] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [101] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [105] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [109] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [111] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [113] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_def, 7, .production_id = 2),
  [115] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2),
  [117] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(59),
  [120] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(57),
  [123] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_params_def, 1),
  [129] = {.entry = {.count = 1, .reusable = false}}, SHIFT(59),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [133] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operator, 1),
  [135] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operator, 1),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [139] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(61),
  [142] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [144] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 5, .production_id = 1),
  [146] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 5, .production_id = 1),
  [148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 1),
  [150] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 1),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [166] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [182] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [186] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [198] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
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
