#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 47
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 31
#define ALIAS_COUNT 0
#define TOKEN_COUNT 19
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 13
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 8

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
  sym_identifier = 18,
  sym_source_file = 19,
  sym_fn_def = 20,
  sym_fn_params_def = 21,
  sym_fn_param_def = 22,
  sym_expression = 23,
  sym_let_expression = 24,
  sym_string_literal = 25,
  sym_infix_operator = 26,
  sym_infix_operation = 27,
  sym_type = 28,
  aux_sym_source_file_repeat1 = 29,
  aux_sym_fn_params_def_repeat1 = 30,
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
  [sym_identifier] = "identifier",
  [sym_source_file] = "source_file",
  [sym_fn_def] = "fn_def",
  [sym_fn_params_def] = "fn_params_def",
  [sym_fn_param_def] = "fn_param_def",
  [sym_expression] = "expression",
  [sym_let_expression] = "let_expression",
  [sym_string_literal] = "string_literal",
  [sym_infix_operator] = "infix_operator",
  [sym_infix_operation] = "infix_operation",
  [sym_type] = "type",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_fn_params_def_repeat1] = "fn_params_def_repeat1",
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
  [sym_identifier] = sym_identifier,
  [sym_source_file] = sym_source_file,
  [sym_fn_def] = sym_fn_def,
  [sym_fn_params_def] = sym_fn_params_def,
  [sym_fn_param_def] = sym_fn_param_def,
  [sym_expression] = sym_expression,
  [sym_let_expression] = sym_let_expression,
  [sym_string_literal] = sym_string_literal,
  [sym_infix_operator] = sym_infix_operator,
  [sym_infix_operation] = sym_infix_operation,
  [sym_type] = sym_type,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_fn_params_def_repeat1] = aux_sym_fn_params_def_repeat1,
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
};

enum {
  field_additional = 1,
  field_body = 2,
  field_expr = 3,
  field_first = 4,
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
  [field_additional] = "additional",
  [field_body] = "body",
  [field_expr] = "expr",
  [field_first] = "first",
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
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 2},
  [4] = {.index = 5, .length = 4},
  [5] = {.index = 9, .length = 1},
  [6] = {.index = 10, .length = 3},
  [7] = {.index = 13, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_first, 0},
  [1] =
    {field_additional, 1},
    {field_first, 0},
  [3] =
    {field_identifier, 1},
    {field_typ, 3},
  [5] =
    {field_body, 6},
    {field_name, 1},
    {field_params, 2},
    {field_return_type, 4},
  [9] =
    {field_value, 1},
  [10] =
    {field_left, 0},
    {field_operator, 1},
    {field_right, 2},
  [13] =
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
  [5] = 5,
  [6] = 5,
  [7] = 7,
  [8] = 3,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 17,
  [19] = 14,
  [20] = 16,
  [21] = 21,
  [22] = 22,
  [23] = 22,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 21,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 34,
  [42] = 42,
  [43] = 36,
  [44] = 44,
  [45] = 33,
  [46] = 37,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(25);
      if (lookahead == '"') ADVANCE(34);
      if (lookahead == '(') ADVANCE(30);
      if (lookahead == ')') ADVANCE(31);
      if (lookahead == '+') ADVANCE(37);
      if (lookahead == '-') ADVANCE(38);
      if (lookahead == ':') ADVANCE(28);
      if (lookahead == '=') ADVANCE(29);
      if (lookahead == 'B') ADVANCE(15);
      if (lookahead == 'C') ADVANCE(8);
      if (lookahead == 'F') ADVANCE(11);
      if (lookahead == 'I') ADVANCE(14);
      if (lookahead == 'S') ADVANCE(20);
      if (lookahead == 'i') ADVANCE(12);
      if (lookahead == 'l') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(33);
      if (lookahead == '+') ADVANCE(37);
      if (lookahead == '-') ADVANCE(38);
      if (lookahead == 'i') ADVANCE(12);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(34);
      if (lookahead == '(') ADVANCE(3);
      if (lookahead == 'l') ADVANCE(45);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    case 3:
      if (lookahead == ')') ADVANCE(39);
      END_STATE();
    case 4:
      if (lookahead == 'a') ADVANCE(19);
      END_STATE();
    case 5:
      if (lookahead == 'a') ADVANCE(23);
      END_STATE();
    case 6:
      if (lookahead == 'e') ADVANCE(22);
      END_STATE();
    case 7:
      if (lookahead == 'g') ADVANCE(43);
      END_STATE();
    case 8:
      if (lookahead == 'h') ADVANCE(4);
      END_STATE();
    case 9:
      if (lookahead == 'i') ADVANCE(13);
      END_STATE();
    case 10:
      if (lookahead == 'l') ADVANCE(41);
      END_STATE();
    case 11:
      if (lookahead == 'l') ADVANCE(17);
      END_STATE();
    case 12:
      if (lookahead == 'n') ADVANCE(32);
      END_STATE();
    case 13:
      if (lookahead == 'n') ADVANCE(7);
      END_STATE();
    case 14:
      if (lookahead == 'n') ADVANCE(21);
      END_STATE();
    case 15:
      if (lookahead == 'o') ADVANCE(16);
      END_STATE();
    case 16:
      if (lookahead == 'o') ADVANCE(10);
      END_STATE();
    case 17:
      if (lookahead == 'o') ADVANCE(5);
      END_STATE();
    case 18:
      if (lookahead == 'r') ADVANCE(9);
      END_STATE();
    case 19:
      if (lookahead == 'r') ADVANCE(44);
      END_STATE();
    case 20:
      if (lookahead == 't') ADVANCE(18);
      END_STATE();
    case 21:
      if (lookahead == 't') ADVANCE(40);
      END_STATE();
    case 22:
      if (lookahead == 't') ADVANCE(26);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(42);
      END_STATE();
    case 24:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(24)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')') ADVANCE(39);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_in);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(35);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(36);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_string_literal_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_unit);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(aux_sym_type_token1);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(aux_sym_type_token2);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(aux_sym_type_token3);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(aux_sym_type_token4);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(aux_sym_type_token5);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(46);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(27);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(47);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 2},
  [3] = {.lex_state = 2},
  [4] = {.lex_state = 2},
  [5] = {.lex_state = 2},
  [6] = {.lex_state = 2},
  [7] = {.lex_state = 2},
  [8] = {.lex_state = 2},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 1},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 2},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 24},
  [36] = {.lex_state = 35},
  [37] = {.lex_state = 24},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 24},
  [43] = {.lex_state = 35},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 24},
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
  },
  [1] = {
    [sym_source_file] = STATE(44),
    [sym_fn_def] = STATE(28),
    [aux_sym_source_file_repeat1] = STATE(28),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(9), 1,
      anon_sym_DQUOTE,
    ACTIONS(11), 1,
      sym_unit,
    ACTIONS(13), 1,
      sym_identifier,
    STATE(16), 1,
      sym_expression,
    STATE(22), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [21] = 6,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(21), 1,
      sym_identifier,
    STATE(18), 1,
      sym_expression,
    STATE(23), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [42] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(9), 1,
      anon_sym_DQUOTE,
    ACTIONS(11), 1,
      sym_unit,
    ACTIONS(13), 1,
      sym_identifier,
    STATE(20), 1,
      sym_expression,
    STATE(22), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [63] = 6,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(21), 1,
      sym_identifier,
    STATE(19), 1,
      sym_expression,
    STATE(23), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [84] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(9), 1,
      anon_sym_DQUOTE,
    ACTIONS(11), 1,
      sym_unit,
    ACTIONS(13), 1,
      sym_identifier,
    STATE(14), 1,
      sym_expression,
    STATE(22), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [105] = 6,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      sym_unit,
    ACTIONS(21), 1,
      sym_identifier,
    STATE(11), 1,
      sym_expression,
    STATE(23), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [126] = 6,
    ACTIONS(7), 1,
      anon_sym_let,
    ACTIONS(9), 1,
      anon_sym_DQUOTE,
    ACTIONS(11), 1,
      sym_unit,
    ACTIONS(13), 1,
      sym_identifier,
    STATE(17), 1,
      sym_expression,
    STATE(22), 3,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
  [147] = 2,
    STATE(39), 1,
      sym_type,
    ACTIONS(23), 5,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
  [158] = 2,
    STATE(38), 1,
      sym_type,
    ACTIONS(23), 5,
      aux_sym_type_token1,
      aux_sym_type_token2,
      aux_sym_type_token3,
      aux_sym_type_token4,
      aux_sym_type_token5,
  [169] = 3,
    STATE(5), 1,
      sym_infix_operator,
    ACTIONS(25), 2,
      ts_builtin_sym_end,
      anon_sym_let,
    ACTIONS(27), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [181] = 4,
    ACTIONS(29), 1,
      anon_sym_COLON,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(33), 1,
      sym_unit,
    STATE(13), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [195] = 4,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(33), 1,
      sym_unit,
    ACTIONS(35), 1,
      anon_sym_COLON,
    STATE(15), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [209] = 3,
    ACTIONS(39), 1,
      anon_sym_LF,
    STATE(6), 1,
      sym_infix_operator,
    ACTIONS(37), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [221] = 4,
    ACTIONS(41), 1,
      anon_sym_COLON,
    ACTIONS(43), 1,
      anon_sym_LPAREN,
    ACTIONS(46), 1,
      sym_unit,
    STATE(15), 2,
      sym_fn_param_def,
      aux_sym_fn_params_def_repeat1,
  [235] = 4,
    ACTIONS(49), 1,
      anon_sym_in,
    ACTIONS(51), 1,
      anon_sym_LF,
    STATE(6), 1,
      sym_infix_operator,
    ACTIONS(53), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [249] = 4,
    ACTIONS(55), 1,
      anon_sym_in,
    ACTIONS(57), 1,
      anon_sym_LF,
    STATE(6), 1,
      sym_infix_operator,
    ACTIONS(53), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [263] = 3,
    STATE(5), 1,
      sym_infix_operator,
    ACTIONS(27), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(57), 2,
      ts_builtin_sym_end,
      anon_sym_let,
  [275] = 2,
    STATE(5), 1,
      sym_infix_operator,
    ACTIONS(39), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [285] = 4,
    ACTIONS(59), 1,
      anon_sym_in,
    ACTIONS(61), 1,
      anon_sym_LF,
    STATE(6), 1,
      sym_infix_operator,
    ACTIONS(53), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [299] = 2,
    ACTIONS(65), 1,
      anon_sym_LF,
    ACTIONS(63), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [308] = 2,
    ACTIONS(69), 1,
      anon_sym_LF,
    ACTIONS(67), 3,
      anon_sym_in,
      anon_sym_PLUS,
      anon_sym_DASH,
  [317] = 1,
    ACTIONS(69), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [324] = 2,
    ACTIONS(71), 2,
      anon_sym_let,
      sym_identifier,
    ACTIONS(73), 2,
      anon_sym_DQUOTE,
      sym_unit,
  [333] = 3,
    ACTIONS(75), 1,
      ts_builtin_sym_end,
    ACTIONS(77), 1,
      anon_sym_let,
    STATE(25), 2,
      sym_fn_def,
      aux_sym_source_file_repeat1,
  [344] = 4,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(33), 1,
      sym_unit,
    STATE(12), 1,
      sym_fn_param_def,
    STATE(40), 1,
      sym_fn_params_def,
  [357] = 1,
    ACTIONS(65), 4,
      ts_builtin_sym_end,
      anon_sym_let,
      anon_sym_PLUS,
      anon_sym_DASH,
  [364] = 3,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(80), 1,
      ts_builtin_sym_end,
    STATE(25), 2,
      sym_fn_def,
      aux_sym_source_file_repeat1,
  [375] = 2,
    ACTIONS(84), 1,
      anon_sym_LPAREN,
    ACTIONS(82), 2,
      anon_sym_COLON,
      sym_unit,
  [383] = 2,
    ACTIONS(88), 1,
      anon_sym_LPAREN,
    ACTIONS(86), 2,
      anon_sym_COLON,
      sym_unit,
  [391] = 1,
    ACTIONS(90), 2,
      anon_sym_EQ,
      anon_sym_RPAREN,
  [396] = 1,
    ACTIONS(92), 1,
      anon_sym_COLON,
  [400] = 1,
    ACTIONS(94), 1,
      anon_sym_EQ,
  [404] = 1,
    ACTIONS(96), 1,
      anon_sym_DQUOTE,
  [408] = 1,
    ACTIONS(98), 1,
      sym_identifier,
  [412] = 1,
    ACTIONS(100), 1,
      aux_sym_string_literal_token1,
  [416] = 1,
    ACTIONS(102), 1,
      sym_identifier,
  [420] = 1,
    ACTIONS(104), 1,
      anon_sym_RPAREN,
  [424] = 1,
    ACTIONS(106), 1,
      anon_sym_EQ,
  [428] = 1,
    ACTIONS(108), 1,
      anon_sym_COLON,
  [432] = 1,
    ACTIONS(110), 1,
      anon_sym_DQUOTE,
  [436] = 1,
    ACTIONS(112), 1,
      sym_identifier,
  [440] = 1,
    ACTIONS(114), 1,
      aux_sym_string_literal_token1,
  [444] = 1,
    ACTIONS(116), 1,
      ts_builtin_sym_end,
  [448] = 1,
    ACTIONS(118), 1,
      anon_sym_EQ,
  [452] = 1,
    ACTIONS(120), 1,
      sym_identifier,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 21,
  [SMALL_STATE(4)] = 42,
  [SMALL_STATE(5)] = 63,
  [SMALL_STATE(6)] = 84,
  [SMALL_STATE(7)] = 105,
  [SMALL_STATE(8)] = 126,
  [SMALL_STATE(9)] = 147,
  [SMALL_STATE(10)] = 158,
  [SMALL_STATE(11)] = 169,
  [SMALL_STATE(12)] = 181,
  [SMALL_STATE(13)] = 195,
  [SMALL_STATE(14)] = 209,
  [SMALL_STATE(15)] = 221,
  [SMALL_STATE(16)] = 235,
  [SMALL_STATE(17)] = 249,
  [SMALL_STATE(18)] = 263,
  [SMALL_STATE(19)] = 275,
  [SMALL_STATE(20)] = 285,
  [SMALL_STATE(21)] = 299,
  [SMALL_STATE(22)] = 308,
  [SMALL_STATE(23)] = 317,
  [SMALL_STATE(24)] = 324,
  [SMALL_STATE(25)] = 333,
  [SMALL_STATE(26)] = 344,
  [SMALL_STATE(27)] = 357,
  [SMALL_STATE(28)] = 364,
  [SMALL_STATE(29)] = 375,
  [SMALL_STATE(30)] = 383,
  [SMALL_STATE(31)] = 391,
  [SMALL_STATE(32)] = 396,
  [SMALL_STATE(33)] = 400,
  [SMALL_STATE(34)] = 404,
  [SMALL_STATE(35)] = 408,
  [SMALL_STATE(36)] = 412,
  [SMALL_STATE(37)] = 416,
  [SMALL_STATE(38)] = 420,
  [SMALL_STATE(39)] = 424,
  [SMALL_STATE(40)] = 428,
  [SMALL_STATE(41)] = 432,
  [SMALL_STATE(42)] = 436,
  [SMALL_STATE(43)] = 440,
  [SMALL_STATE(44)] = 444,
  [SMALL_STATE(45)] = 448,
  [SMALL_STATE(46)] = 452,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(46),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_def, 7, .production_id = 4),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_params_def, 1, .production_id = 1),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_params_def, 2, .production_id = 2),
  [37] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operation, 3, .production_id = 6),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operation, 3, .production_id = 6),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2),
  [43] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(42),
  [46] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fn_params_def_repeat1, 2), SHIFT_REPEAT(29),
  [49] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [53] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [55] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_expression, 6, .production_id = 7),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_expression, 6, .production_id = 7),
  [59] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [63] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 3, .production_id = 5),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 3, .production_id = 5),
  [67] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [71] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operator, 1),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operator, 1),
  [75] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [77] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(35),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 1),
  [84] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 1),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_param_def, 5, .production_id = 3),
  [88] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_param_def, 5, .production_id = 3),
  [90] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [116] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
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
