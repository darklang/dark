#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 214
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 86
#define ALIAS_COUNT 0
#define TOKEN_COUNT 49
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
  anon_sym_y = 15,
  anon_sym_uy = 16,
  anon_sym_s = 17,
  anon_sym_us = 18,
  anon_sym_l = 19,
  anon_sym_ul = 20,
  anon_sym_L = 21,
  anon_sym_UL = 22,
  anon_sym_Q = 23,
  anon_sym_Z = 24,
  sym_float_literal = 25,
  sym_negative_digits = 26,
  sym_positive_digits = 27,
  aux_sym_builtin_type_token1 = 28,
  aux_sym_builtin_type_token2 = 29,
  aux_sym_builtin_type_token3 = 30,
  aux_sym_builtin_type_token4 = 31,
  aux_sym_builtin_type_token5 = 32,
  aux_sym_builtin_type_token6 = 33,
  aux_sym_builtin_type_token7 = 34,
  aux_sym_builtin_type_token8 = 35,
  aux_sym_builtin_type_token9 = 36,
  aux_sym_builtin_type_token10 = 37,
  aux_sym_builtin_type_token11 = 38,
  aux_sym_builtin_type_token12 = 39,
  aux_sym_builtin_type_token13 = 40,
  aux_sym_builtin_type_token14 = 41,
  aux_sym_builtin_type_token15 = 42,
  aux_sym_builtin_type_token16 = 43,
  aux_sym_builtin_type_token17 = 44,
  anon_sym_DOT = 45,
  aux_sym_variable_identifier_token1 = 46,
  aux_sym_type_identifier_token1 = 47,
  sym_unit = 48,
  sym_source_file = 49,
  sym_fn_decl = 50,
  sym_fn_decl_params = 51,
  sym_fn_decl_param = 52,
  sym_type_decl = 53,
  sym_expression = 54,
  sym_paren_expression = 55,
  sym_bool_literal = 56,
  sym_function_call = 57,
  sym_let_expression = 58,
  sym_string_literal = 59,
  sym_string_content = 60,
  sym_infix_operation = 61,
  sym_int8_literal = 62,
  sym_uint8_literal = 63,
  sym_int16_literal = 64,
  sym_uint16_literal = 65,
  sym_int32_literal = 66,
  sym_uint32_literal = 67,
  sym_int64_literal = 68,
  sym_uint64_literal = 69,
  sym_int128_literal = 70,
  sym_uint128_literal = 71,
  sym_digits = 72,
  sym_type_reference = 73,
  sym_builtin_type = 74,
  sym_qualified_fn_name = 75,
  sym_qualified_type_name = 76,
  sym_variable_identifier = 77,
  sym_fn_identifier = 78,
  sym_type_identifier = 79,
  sym_module_identifier = 80,
  aux_sym_source_file_repeat1 = 81,
  aux_sym_source_file_repeat2 = 82,
  aux_sym_fn_decl_params_repeat1 = 83,
  aux_sym_string_content_repeat1 = 84,
  aux_sym_qualified_fn_name_repeat1 = 85,
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
  [anon_sym_y] = "symbol",
  [anon_sym_uy] = "symbol",
  [anon_sym_s] = "symbol",
  [anon_sym_us] = "symbol",
  [anon_sym_l] = "symbol",
  [anon_sym_ul] = "symbol",
  [anon_sym_L] = "symbol",
  [anon_sym_UL] = "symbol",
  [anon_sym_Q] = "symbol",
  [anon_sym_Z] = "symbol",
  [sym_float_literal] = "float_literal",
  [sym_negative_digits] = "negative_digits",
  [sym_positive_digits] = "positive_digits",
  [aux_sym_builtin_type_token1] = "builtin_type_token1",
  [aux_sym_builtin_type_token2] = "builtin_type_token2",
  [aux_sym_builtin_type_token3] = "builtin_type_token3",
  [aux_sym_builtin_type_token4] = "builtin_type_token4",
  [aux_sym_builtin_type_token5] = "builtin_type_token5",
  [aux_sym_builtin_type_token6] = "builtin_type_token6",
  [aux_sym_builtin_type_token7] = "builtin_type_token7",
  [aux_sym_builtin_type_token8] = "builtin_type_token8",
  [aux_sym_builtin_type_token9] = "builtin_type_token9",
  [aux_sym_builtin_type_token10] = "builtin_type_token10",
  [aux_sym_builtin_type_token11] = "builtin_type_token11",
  [aux_sym_builtin_type_token12] = "builtin_type_token12",
  [aux_sym_builtin_type_token13] = "builtin_type_token13",
  [aux_sym_builtin_type_token14] = "builtin_type_token14",
  [aux_sym_builtin_type_token15] = "builtin_type_token15",
  [aux_sym_builtin_type_token16] = "builtin_type_token16",
  [aux_sym_builtin_type_token17] = "builtin_type_token17",
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
  [sym_int8_literal] = "int8_literal",
  [sym_uint8_literal] = "uint8_literal",
  [sym_int16_literal] = "int16_literal",
  [sym_uint16_literal] = "uint16_literal",
  [sym_int32_literal] = "int32_literal",
  [sym_uint32_literal] = "uint32_literal",
  [sym_int64_literal] = "int64_literal",
  [sym_uint64_literal] = "uint64_literal",
  [sym_int128_literal] = "int128_literal",
  [sym_uint128_literal] = "uint128_literal",
  [sym_digits] = "digits",
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
  [anon_sym_y] = anon_sym_COLON,
  [anon_sym_uy] = anon_sym_COLON,
  [anon_sym_s] = anon_sym_COLON,
  [anon_sym_us] = anon_sym_COLON,
  [anon_sym_l] = anon_sym_COLON,
  [anon_sym_ul] = anon_sym_COLON,
  [anon_sym_L] = anon_sym_COLON,
  [anon_sym_UL] = anon_sym_COLON,
  [anon_sym_Q] = anon_sym_COLON,
  [anon_sym_Z] = anon_sym_COLON,
  [sym_float_literal] = sym_float_literal,
  [sym_negative_digits] = sym_negative_digits,
  [sym_positive_digits] = sym_positive_digits,
  [aux_sym_builtin_type_token1] = aux_sym_builtin_type_token1,
  [aux_sym_builtin_type_token2] = aux_sym_builtin_type_token2,
  [aux_sym_builtin_type_token3] = aux_sym_builtin_type_token3,
  [aux_sym_builtin_type_token4] = aux_sym_builtin_type_token4,
  [aux_sym_builtin_type_token5] = aux_sym_builtin_type_token5,
  [aux_sym_builtin_type_token6] = aux_sym_builtin_type_token6,
  [aux_sym_builtin_type_token7] = aux_sym_builtin_type_token7,
  [aux_sym_builtin_type_token8] = aux_sym_builtin_type_token8,
  [aux_sym_builtin_type_token9] = aux_sym_builtin_type_token9,
  [aux_sym_builtin_type_token10] = aux_sym_builtin_type_token10,
  [aux_sym_builtin_type_token11] = aux_sym_builtin_type_token11,
  [aux_sym_builtin_type_token12] = aux_sym_builtin_type_token12,
  [aux_sym_builtin_type_token13] = aux_sym_builtin_type_token13,
  [aux_sym_builtin_type_token14] = aux_sym_builtin_type_token14,
  [aux_sym_builtin_type_token15] = aux_sym_builtin_type_token15,
  [aux_sym_builtin_type_token16] = aux_sym_builtin_type_token16,
  [aux_sym_builtin_type_token17] = aux_sym_builtin_type_token17,
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
  [sym_int8_literal] = sym_int8_literal,
  [sym_uint8_literal] = sym_uint8_literal,
  [sym_int16_literal] = sym_int16_literal,
  [sym_uint16_literal] = sym_uint16_literal,
  [sym_int32_literal] = sym_int32_literal,
  [sym_uint32_literal] = sym_uint32_literal,
  [sym_int64_literal] = sym_int64_literal,
  [sym_uint64_literal] = sym_uint64_literal,
  [sym_int128_literal] = sym_int128_literal,
  [sym_uint128_literal] = sym_uint128_literal,
  [sym_digits] = sym_digits,
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
  [anon_sym_y] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_uy] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_s] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_us] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_l] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_ul] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_L] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_UL] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_Q] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_Z] = {
    .visible = true,
    .named = true,
  },
  [sym_float_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_negative_digits] = {
    .visible = true,
    .named = true,
  },
  [sym_positive_digits] = {
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
  [aux_sym_builtin_type_token7] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token8] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token9] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token10] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token11] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token12] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token13] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token14] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token15] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token16] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_builtin_type_token17] = {
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
  [sym_int8_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint8_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_int16_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint16_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_int32_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint32_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_int64_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint64_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_int128_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint128_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_digits] = {
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
  [7] = 3,
  [8] = 8,
  [9] = 9,
  [10] = 8,
  [11] = 11,
  [12] = 12,
  [13] = 9,
  [14] = 9,
  [15] = 9,
  [16] = 9,
  [17] = 17,
  [18] = 17,
  [19] = 17,
  [20] = 17,
  [21] = 17,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 22,
  [26] = 26,
  [27] = 23,
  [28] = 22,
  [29] = 22,
  [30] = 26,
  [31] = 22,
  [32] = 26,
  [33] = 23,
  [34] = 23,
  [35] = 26,
  [36] = 26,
  [37] = 23,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 41,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 62,
  [70] = 50,
  [71] = 63,
  [72] = 50,
  [73] = 45,
  [74] = 49,
  [75] = 54,
  [76] = 64,
  [77] = 53,
  [78] = 52,
  [79] = 44,
  [80] = 67,
  [81] = 57,
  [82] = 82,
  [83] = 59,
  [84] = 46,
  [85] = 41,
  [86] = 82,
  [87] = 55,
  [88] = 67,
  [89] = 64,
  [90] = 63,
  [91] = 58,
  [92] = 45,
  [93] = 54,
  [94] = 53,
  [95] = 52,
  [96] = 44,
  [97] = 57,
  [98] = 58,
  [99] = 59,
  [100] = 46,
  [101] = 47,
  [102] = 55,
  [103] = 66,
  [104] = 49,
  [105] = 62,
  [106] = 47,
  [107] = 66,
  [108] = 108,
  [109] = 109,
  [110] = 110,
  [111] = 111,
  [112] = 112,
  [113] = 112,
  [114] = 112,
  [115] = 112,
  [116] = 112,
  [117] = 117,
  [118] = 118,
  [119] = 117,
  [120] = 118,
  [121] = 117,
  [122] = 122,
  [123] = 118,
  [124] = 124,
  [125] = 118,
  [126] = 117,
  [127] = 127,
  [128] = 118,
  [129] = 129,
  [130] = 130,
  [131] = 117,
  [132] = 132,
  [133] = 133,
  [134] = 134,
  [135] = 135,
  [136] = 63,
  [137] = 52,
  [138] = 54,
  [139] = 45,
  [140] = 58,
  [141] = 141,
  [142] = 64,
  [143] = 52,
  [144] = 67,
  [145] = 145,
  [146] = 44,
  [147] = 57,
  [148] = 50,
  [149] = 49,
  [150] = 59,
  [151] = 46,
  [152] = 55,
  [153] = 66,
  [154] = 62,
  [155] = 47,
  [156] = 41,
  [157] = 49,
  [158] = 50,
  [159] = 159,
  [160] = 41,
  [161] = 47,
  [162] = 67,
  [163] = 141,
  [164] = 164,
  [165] = 64,
  [166] = 63,
  [167] = 58,
  [168] = 45,
  [169] = 164,
  [170] = 62,
  [171] = 66,
  [172] = 141,
  [173] = 54,
  [174] = 53,
  [175] = 53,
  [176] = 164,
  [177] = 55,
  [178] = 44,
  [179] = 141,
  [180] = 57,
  [181] = 59,
  [182] = 46,
  [183] = 164,
  [184] = 141,
  [185] = 164,
  [186] = 186,
  [187] = 187,
  [188] = 186,
  [189] = 186,
  [190] = 190,
  [191] = 191,
  [192] = 186,
  [193] = 193,
  [194] = 186,
  [195] = 195,
  [196] = 196,
  [197] = 197,
  [198] = 198,
  [199] = 199,
  [200] = 197,
  [201] = 201,
  [202] = 202,
  [203] = 203,
  [204] = 204,
  [205] = 197,
  [206] = 199,
  [207] = 197,
  [208] = 199,
  [209] = 199,
  [210] = 199,
  [211] = 211,
  [212] = 212,
  [213] = 197,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(62);
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '(') ADVANCE(66);
      if (lookahead == ')') ADVANCE(67);
      if (lookahead == '+') ADVANCE(80);
      if (lookahead == '-') ADVANCE(82);
      if (lookahead == '.') ADVANCE(130);
      if (lookahead == ':') ADVANCE(64);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == 'B') ADVANCE(42);
      if (lookahead == 'C') ADVANCE(29);
      if (lookahead == 'D') ADVANCE(19);
      if (lookahead == 'F') ADVANCE(37);
      if (lookahead == 'I') ADVANCE(40);
      if (lookahead == 'L') ADVANCE(89);
      if (lookahead == 'Q') ADVANCE(91);
      if (lookahead == 'S') ADVANCE(50);
      if (lookahead == 'U') ADVANCE(16);
      if (lookahead == 'Z') ADVANCE(92);
      if (lookahead == '\\') ADVANCE(57);
      if (lookahead == 'f') ADVANCE(18);
      if (lookahead == 'l') ADVANCE(87);
      if (lookahead == 's') ADVANCE(85);
      if (lookahead == 't') ADVANCE(46);
      if (lookahead == 'u') ADVANCE(34);
      if (lookahead == 'y') ADVANCE(83);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(59)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 1:
      if (lookahead == '\n') SKIP(3)
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '\\') ADVANCE(57);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(76);
      if (lookahead != 0) ADVANCE(77);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(74);
      if (lookahead == '+') ADVANCE(79);
      if (lookahead == '-') ADVANCE(81);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      END_STATE();
    case 4:
      if (lookahead == ')') ADVANCE(67);
      if (lookahead == '+') ADVANCE(79);
      if (lookahead == '-') ADVANCE(81);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(185);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 5:
      if (lookahead == '.') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(5);
      END_STATE();
    case 6:
      if (lookahead == '1') ADVANCE(8);
      if (lookahead == '3') ADVANCE(9);
      if (lookahead == '6') ADVANCE(12);
      if (lookahead == '8') ADVANCE(100);
      END_STATE();
    case 7:
      if (lookahead == '1') ADVANCE(11);
      if (lookahead == '3') ADVANCE(10);
      if (lookahead == '6') ADVANCE(13);
      if (lookahead == '8') ADVANCE(102);
      END_STATE();
    case 8:
      if (lookahead == '2') ADVANCE(14);
      if (lookahead == '6') ADVANCE(104);
      END_STATE();
    case 9:
      if (lookahead == '2') ADVANCE(108);
      END_STATE();
    case 10:
      if (lookahead == '2') ADVANCE(110);
      END_STATE();
    case 11:
      if (lookahead == '2') ADVANCE(15);
      if (lookahead == '6') ADVANCE(106);
      END_STATE();
    case 12:
      if (lookahead == '4') ADVANCE(112);
      END_STATE();
    case 13:
      if (lookahead == '4') ADVANCE(114);
      END_STATE();
    case 14:
      if (lookahead == '8') ADVANCE(116);
      END_STATE();
    case 15:
      if (lookahead == '8') ADVANCE(118);
      END_STATE();
    case 16:
      if (lookahead == 'I') ADVANCE(41);
      if (lookahead == 'L') ADVANCE(90);
      if (lookahead == 'n') ADVANCE(33);
      if (lookahead == 'u') ADVANCE(30);
      END_STATE();
    case 17:
      if (lookahead == 'T') ADVANCE(31);
      END_STATE();
    case 18:
      if (lookahead == 'a') ADVANCE(35);
      END_STATE();
    case 19:
      if (lookahead == 'a') ADVANCE(51);
      END_STATE();
    case 20:
      if (lookahead == 'a') ADVANCE(47);
      END_STATE();
    case 21:
      if (lookahead == 'a') ADVANCE(54);
      END_STATE();
    case 22:
      if (lookahead == 'd') ADVANCE(128);
      END_STATE();
    case 23:
      if (lookahead == 'e') ADVANCE(17);
      END_STATE();
    case 24:
      if (lookahead == 'e') ADVANCE(70);
      END_STATE();
    case 25:
      if (lookahead == 'e') ADVANCE(68);
      END_STATE();
    case 26:
      if (lookahead == 'e') ADVANCE(72);
      END_STATE();
    case 27:
      if (lookahead == 'e') ADVANCE(126);
      END_STATE();
    case 28:
      if (lookahead == 'g') ADVANCE(124);
      END_STATE();
    case 29:
      if (lookahead == 'h') ADVANCE(20);
      END_STATE();
    case 30:
      if (lookahead == 'i') ADVANCE(22);
      END_STATE();
    case 31:
      if (lookahead == 'i') ADVANCE(38);
      END_STATE();
    case 32:
      if (lookahead == 'i') ADVANCE(39);
      END_STATE();
    case 33:
      if (lookahead == 'i') ADVANCE(53);
      END_STATE();
    case 34:
      if (lookahead == 'l') ADVANCE(88);
      if (lookahead == 's') ADVANCE(86);
      if (lookahead == 'y') ADVANCE(84);
      END_STATE();
    case 35:
      if (lookahead == 'l') ADVANCE(49);
      END_STATE();
    case 36:
      if (lookahead == 'l') ADVANCE(98);
      END_STATE();
    case 37:
      if (lookahead == 'l') ADVANCE(44);
      END_STATE();
    case 38:
      if (lookahead == 'm') ADVANCE(27);
      END_STATE();
    case 39:
      if (lookahead == 'n') ADVANCE(28);
      END_STATE();
    case 40:
      if (lookahead == 'n') ADVANCE(52);
      END_STATE();
    case 41:
      if (lookahead == 'n') ADVANCE(55);
      END_STATE();
    case 42:
      if (lookahead == 'o') ADVANCE(43);
      END_STATE();
    case 43:
      if (lookahead == 'o') ADVANCE(36);
      END_STATE();
    case 44:
      if (lookahead == 'o') ADVANCE(21);
      END_STATE();
    case 45:
      if (lookahead == 'p') ADVANCE(25);
      END_STATE();
    case 46:
      if (lookahead == 'r') ADVANCE(56);
      if (lookahead == 'y') ADVANCE(45);
      END_STATE();
    case 47:
      if (lookahead == 'r') ADVANCE(122);
      END_STATE();
    case 48:
      if (lookahead == 'r') ADVANCE(32);
      END_STATE();
    case 49:
      if (lookahead == 's') ADVANCE(26);
      END_STATE();
    case 50:
      if (lookahead == 't') ADVANCE(48);
      END_STATE();
    case 51:
      if (lookahead == 't') ADVANCE(23);
      END_STATE();
    case 52:
      if (lookahead == 't') ADVANCE(6);
      END_STATE();
    case 53:
      if (lookahead == 't') ADVANCE(96);
      END_STATE();
    case 54:
      if (lookahead == 't') ADVANCE(120);
      END_STATE();
    case 55:
      if (lookahead == 't') ADVANCE(7);
      END_STATE();
    case 56:
      if (lookahead == 'u') ADVANCE(24);
      END_STATE();
    case 57:
      if (lookahead == '"' ||
          lookahead == '/' ||
          lookahead == '\\' ||
          lookahead == 'b' ||
          lookahead == 'f' ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't' ||
          lookahead == 'u') ADVANCE(78);
      END_STATE();
    case 58:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(93);
      END_STATE();
    case 59:
      if (eof) ADVANCE(62);
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '(') ADVANCE(66);
      if (lookahead == ')') ADVANCE(67);
      if (lookahead == '+') ADVANCE(80);
      if (lookahead == '-') ADVANCE(82);
      if (lookahead == '.') ADVANCE(130);
      if (lookahead == ':') ADVANCE(64);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == 'B') ADVANCE(42);
      if (lookahead == 'C') ADVANCE(29);
      if (lookahead == 'D') ADVANCE(19);
      if (lookahead == 'F') ADVANCE(37);
      if (lookahead == 'I') ADVANCE(40);
      if (lookahead == 'L') ADVANCE(89);
      if (lookahead == 'Q') ADVANCE(91);
      if (lookahead == 'S') ADVANCE(50);
      if (lookahead == 'U') ADVANCE(16);
      if (lookahead == 'Z') ADVANCE(92);
      if (lookahead == 'f') ADVANCE(18);
      if (lookahead == 'l') ADVANCE(87);
      if (lookahead == 's') ADVANCE(85);
      if (lookahead == 't') ADVANCE(46);
      if (lookahead == 'u') ADVANCE(34);
      if (lookahead == 'y') ADVANCE(83);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(59)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 60:
      if (eof) ADVANCE(62);
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '(') ADVANCE(66);
      if (lookahead == ')') ADVANCE(67);
      if (lookahead == '+') ADVANCE(80);
      if (lookahead == '-') ADVANCE(82);
      if (lookahead == '.') ADVANCE(130);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == 'B') ADVANCE(174);
      if (lookahead == 'C') ADVANCE(163);
      if (lookahead == 'D') ADVANCE(156);
      if (lookahead == 'F') ADVANCE(169);
      if (lookahead == 'I') ADVANCE(172);
      if (lookahead == 'S') ADVANCE(179);
      if (lookahead == 'U') ADVANCE(154);
      if (lookahead == 'f') ADVANCE(131);
      if (lookahead == 'l') ADVANCE(132);
      if (lookahead == 't') ADVANCE(138);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(60)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(185);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 61:
      if (eof) ADVANCE(62);
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '(') ADVANCE(66);
      if (lookahead == ')') ADVANCE(67);
      if (lookahead == '+') ADVANCE(80);
      if (lookahead == '-') ADVANCE(82);
      if (lookahead == ':') ADVANCE(64);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == 'f') ADVANCE(131);
      if (lookahead == 'l') ADVANCE(132);
      if (lookahead == 't') ADVANCE(139);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(61)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(185);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')') ADVANCE(186);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_type);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(aux_sym_bool_literal_token1);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(aux_sym_bool_literal_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(aux_sym_bool_literal_token2);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(aux_sym_bool_literal_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(74);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(aux_sym_string_content_token1);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(76);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(77);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(aux_sym_string_content_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(77);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_string_escape_sequence);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_PLUS);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(5);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(94);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_y);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_uy);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_s);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_us);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_l);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_ul);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_L);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_UL);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_Q);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_Z);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_float_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(93);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym_negative_digits);
      if (lookahead == '.') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(94);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_positive_digits);
      if (lookahead == '.') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(aux_sym_builtin_type_token1);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(aux_sym_builtin_type_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(aux_sym_builtin_type_token2);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(aux_sym_builtin_type_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(aux_sym_builtin_type_token3);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(aux_sym_builtin_type_token3);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(aux_sym_builtin_type_token4);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(aux_sym_builtin_type_token4);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(aux_sym_builtin_type_token5);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(aux_sym_builtin_type_token5);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(aux_sym_builtin_type_token6);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(aux_sym_builtin_type_token6);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(aux_sym_builtin_type_token7);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(aux_sym_builtin_type_token7);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(aux_sym_builtin_type_token8);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(aux_sym_builtin_type_token8);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(aux_sym_builtin_type_token9);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(aux_sym_builtin_type_token9);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(aux_sym_builtin_type_token10);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(aux_sym_builtin_type_token10);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(aux_sym_builtin_type_token11);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(aux_sym_builtin_type_token11);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(aux_sym_builtin_type_token12);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(aux_sym_builtin_type_token12);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(aux_sym_builtin_type_token13);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(aux_sym_builtin_type_token13);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(aux_sym_builtin_type_token14);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(aux_sym_builtin_type_token14);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(aux_sym_builtin_type_token15);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(aux_sym_builtin_type_token15);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 126:
      ACCEPT_TOKEN(aux_sym_builtin_type_token16);
      END_STATE();
    case 127:
      ACCEPT_TOKEN(aux_sym_builtin_type_token16);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 128:
      ACCEPT_TOKEN(aux_sym_builtin_type_token17);
      END_STATE();
    case 129:
      ACCEPT_TOKEN(aux_sym_builtin_type_token17);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 130:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 131:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'a') ADVANCE(136);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 132:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(141);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 133:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(71);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 134:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(69);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 135:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'e') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 136:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'l') ADVANCE(140);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 137:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'p') ADVANCE(134);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 138:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'r') ADVANCE(142);
      if (lookahead == 'y') ADVANCE(137);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 139:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'r') ADVANCE(142);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 140:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 's') ADVANCE(135);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 141:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 't') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 142:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (lookahead == 'u') ADVANCE(133);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 143:
      ACCEPT_TOKEN(aux_sym_variable_identifier_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(143);
      END_STATE();
    case 144:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '1') ADVANCE(146);
      if (lookahead == '3') ADVANCE(147);
      if (lookahead == '6') ADVANCE(150);
      if (lookahead == '8') ADVANCE(101);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 145:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '1') ADVANCE(149);
      if (lookahead == '3') ADVANCE(148);
      if (lookahead == '6') ADVANCE(151);
      if (lookahead == '8') ADVANCE(103);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 146:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '2') ADVANCE(152);
      if (lookahead == '6') ADVANCE(105);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 147:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '2') ADVANCE(109);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 148:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '2') ADVANCE(111);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 149:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '2') ADVANCE(153);
      if (lookahead == '6') ADVANCE(107);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 150:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '4') ADVANCE(113);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 151:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '4') ADVANCE(115);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 152:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '8') ADVANCE(117);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 153:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == '8') ADVANCE(119);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 154:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'I') ADVANCE(173);
      if (lookahead == 'n') ADVANCE(167);
      if (lookahead == 'u') ADVANCE(164);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 155:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'T') ADVANCE(165);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 156:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'a') ADVANCE(180);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 157:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'a') ADVANCE(177);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 158:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'a') ADVANCE(183);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 159:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'd') ADVANCE(129);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 160:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'e') ADVANCE(155);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 161:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'e') ADVANCE(127);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 162:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'g') ADVANCE(125);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 163:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'h') ADVANCE(157);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 164:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(159);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 165:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(170);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 166:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(171);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 167:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'i') ADVANCE(182);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 168:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'l') ADVANCE(99);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 169:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'l') ADVANCE(176);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 170:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'm') ADVANCE(161);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 171:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(162);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 172:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(181);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 173:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'n') ADVANCE(184);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 174:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(175);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 175:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(168);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 176:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'o') ADVANCE(158);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 177:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'r') ADVANCE(123);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 178:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 'r') ADVANCE(166);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 179:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(178);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 180:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(160);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 181:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(144);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 182:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(97);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 183:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(121);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 184:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (lookahead == 't') ADVANCE(145);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 185:
      ACCEPT_TOKEN(aux_sym_type_identifier_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(185);
      END_STATE();
    case 186:
      ACCEPT_TOKEN(sym_unit);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 60},
  [2] = {.lex_state = 60},
  [3] = {.lex_state = 61},
  [4] = {.lex_state = 61},
  [5] = {.lex_state = 61},
  [6] = {.lex_state = 61},
  [7] = {.lex_state = 61},
  [8] = {.lex_state = 61},
  [9] = {.lex_state = 61},
  [10] = {.lex_state = 61},
  [11] = {.lex_state = 61},
  [12] = {.lex_state = 61},
  [13] = {.lex_state = 61},
  [14] = {.lex_state = 61},
  [15] = {.lex_state = 61},
  [16] = {.lex_state = 61},
  [17] = {.lex_state = 61},
  [18] = {.lex_state = 61},
  [19] = {.lex_state = 61},
  [20] = {.lex_state = 61},
  [21] = {.lex_state = 61},
  [22] = {.lex_state = 61},
  [23] = {.lex_state = 61},
  [24] = {.lex_state = 61},
  [25] = {.lex_state = 61},
  [26] = {.lex_state = 61},
  [27] = {.lex_state = 61},
  [28] = {.lex_state = 61},
  [29] = {.lex_state = 61},
  [30] = {.lex_state = 61},
  [31] = {.lex_state = 61},
  [32] = {.lex_state = 61},
  [33] = {.lex_state = 61},
  [34] = {.lex_state = 61},
  [35] = {.lex_state = 61},
  [36] = {.lex_state = 61},
  [37] = {.lex_state = 61},
  [38] = {.lex_state = 60},
  [39] = {.lex_state = 60},
  [40] = {.lex_state = 60},
  [41] = {.lex_state = 61},
  [42] = {.lex_state = 60},
  [43] = {.lex_state = 60},
  [44] = {.lex_state = 60},
  [45] = {.lex_state = 60},
  [46] = {.lex_state = 60},
  [47] = {.lex_state = 60},
  [48] = {.lex_state = 60},
  [49] = {.lex_state = 60},
  [50] = {.lex_state = 60},
  [51] = {.lex_state = 60},
  [52] = {.lex_state = 60},
  [53] = {.lex_state = 60},
  [54] = {.lex_state = 60},
  [55] = {.lex_state = 60},
  [56] = {.lex_state = 61},
  [57] = {.lex_state = 60},
  [58] = {.lex_state = 60},
  [59] = {.lex_state = 60},
  [60] = {.lex_state = 60},
  [61] = {.lex_state = 60},
  [62] = {.lex_state = 60},
  [63] = {.lex_state = 60},
  [64] = {.lex_state = 60},
  [65] = {.lex_state = 60},
  [66] = {.lex_state = 60},
  [67] = {.lex_state = 60},
  [68] = {.lex_state = 60},
  [69] = {.lex_state = 61},
  [70] = {.lex_state = 61},
  [71] = {.lex_state = 61},
  [72] = {.lex_state = 61},
  [73] = {.lex_state = 61},
  [74] = {.lex_state = 61},
  [75] = {.lex_state = 61},
  [76] = {.lex_state = 61},
  [77] = {.lex_state = 61},
  [78] = {.lex_state = 61},
  [79] = {.lex_state = 61},
  [80] = {.lex_state = 61},
  [81] = {.lex_state = 61},
  [82] = {.lex_state = 61},
  [83] = {.lex_state = 61},
  [84] = {.lex_state = 61},
  [85] = {.lex_state = 61},
  [86] = {.lex_state = 61},
  [87] = {.lex_state = 61},
  [88] = {.lex_state = 61},
  [89] = {.lex_state = 61},
  [90] = {.lex_state = 61},
  [91] = {.lex_state = 61},
  [92] = {.lex_state = 61},
  [93] = {.lex_state = 61},
  [94] = {.lex_state = 61},
  [95] = {.lex_state = 61},
  [96] = {.lex_state = 61},
  [97] = {.lex_state = 61},
  [98] = {.lex_state = 61},
  [99] = {.lex_state = 61},
  [100] = {.lex_state = 61},
  [101] = {.lex_state = 61},
  [102] = {.lex_state = 61},
  [103] = {.lex_state = 61},
  [104] = {.lex_state = 61},
  [105] = {.lex_state = 61},
  [106] = {.lex_state = 61},
  [107] = {.lex_state = 61},
  [108] = {.lex_state = 60},
  [109] = {.lex_state = 61},
  [110] = {.lex_state = 61},
  [111] = {.lex_state = 61},
  [112] = {.lex_state = 0},
  [113] = {.lex_state = 0},
  [114] = {.lex_state = 0},
  [115] = {.lex_state = 0},
  [116] = {.lex_state = 0},
  [117] = {.lex_state = 1},
  [118] = {.lex_state = 0},
  [119] = {.lex_state = 1},
  [120] = {.lex_state = 0},
  [121] = {.lex_state = 1},
  [122] = {.lex_state = 4},
  [123] = {.lex_state = 0},
  [124] = {.lex_state = 0},
  [125] = {.lex_state = 0},
  [126] = {.lex_state = 1},
  [127] = {.lex_state = 0},
  [128] = {.lex_state = 0},
  [129] = {.lex_state = 0},
  [130] = {.lex_state = 0},
  [131] = {.lex_state = 1},
  [132] = {.lex_state = 4},
  [133] = {.lex_state = 61},
  [134] = {.lex_state = 1},
  [135] = {.lex_state = 1},
  [136] = {.lex_state = 2},
  [137] = {.lex_state = 4},
  [138] = {.lex_state = 2},
  [139] = {.lex_state = 2},
  [140] = {.lex_state = 2},
  [141] = {.lex_state = 2},
  [142] = {.lex_state = 2},
  [143] = {.lex_state = 2},
  [144] = {.lex_state = 2},
  [145] = {.lex_state = 0},
  [146] = {.lex_state = 2},
  [147] = {.lex_state = 2},
  [148] = {.lex_state = 4},
  [149] = {.lex_state = 4},
  [150] = {.lex_state = 2},
  [151] = {.lex_state = 2},
  [152] = {.lex_state = 2},
  [153] = {.lex_state = 2},
  [154] = {.lex_state = 2},
  [155] = {.lex_state = 2},
  [156] = {.lex_state = 4},
  [157] = {.lex_state = 2},
  [158] = {.lex_state = 2},
  [159] = {.lex_state = 4},
  [160] = {.lex_state = 2},
  [161] = {.lex_state = 4},
  [162] = {.lex_state = 4},
  [163] = {.lex_state = 2},
  [164] = {.lex_state = 4},
  [165] = {.lex_state = 4},
  [166] = {.lex_state = 4},
  [167] = {.lex_state = 4},
  [168] = {.lex_state = 4},
  [169] = {.lex_state = 4},
  [170] = {.lex_state = 4},
  [171] = {.lex_state = 4},
  [172] = {.lex_state = 2},
  [173] = {.lex_state = 4},
  [174] = {.lex_state = 4},
  [175] = {.lex_state = 2},
  [176] = {.lex_state = 4},
  [177] = {.lex_state = 4},
  [178] = {.lex_state = 4},
  [179] = {.lex_state = 2},
  [180] = {.lex_state = 4},
  [181] = {.lex_state = 4},
  [182] = {.lex_state = 4},
  [183] = {.lex_state = 4},
  [184] = {.lex_state = 2},
  [185] = {.lex_state = 4},
  [186] = {.lex_state = 4},
  [187] = {.lex_state = 4},
  [188] = {.lex_state = 4},
  [189] = {.lex_state = 4},
  [190] = {.lex_state = 61},
  [191] = {.lex_state = 4},
  [192] = {.lex_state = 4},
  [193] = {.lex_state = 4},
  [194] = {.lex_state = 4},
  [195] = {.lex_state = 0},
  [196] = {.lex_state = 0},
  [197] = {.lex_state = 0},
  [198] = {.lex_state = 0},
  [199] = {.lex_state = 0},
  [200] = {.lex_state = 0},
  [201] = {.lex_state = 0},
  [202] = {.lex_state = 0},
  [203] = {.lex_state = 0},
  [204] = {.lex_state = 0},
  [205] = {.lex_state = 0},
  [206] = {.lex_state = 0},
  [207] = {.lex_state = 0},
  [208] = {.lex_state = 0},
  [209] = {.lex_state = 0},
  [210] = {.lex_state = 0},
  [211] = {.lex_state = 0},
  [212] = {.lex_state = 0},
  [213] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
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
    [anon_sym_y] = ACTIONS(1),
    [anon_sym_uy] = ACTIONS(1),
    [anon_sym_s] = ACTIONS(1),
    [anon_sym_us] = ACTIONS(1),
    [anon_sym_l] = ACTIONS(1),
    [anon_sym_ul] = ACTIONS(1),
    [anon_sym_L] = ACTIONS(1),
    [anon_sym_UL] = ACTIONS(1),
    [anon_sym_Q] = ACTIONS(1),
    [anon_sym_Z] = ACTIONS(1),
    [sym_float_literal] = ACTIONS(1),
    [sym_negative_digits] = ACTIONS(1),
    [sym_positive_digits] = ACTIONS(1),
    [aux_sym_builtin_type_token1] = ACTIONS(1),
    [aux_sym_builtin_type_token2] = ACTIONS(1),
    [aux_sym_builtin_type_token3] = ACTIONS(1),
    [aux_sym_builtin_type_token4] = ACTIONS(1),
    [aux_sym_builtin_type_token5] = ACTIONS(1),
    [aux_sym_builtin_type_token6] = ACTIONS(1),
    [aux_sym_builtin_type_token7] = ACTIONS(1),
    [aux_sym_builtin_type_token8] = ACTIONS(1),
    [aux_sym_builtin_type_token9] = ACTIONS(1),
    [aux_sym_builtin_type_token10] = ACTIONS(1),
    [aux_sym_builtin_type_token11] = ACTIONS(1),
    [aux_sym_builtin_type_token12] = ACTIONS(1),
    [aux_sym_builtin_type_token13] = ACTIONS(1),
    [aux_sym_builtin_type_token14] = ACTIONS(1),
    [aux_sym_builtin_type_token15] = ACTIONS(1),
    [aux_sym_builtin_type_token16] = ACTIONS(1),
    [aux_sym_builtin_type_token17] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [sym_unit] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(198),
    [sym_fn_decl] = STATE(2),
    [sym_type_decl] = STATE(2),
    [sym_expression] = STATE(82),
    [sym_paren_expression] = STATE(74),
    [sym_bool_literal] = STATE(74),
    [sym_function_call] = STATE(74),
    [sym_let_expression] = STATE(74),
    [sym_string_literal] = STATE(74),
    [sym_infix_operation] = STATE(74),
    [sym_int8_literal] = STATE(74),
    [sym_uint8_literal] = STATE(74),
    [sym_int16_literal] = STATE(74),
    [sym_uint16_literal] = STATE(74),
    [sym_int32_literal] = STATE(74),
    [sym_uint32_literal] = STATE(74),
    [sym_int64_literal] = STATE(74),
    [sym_uint64_literal] = STATE(74),
    [sym_int128_literal] = STATE(74),
    [sym_uint128_literal] = STATE(74),
    [sym_digits] = STATE(125),
    [sym_variable_identifier] = STATE(74),
    [aux_sym_source_file_repeat1] = STATE(2),
    [aux_sym_source_file_repeat2] = STATE(12),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_LPAREN] = ACTIONS(7),
    [anon_sym_type] = ACTIONS(9),
    [aux_sym_bool_literal_token1] = ACTIONS(11),
    [aux_sym_bool_literal_token2] = ACTIONS(11),
    [anon_sym_DQUOTE] = ACTIONS(13),
    [sym_float_literal] = ACTIONS(15),
    [sym_negative_digits] = ACTIONS(17),
    [sym_positive_digits] = ACTIONS(19),
    [aux_sym_variable_identifier_token1] = ACTIONS(21),
    [sym_unit] = ACTIONS(15),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 15,
    ACTIONS(5), 1,
      anon_sym_let,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 1,
      anon_sym_type,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(19), 1,
      sym_positive_digits,
    ACTIONS(21), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(23), 1,
      ts_builtin_sym_end,
    STATE(11), 1,
      aux_sym_source_file_repeat2,
    STATE(82), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(15), 2,
      sym_float_literal,
      sym_unit,
    STATE(43), 3,
      sym_fn_decl,
      sym_type_decl,
      aux_sym_source_file_repeat1,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [66] = 16,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(37), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    STATE(19), 1,
      sym_qualified_fn_name,
    STATE(109), 1,
      sym_fn_identifier,
    STATE(122), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(128), 1,
      sym_digits,
    STATE(183), 1,
      sym_expression,
    STATE(204), 1,
      sym_module_identifier,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [133] = 16,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(37), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    STATE(20), 1,
      sym_qualified_fn_name,
    STATE(109), 1,
      sym_fn_identifier,
    STATE(122), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(128), 1,
      sym_digits,
    STATE(169), 1,
      sym_expression,
    STATE(204), 1,
      sym_module_identifier,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [200] = 16,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(37), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    STATE(17), 1,
      sym_qualified_fn_name,
    STATE(109), 1,
      sym_fn_identifier,
    STATE(122), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(128), 1,
      sym_digits,
    STATE(185), 1,
      sym_expression,
    STATE(204), 1,
      sym_module_identifier,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [267] = 16,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(37), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    STATE(21), 1,
      sym_qualified_fn_name,
    STATE(109), 1,
      sym_fn_identifier,
    STATE(122), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(128), 1,
      sym_digits,
    STATE(164), 1,
      sym_expression,
    STATE(204), 1,
      sym_module_identifier,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [334] = 16,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(37), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    STATE(18), 1,
      sym_qualified_fn_name,
    STATE(109), 1,
      sym_fn_identifier,
    STATE(122), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(128), 1,
      sym_digits,
    STATE(176), 1,
      sym_expression,
    STATE(204), 1,
      sym_module_identifier,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [401] = 13,
    ACTIONS(41), 1,
      ts_builtin_sym_end,
    ACTIONS(43), 1,
      anon_sym_let,
    ACTIONS(46), 1,
      anon_sym_LPAREN,
    ACTIONS(52), 1,
      anon_sym_DQUOTE,
    ACTIONS(58), 1,
      sym_negative_digits,
    ACTIONS(61), 1,
      sym_positive_digits,
    ACTIONS(64), 1,
      aux_sym_variable_identifier_token1,
    STATE(8), 1,
      aux_sym_source_file_repeat2,
    STATE(82), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(49), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(55), 2,
      sym_float_literal,
      sym_unit,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [459] = 13,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(71), 1,
      anon_sym_RPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [517] = 13,
    ACTIONS(41), 1,
      anon_sym_RPAREN,
    ACTIONS(58), 1,
      sym_negative_digits,
    ACTIONS(83), 1,
      anon_sym_let,
    ACTIONS(86), 1,
      anon_sym_LPAREN,
    ACTIONS(92), 1,
      anon_sym_DQUOTE,
    ACTIONS(98), 1,
      sym_positive_digits,
    ACTIONS(101), 1,
      aux_sym_variable_identifier_token1,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(89), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(95), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [575] = 13,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(19), 1,
      sym_positive_digits,
    ACTIONS(21), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(104), 1,
      ts_builtin_sym_end,
    ACTIONS(106), 1,
      anon_sym_let,
    STATE(8), 1,
      aux_sym_source_file_repeat2,
    STATE(82), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(15), 2,
      sym_float_literal,
      sym_unit,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [633] = 13,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(19), 1,
      sym_positive_digits,
    ACTIONS(21), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(23), 1,
      ts_builtin_sym_end,
    ACTIONS(106), 1,
      anon_sym_let,
    STATE(8), 1,
      aux_sym_source_file_repeat2,
    STATE(82), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(15), 2,
      sym_float_literal,
      sym_unit,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [691] = 13,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(108), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [749] = 13,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(110), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [807] = 13,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(112), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [865] = 13,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(114), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [923] = 12,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(9), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [978] = 12,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(16), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1033] = 12,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(15), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1088] = 12,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(13), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1143] = 12,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(14), 1,
      aux_sym_source_file_repeat2,
    STATE(86), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1198] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(144), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1250] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(179), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1302] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(130), 1,
      anon_sym_let,
    ACTIONS(132), 1,
      anon_sym_LPAREN,
    ACTIONS(136), 1,
      anon_sym_DQUOTE,
    ACTIONS(140), 1,
      sym_positive_digits,
    ACTIONS(142), 1,
      aux_sym_variable_identifier_token1,
    STATE(60), 1,
      sym_expression,
    STATE(123), 1,
      sym_digits,
    ACTIONS(134), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(138), 2,
      sym_float_literal,
      sym_unit,
    STATE(49), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1354] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(130), 1,
      anon_sym_let,
    ACTIONS(132), 1,
      anon_sym_LPAREN,
    ACTIONS(136), 1,
      anon_sym_DQUOTE,
    ACTIONS(140), 1,
      sym_positive_digits,
    ACTIONS(142), 1,
      aux_sym_variable_identifier_token1,
    STATE(67), 1,
      sym_expression,
    STATE(123), 1,
      sym_digits,
    ACTIONS(134), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(138), 2,
      sym_float_literal,
      sym_unit,
    STATE(49), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1406] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(144), 1,
      aux_sym_variable_identifier_token1,
    STATE(128), 1,
      sym_digits,
    STATE(166), 1,
      sym_expression,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1458] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(184), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1510] = 11,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(19), 1,
      sym_positive_digits,
    ACTIONS(21), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(106), 1,
      anon_sym_let,
    STATE(80), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(15), 2,
      sym_float_literal,
      sym_unit,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1562] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(25), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    ACTIONS(35), 1,
      sym_positive_digits,
    ACTIONS(144), 1,
      aux_sym_variable_identifier_token1,
    STATE(128), 1,
      sym_digits,
    STATE(162), 1,
      sym_expression,
    ACTIONS(29), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(33), 2,
      sym_float_literal,
      sym_unit,
    STATE(149), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1614] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(90), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1666] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(67), 1,
      anon_sym_let,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    ACTIONS(79), 1,
      sym_positive_digits,
    ACTIONS(81), 1,
      aux_sym_variable_identifier_token1,
    STATE(88), 1,
      sym_expression,
    STATE(118), 1,
      sym_digits,
    ACTIONS(73), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(77), 2,
      sym_float_literal,
      sym_unit,
    STATE(104), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1718] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(136), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1770] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(141), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1822] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(172), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1874] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(130), 1,
      anon_sym_let,
    ACTIONS(132), 1,
      anon_sym_LPAREN,
    ACTIONS(136), 1,
      anon_sym_DQUOTE,
    ACTIONS(140), 1,
      sym_positive_digits,
    ACTIONS(142), 1,
      aux_sym_variable_identifier_token1,
    STATE(63), 1,
      sym_expression,
    STATE(123), 1,
      sym_digits,
    ACTIONS(134), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(138), 2,
      sym_float_literal,
      sym_unit,
    STATE(49), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1926] = 11,
    ACTIONS(7), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(19), 1,
      sym_positive_digits,
    ACTIONS(21), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(106), 1,
      anon_sym_let,
    STATE(71), 1,
      sym_expression,
    STATE(125), 1,
      sym_digits,
    ACTIONS(11), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(15), 2,
      sym_float_literal,
      sym_unit,
    STATE(74), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [1978] = 11,
    ACTIONS(17), 1,
      sym_negative_digits,
    ACTIONS(116), 1,
      anon_sym_let,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    ACTIONS(122), 1,
      anon_sym_DQUOTE,
    ACTIONS(126), 1,
      sym_positive_digits,
    ACTIONS(128), 1,
      aux_sym_variable_identifier_token1,
    STATE(120), 1,
      sym_digits,
    STATE(163), 1,
      sym_expression,
    ACTIONS(120), 2,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
    ACTIONS(124), 2,
      sym_float_literal,
      sym_unit,
    STATE(157), 17,
      sym_paren_expression,
      sym_bool_literal,
      sym_function_call,
      sym_let_expression,
      sym_string_literal,
      sym_infix_operation,
      sym_int8_literal,
      sym_uint8_literal,
      sym_int16_literal,
      sym_uint16_literal,
      sym_int32_literal,
      sym_uint32_literal,
      sym_int64_literal,
      sym_uint64_literal,
      sym_int128_literal,
      sym_uint128_literal,
      sym_variable_identifier,
  [2030] = 7,
    ACTIONS(148), 1,
      aux_sym_type_identifier_token1,
    STATE(51), 1,
      sym_type_identifier,
    STATE(133), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(202), 1,
      sym_type_reference,
    STATE(204), 1,
      sym_module_identifier,
    STATE(68), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(146), 17,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
      aux_sym_builtin_type_token7,
      aux_sym_builtin_type_token8,
      aux_sym_builtin_type_token9,
      aux_sym_builtin_type_token10,
      aux_sym_builtin_type_token11,
      aux_sym_builtin_type_token12,
      aux_sym_builtin_type_token13,
      aux_sym_builtin_type_token14,
      aux_sym_builtin_type_token15,
      aux_sym_builtin_type_token16,
      aux_sym_builtin_type_token17,
  [2069] = 7,
    ACTIONS(148), 1,
      aux_sym_type_identifier_token1,
    STATE(51), 1,
      sym_type_identifier,
    STATE(133), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(201), 1,
      sym_type_reference,
    STATE(204), 1,
      sym_module_identifier,
    STATE(68), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(146), 17,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
      aux_sym_builtin_type_token7,
      aux_sym_builtin_type_token8,
      aux_sym_builtin_type_token9,
      aux_sym_builtin_type_token10,
      aux_sym_builtin_type_token11,
      aux_sym_builtin_type_token12,
      aux_sym_builtin_type_token13,
      aux_sym_builtin_type_token14,
      aux_sym_builtin_type_token15,
      aux_sym_builtin_type_token16,
      aux_sym_builtin_type_token17,
  [2108] = 7,
    ACTIONS(148), 1,
      aux_sym_type_identifier_token1,
    STATE(51), 1,
      sym_type_identifier,
    STATE(108), 1,
      sym_type_reference,
    STATE(133), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(204), 1,
      sym_module_identifier,
    STATE(68), 2,
      sym_builtin_type,
      sym_qualified_type_name,
    ACTIONS(146), 17,
      aux_sym_builtin_type_token1,
      aux_sym_builtin_type_token2,
      aux_sym_builtin_type_token3,
      aux_sym_builtin_type_token4,
      aux_sym_builtin_type_token5,
      aux_sym_builtin_type_token6,
      aux_sym_builtin_type_token7,
      aux_sym_builtin_type_token8,
      aux_sym_builtin_type_token9,
      aux_sym_builtin_type_token10,
      aux_sym_builtin_type_token11,
      aux_sym_builtin_type_token12,
      aux_sym_builtin_type_token13,
      aux_sym_builtin_type_token14,
      aux_sym_builtin_type_token15,
      aux_sym_builtin_type_token16,
      aux_sym_builtin_type_token17,
  [2147] = 2,
    ACTIONS(150), 6,
      ts_builtin_sym_end,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(152), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2167] = 3,
    ACTIONS(158), 1,
      anon_sym_DOT,
    ACTIONS(154), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(156), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2189] = 5,
    ACTIONS(162), 1,
      anon_sym_let,
    ACTIONS(167), 1,
      anon_sym_type,
    STATE(43), 3,
      sym_fn_decl,
      sym_type_decl,
      aux_sym_source_file_repeat1,
    ACTIONS(160), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(165), 6,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2215] = 2,
    ACTIONS(170), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(172), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2234] = 2,
    ACTIONS(174), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(176), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2253] = 2,
    ACTIONS(178), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(180), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2272] = 2,
    ACTIONS(182), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(184), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2291] = 2,
    ACTIONS(150), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(152), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2310] = 2,
    ACTIONS(186), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(188), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2329] = 2,
    ACTIONS(190), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(192), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2348] = 2,
    ACTIONS(194), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(196), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2367] = 2,
    ACTIONS(198), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(200), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2386] = 2,
    ACTIONS(202), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(204), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2405] = 2,
    ACTIONS(206), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(208), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2424] = 2,
    ACTIONS(210), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(212), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2443] = 4,
    ACTIONS(150), 2,
      anon_sym_EQ,
      anon_sym_RPAREN,
    ACTIONS(152), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(216), 3,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(214), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2466] = 2,
    ACTIONS(218), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(220), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2485] = 2,
    ACTIONS(222), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(224), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2504] = 2,
    ACTIONS(226), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(228), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2523] = 3,
    ACTIONS(234), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(230), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(232), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2544] = 2,
    ACTIONS(236), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(238), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2563] = 2,
    ACTIONS(240), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(242), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2582] = 2,
    ACTIONS(244), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(246), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2601] = 2,
    ACTIONS(248), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(250), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2620] = 2,
    ACTIONS(252), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(254), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2639] = 2,
    ACTIONS(256), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(258), 10,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2658] = 3,
    ACTIONS(234), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(260), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(262), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2679] = 2,
    ACTIONS(264), 6,
      ts_builtin_sym_end,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(266), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2698] = 2,
    ACTIONS(240), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(242), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2716] = 2,
    ACTIONS(190), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(192), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2734] = 2,
    ACTIONS(244), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(246), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2752] = 2,
    ACTIONS(190), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(192), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2770] = 2,
    ACTIONS(174), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(176), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2788] = 2,
    ACTIONS(186), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(188), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2806] = 2,
    ACTIONS(206), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(208), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2824] = 2,
    ACTIONS(248), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(250), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2842] = 2,
    ACTIONS(202), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(204), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2860] = 2,
    ACTIONS(198), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(200), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2878] = 2,
    ACTIONS(170), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(172), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2896] = 3,
    ACTIONS(268), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(260), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(262), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2916] = 2,
    ACTIONS(218), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(220), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2934] = 3,
    ACTIONS(268), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(270), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(272), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2954] = 2,
    ACTIONS(226), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(228), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2972] = 2,
    ACTIONS(178), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(180), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [2990] = 2,
    ACTIONS(150), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(152), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3008] = 3,
    ACTIONS(274), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(270), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(272), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3028] = 2,
    ACTIONS(210), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(212), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3046] = 3,
    ACTIONS(274), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(260), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(262), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3066] = 2,
    ACTIONS(248), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(250), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3084] = 2,
    ACTIONS(244), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(246), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3102] = 2,
    ACTIONS(222), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(224), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3120] = 2,
    ACTIONS(174), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(176), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3138] = 2,
    ACTIONS(206), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(208), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3156] = 2,
    ACTIONS(202), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(204), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3174] = 2,
    ACTIONS(198), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(200), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3192] = 2,
    ACTIONS(170), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(172), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3210] = 2,
    ACTIONS(218), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(220), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3228] = 2,
    ACTIONS(222), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(224), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3246] = 2,
    ACTIONS(226), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(228), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3264] = 2,
    ACTIONS(178), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(180), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3282] = 2,
    ACTIONS(182), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(184), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3300] = 2,
    ACTIONS(210), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(212), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3318] = 2,
    ACTIONS(256), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(258), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3336] = 2,
    ACTIONS(186), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(188), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3354] = 2,
    ACTIONS(240), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(242), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3372] = 2,
    ACTIONS(182), 4,
      anon_sym_RPAREN,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(184), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3390] = 2,
    ACTIONS(256), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(258), 9,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3408] = 2,
    ACTIONS(276), 4,
      ts_builtin_sym_end,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(278), 8,
      anon_sym_let,
      anon_sym_LPAREN,
      anon_sym_type,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3425] = 2,
    ACTIONS(282), 3,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(280), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3440] = 2,
    ACTIONS(216), 3,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(214), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3455] = 2,
    ACTIONS(286), 3,
      anon_sym_DQUOTE,
      sym_float_literal,
      sym_unit,
    ACTIONS(284), 7,
      anon_sym_let,
      anon_sym_LPAREN,
      aux_sym_bool_literal_token1,
      aux_sym_bool_literal_token2,
      sym_negative_digits,
      sym_positive_digits,
      aux_sym_variable_identifier_token1,
  [3470] = 6,
    ACTIONS(290), 1,
      anon_sym_uy,
    ACTIONS(292), 1,
      anon_sym_us,
    ACTIONS(294), 1,
      anon_sym_ul,
    ACTIONS(296), 1,
      anon_sym_UL,
    ACTIONS(298), 1,
      anon_sym_Z,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3493] = 6,
    ACTIONS(300), 1,
      anon_sym_uy,
    ACTIONS(302), 1,
      anon_sym_us,
    ACTIONS(304), 1,
      anon_sym_ul,
    ACTIONS(306), 1,
      anon_sym_UL,
    ACTIONS(308), 1,
      anon_sym_Z,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3516] = 6,
    ACTIONS(310), 1,
      anon_sym_uy,
    ACTIONS(312), 1,
      anon_sym_us,
    ACTIONS(314), 1,
      anon_sym_ul,
    ACTIONS(316), 1,
      anon_sym_UL,
    ACTIONS(318), 1,
      anon_sym_Z,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3539] = 6,
    ACTIONS(320), 1,
      anon_sym_uy,
    ACTIONS(322), 1,
      anon_sym_us,
    ACTIONS(324), 1,
      anon_sym_ul,
    ACTIONS(326), 1,
      anon_sym_UL,
    ACTIONS(328), 1,
      anon_sym_Z,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3562] = 6,
    ACTIONS(330), 1,
      anon_sym_uy,
    ACTIONS(332), 1,
      anon_sym_us,
    ACTIONS(334), 1,
      anon_sym_ul,
    ACTIONS(336), 1,
      anon_sym_UL,
    ACTIONS(338), 1,
      anon_sym_Z,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3585] = 4,
    ACTIONS(340), 1,
      anon_sym_DQUOTE,
    STATE(135), 1,
      aux_sym_string_content_repeat1,
    STATE(200), 1,
      sym_string_content,
    ACTIONS(342), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3599] = 5,
    ACTIONS(344), 1,
      anon_sym_y,
    ACTIONS(346), 1,
      anon_sym_s,
    ACTIONS(348), 1,
      anon_sym_l,
    ACTIONS(350), 1,
      anon_sym_L,
    ACTIONS(352), 1,
      anon_sym_Q,
  [3615] = 4,
    ACTIONS(354), 1,
      anon_sym_DQUOTE,
    STATE(135), 1,
      aux_sym_string_content_repeat1,
    STATE(207), 1,
      sym_string_content,
    ACTIONS(342), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3629] = 5,
    ACTIONS(356), 1,
      anon_sym_y,
    ACTIONS(358), 1,
      anon_sym_s,
    ACTIONS(360), 1,
      anon_sym_l,
    ACTIONS(362), 1,
      anon_sym_L,
    ACTIONS(364), 1,
      anon_sym_Q,
  [3645] = 4,
    ACTIONS(366), 1,
      anon_sym_DQUOTE,
    STATE(135), 1,
      aux_sym_string_content_repeat1,
    STATE(205), 1,
      sym_string_content,
    ACTIONS(342), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3659] = 5,
    ACTIONS(39), 1,
      aux_sym_type_identifier_token1,
    ACTIONS(368), 1,
      aux_sym_variable_identifier_token1,
    STATE(111), 1,
      sym_fn_identifier,
    STATE(132), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(204), 1,
      sym_module_identifier,
  [3675] = 5,
    ACTIONS(370), 1,
      anon_sym_y,
    ACTIONS(372), 1,
      anon_sym_s,
    ACTIONS(374), 1,
      anon_sym_l,
    ACTIONS(376), 1,
      anon_sym_L,
    ACTIONS(378), 1,
      anon_sym_Q,
  [3691] = 4,
    ACTIONS(380), 1,
      anon_sym_LPAREN,
    ACTIONS(382), 1,
      sym_unit,
    STATE(211), 1,
      sym_fn_decl_params,
    STATE(130), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [3705] = 5,
    ACTIONS(384), 1,
      anon_sym_y,
    ACTIONS(386), 1,
      anon_sym_s,
    ACTIONS(388), 1,
      anon_sym_l,
    ACTIONS(390), 1,
      anon_sym_L,
    ACTIONS(392), 1,
      anon_sym_Q,
  [3721] = 4,
    ACTIONS(394), 1,
      anon_sym_DQUOTE,
    STATE(135), 1,
      aux_sym_string_content_repeat1,
    STATE(197), 1,
      sym_string_content,
    ACTIONS(342), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3735] = 1,
    ACTIONS(288), 5,
      anon_sym_y,
      anon_sym_s,
      anon_sym_l,
      anon_sym_L,
      anon_sym_Q,
  [3743] = 5,
    ACTIONS(396), 1,
      anon_sym_y,
    ACTIONS(398), 1,
      anon_sym_s,
    ACTIONS(400), 1,
      anon_sym_l,
    ACTIONS(402), 1,
      anon_sym_L,
    ACTIONS(404), 1,
      anon_sym_Q,
  [3759] = 4,
    ACTIONS(406), 1,
      anon_sym_COLON,
    ACTIONS(408), 1,
      anon_sym_LPAREN,
    ACTIONS(411), 1,
      sym_unit,
    STATE(129), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [3773] = 4,
    ACTIONS(380), 1,
      anon_sym_LPAREN,
    ACTIONS(414), 1,
      anon_sym_COLON,
    ACTIONS(416), 1,
      sym_unit,
    STATE(129), 2,
      sym_fn_decl_param,
      aux_sym_fn_decl_params_repeat1,
  [3787] = 4,
    ACTIONS(418), 1,
      anon_sym_DQUOTE,
    STATE(135), 1,
      aux_sym_string_content_repeat1,
    STATE(213), 1,
      sym_string_content,
    ACTIONS(342), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3801] = 4,
    ACTIONS(420), 1,
      aux_sym_variable_identifier_token1,
    ACTIONS(422), 1,
      aux_sym_type_identifier_token1,
    STATE(132), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(204), 1,
      sym_module_identifier,
  [3814] = 4,
    ACTIONS(425), 1,
      aux_sym_type_identifier_token1,
    STATE(61), 1,
      sym_type_identifier,
    STATE(132), 1,
      aux_sym_qualified_fn_name_repeat1,
    STATE(204), 1,
      sym_module_identifier,
  [3827] = 3,
    ACTIONS(427), 1,
      anon_sym_DQUOTE,
    STATE(134), 1,
      aux_sym_string_content_repeat1,
    ACTIONS(429), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3838] = 3,
    ACTIONS(432), 1,
      anon_sym_DQUOTE,
    STATE(134), 1,
      aux_sym_string_content_repeat1,
    ACTIONS(434), 2,
      aux_sym_string_content_token1,
      sym_string_escape_sequence,
  [3849] = 2,
    ACTIONS(244), 1,
      anon_sym_LF,
    ACTIONS(246), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3857] = 1,
    ACTIONS(198), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3863] = 2,
    ACTIONS(206), 1,
      anon_sym_LF,
    ACTIONS(208), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3871] = 2,
    ACTIONS(174), 1,
      anon_sym_LF,
    ACTIONS(176), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3879] = 2,
    ACTIONS(222), 1,
      anon_sym_LF,
    ACTIONS(224), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3887] = 2,
    ACTIONS(436), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3895] = 2,
    ACTIONS(248), 1,
      anon_sym_LF,
    ACTIONS(250), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3903] = 2,
    ACTIONS(198), 1,
      anon_sym_LF,
    ACTIONS(200), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3911] = 2,
    ACTIONS(260), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3919] = 2,
    ACTIONS(442), 1,
      anon_sym_LPAREN,
    ACTIONS(440), 2,
      anon_sym_COLON,
      sym_unit,
  [3927] = 2,
    ACTIONS(170), 1,
      anon_sym_LF,
    ACTIONS(172), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3935] = 2,
    ACTIONS(218), 1,
      anon_sym_LF,
    ACTIONS(220), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3943] = 1,
    ACTIONS(190), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3949] = 1,
    ACTIONS(186), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3955] = 2,
    ACTIONS(226), 1,
      anon_sym_LF,
    ACTIONS(228), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3963] = 2,
    ACTIONS(178), 1,
      anon_sym_LF,
    ACTIONS(180), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3971] = 2,
    ACTIONS(210), 1,
      anon_sym_LF,
    ACTIONS(212), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3979] = 2,
    ACTIONS(256), 1,
      anon_sym_LF,
    ACTIONS(258), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3987] = 2,
    ACTIONS(240), 1,
      anon_sym_LF,
    ACTIONS(242), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [3995] = 2,
    ACTIONS(182), 1,
      anon_sym_LF,
    ACTIONS(184), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4003] = 1,
    ACTIONS(150), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4009] = 2,
    ACTIONS(186), 1,
      anon_sym_LF,
    ACTIONS(188), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4017] = 2,
    ACTIONS(190), 1,
      anon_sym_LF,
    ACTIONS(192), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4025] = 3,
    ACTIONS(444), 1,
      aux_sym_variable_identifier_token1,
    STATE(124), 1,
      sym_fn_identifier,
    STATE(199), 1,
      sym_variable_identifier,
  [4035] = 2,
    ACTIONS(150), 1,
      anon_sym_LF,
    ACTIONS(152), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4043] = 1,
    ACTIONS(182), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4049] = 2,
    ACTIONS(260), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4057] = 2,
    ACTIONS(448), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4065] = 2,
    ACTIONS(450), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4073] = 1,
    ACTIONS(248), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4079] = 1,
    ACTIONS(244), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4085] = 1,
    ACTIONS(222), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4091] = 1,
    ACTIONS(174), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4097] = 2,
    ACTIONS(452), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4105] = 1,
    ACTIONS(240), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4111] = 1,
    ACTIONS(256), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4117] = 2,
    ACTIONS(454), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4125] = 1,
    ACTIONS(206), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4131] = 1,
    ACTIONS(202), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4137] = 2,
    ACTIONS(202), 1,
      anon_sym_LF,
    ACTIONS(204), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4145] = 2,
    ACTIONS(456), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4153] = 1,
    ACTIONS(210), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4159] = 1,
    ACTIONS(170), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4165] = 2,
    ACTIONS(458), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4173] = 1,
    ACTIONS(218), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4179] = 1,
    ACTIONS(226), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4185] = 1,
    ACTIONS(178), 3,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4191] = 2,
    ACTIONS(460), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4199] = 2,
    ACTIONS(462), 1,
      anon_sym_LF,
    ACTIONS(438), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4207] = 2,
    ACTIONS(464), 1,
      anon_sym_RPAREN,
    ACTIONS(446), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [4215] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(199), 1,
      sym_variable_identifier,
  [4222] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(203), 1,
      sym_variable_identifier,
  [4229] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(210), 1,
      sym_variable_identifier,
  [4236] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(209), 1,
      sym_variable_identifier,
  [4243] = 2,
    ACTIONS(468), 1,
      aux_sym_type_identifier_token1,
    STATE(195), 1,
      sym_type_identifier,
  [4250] = 2,
    ACTIONS(368), 1,
      aux_sym_variable_identifier_token1,
    STATE(124), 1,
      sym_fn_identifier,
  [4257] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(208), 1,
      sym_variable_identifier,
  [4264] = 1,
    ACTIONS(420), 2,
      aux_sym_variable_identifier_token1,
      aux_sym_type_identifier_token1,
  [4269] = 2,
    ACTIONS(466), 1,
      aux_sym_variable_identifier_token1,
    STATE(206), 1,
      sym_variable_identifier,
  [4276] = 1,
    ACTIONS(470), 1,
      anon_sym_EQ,
  [4280] = 1,
    ACTIONS(158), 1,
      anon_sym_DOT,
  [4284] = 1,
    ACTIONS(472), 1,
      anon_sym_DQUOTE,
  [4288] = 1,
    ACTIONS(474), 1,
      ts_builtin_sym_end,
  [4292] = 1,
    ACTIONS(476), 1,
      anon_sym_EQ,
  [4296] = 1,
    ACTIONS(478), 1,
      anon_sym_DQUOTE,
  [4300] = 1,
    ACTIONS(480), 1,
      anon_sym_RPAREN,
  [4304] = 1,
    ACTIONS(482), 1,
      anon_sym_EQ,
  [4308] = 1,
    ACTIONS(484), 1,
      anon_sym_COLON,
  [4312] = 1,
    ACTIONS(486), 1,
      anon_sym_DOT,
  [4316] = 1,
    ACTIONS(488), 1,
      anon_sym_DQUOTE,
  [4320] = 1,
    ACTIONS(490), 1,
      anon_sym_EQ,
  [4324] = 1,
    ACTIONS(492), 1,
      anon_sym_DQUOTE,
  [4328] = 1,
    ACTIONS(494), 1,
      anon_sym_EQ,
  [4332] = 1,
    ACTIONS(496), 1,
      anon_sym_EQ,
  [4336] = 1,
    ACTIONS(498), 1,
      anon_sym_EQ,
  [4340] = 1,
    ACTIONS(500), 1,
      anon_sym_COLON,
  [4344] = 1,
    ACTIONS(154), 1,
      anon_sym_EQ,
  [4348] = 1,
    ACTIONS(502), 1,
      anon_sym_DQUOTE,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 66,
  [SMALL_STATE(4)] = 133,
  [SMALL_STATE(5)] = 200,
  [SMALL_STATE(6)] = 267,
  [SMALL_STATE(7)] = 334,
  [SMALL_STATE(8)] = 401,
  [SMALL_STATE(9)] = 459,
  [SMALL_STATE(10)] = 517,
  [SMALL_STATE(11)] = 575,
  [SMALL_STATE(12)] = 633,
  [SMALL_STATE(13)] = 691,
  [SMALL_STATE(14)] = 749,
  [SMALL_STATE(15)] = 807,
  [SMALL_STATE(16)] = 865,
  [SMALL_STATE(17)] = 923,
  [SMALL_STATE(18)] = 978,
  [SMALL_STATE(19)] = 1033,
  [SMALL_STATE(20)] = 1088,
  [SMALL_STATE(21)] = 1143,
  [SMALL_STATE(22)] = 1198,
  [SMALL_STATE(23)] = 1250,
  [SMALL_STATE(24)] = 1302,
  [SMALL_STATE(25)] = 1354,
  [SMALL_STATE(26)] = 1406,
  [SMALL_STATE(27)] = 1458,
  [SMALL_STATE(28)] = 1510,
  [SMALL_STATE(29)] = 1562,
  [SMALL_STATE(30)] = 1614,
  [SMALL_STATE(31)] = 1666,
  [SMALL_STATE(32)] = 1718,
  [SMALL_STATE(33)] = 1770,
  [SMALL_STATE(34)] = 1822,
  [SMALL_STATE(35)] = 1874,
  [SMALL_STATE(36)] = 1926,
  [SMALL_STATE(37)] = 1978,
  [SMALL_STATE(38)] = 2030,
  [SMALL_STATE(39)] = 2069,
  [SMALL_STATE(40)] = 2108,
  [SMALL_STATE(41)] = 2147,
  [SMALL_STATE(42)] = 2167,
  [SMALL_STATE(43)] = 2189,
  [SMALL_STATE(44)] = 2215,
  [SMALL_STATE(45)] = 2234,
  [SMALL_STATE(46)] = 2253,
  [SMALL_STATE(47)] = 2272,
  [SMALL_STATE(48)] = 2291,
  [SMALL_STATE(49)] = 2310,
  [SMALL_STATE(50)] = 2329,
  [SMALL_STATE(51)] = 2348,
  [SMALL_STATE(52)] = 2367,
  [SMALL_STATE(53)] = 2386,
  [SMALL_STATE(54)] = 2405,
  [SMALL_STATE(55)] = 2424,
  [SMALL_STATE(56)] = 2443,
  [SMALL_STATE(57)] = 2466,
  [SMALL_STATE(58)] = 2485,
  [SMALL_STATE(59)] = 2504,
  [SMALL_STATE(60)] = 2523,
  [SMALL_STATE(61)] = 2544,
  [SMALL_STATE(62)] = 2563,
  [SMALL_STATE(63)] = 2582,
  [SMALL_STATE(64)] = 2601,
  [SMALL_STATE(65)] = 2620,
  [SMALL_STATE(66)] = 2639,
  [SMALL_STATE(67)] = 2658,
  [SMALL_STATE(68)] = 2679,
  [SMALL_STATE(69)] = 2698,
  [SMALL_STATE(70)] = 2716,
  [SMALL_STATE(71)] = 2734,
  [SMALL_STATE(72)] = 2752,
  [SMALL_STATE(73)] = 2770,
  [SMALL_STATE(74)] = 2788,
  [SMALL_STATE(75)] = 2806,
  [SMALL_STATE(76)] = 2824,
  [SMALL_STATE(77)] = 2842,
  [SMALL_STATE(78)] = 2860,
  [SMALL_STATE(79)] = 2878,
  [SMALL_STATE(80)] = 2896,
  [SMALL_STATE(81)] = 2916,
  [SMALL_STATE(82)] = 2934,
  [SMALL_STATE(83)] = 2954,
  [SMALL_STATE(84)] = 2972,
  [SMALL_STATE(85)] = 2990,
  [SMALL_STATE(86)] = 3008,
  [SMALL_STATE(87)] = 3028,
  [SMALL_STATE(88)] = 3046,
  [SMALL_STATE(89)] = 3066,
  [SMALL_STATE(90)] = 3084,
  [SMALL_STATE(91)] = 3102,
  [SMALL_STATE(92)] = 3120,
  [SMALL_STATE(93)] = 3138,
  [SMALL_STATE(94)] = 3156,
  [SMALL_STATE(95)] = 3174,
  [SMALL_STATE(96)] = 3192,
  [SMALL_STATE(97)] = 3210,
  [SMALL_STATE(98)] = 3228,
  [SMALL_STATE(99)] = 3246,
  [SMALL_STATE(100)] = 3264,
  [SMALL_STATE(101)] = 3282,
  [SMALL_STATE(102)] = 3300,
  [SMALL_STATE(103)] = 3318,
  [SMALL_STATE(104)] = 3336,
  [SMALL_STATE(105)] = 3354,
  [SMALL_STATE(106)] = 3372,
  [SMALL_STATE(107)] = 3390,
  [SMALL_STATE(108)] = 3408,
  [SMALL_STATE(109)] = 3425,
  [SMALL_STATE(110)] = 3440,
  [SMALL_STATE(111)] = 3455,
  [SMALL_STATE(112)] = 3470,
  [SMALL_STATE(113)] = 3493,
  [SMALL_STATE(114)] = 3516,
  [SMALL_STATE(115)] = 3539,
  [SMALL_STATE(116)] = 3562,
  [SMALL_STATE(117)] = 3585,
  [SMALL_STATE(118)] = 3599,
  [SMALL_STATE(119)] = 3615,
  [SMALL_STATE(120)] = 3629,
  [SMALL_STATE(121)] = 3645,
  [SMALL_STATE(122)] = 3659,
  [SMALL_STATE(123)] = 3675,
  [SMALL_STATE(124)] = 3691,
  [SMALL_STATE(125)] = 3705,
  [SMALL_STATE(126)] = 3721,
  [SMALL_STATE(127)] = 3735,
  [SMALL_STATE(128)] = 3743,
  [SMALL_STATE(129)] = 3759,
  [SMALL_STATE(130)] = 3773,
  [SMALL_STATE(131)] = 3787,
  [SMALL_STATE(132)] = 3801,
  [SMALL_STATE(133)] = 3814,
  [SMALL_STATE(134)] = 3827,
  [SMALL_STATE(135)] = 3838,
  [SMALL_STATE(136)] = 3849,
  [SMALL_STATE(137)] = 3857,
  [SMALL_STATE(138)] = 3863,
  [SMALL_STATE(139)] = 3871,
  [SMALL_STATE(140)] = 3879,
  [SMALL_STATE(141)] = 3887,
  [SMALL_STATE(142)] = 3895,
  [SMALL_STATE(143)] = 3903,
  [SMALL_STATE(144)] = 3911,
  [SMALL_STATE(145)] = 3919,
  [SMALL_STATE(146)] = 3927,
  [SMALL_STATE(147)] = 3935,
  [SMALL_STATE(148)] = 3943,
  [SMALL_STATE(149)] = 3949,
  [SMALL_STATE(150)] = 3955,
  [SMALL_STATE(151)] = 3963,
  [SMALL_STATE(152)] = 3971,
  [SMALL_STATE(153)] = 3979,
  [SMALL_STATE(154)] = 3987,
  [SMALL_STATE(155)] = 3995,
  [SMALL_STATE(156)] = 4003,
  [SMALL_STATE(157)] = 4009,
  [SMALL_STATE(158)] = 4017,
  [SMALL_STATE(159)] = 4025,
  [SMALL_STATE(160)] = 4035,
  [SMALL_STATE(161)] = 4043,
  [SMALL_STATE(162)] = 4049,
  [SMALL_STATE(163)] = 4057,
  [SMALL_STATE(164)] = 4065,
  [SMALL_STATE(165)] = 4073,
  [SMALL_STATE(166)] = 4079,
  [SMALL_STATE(167)] = 4085,
  [SMALL_STATE(168)] = 4091,
  [SMALL_STATE(169)] = 4097,
  [SMALL_STATE(170)] = 4105,
  [SMALL_STATE(171)] = 4111,
  [SMALL_STATE(172)] = 4117,
  [SMALL_STATE(173)] = 4125,
  [SMALL_STATE(174)] = 4131,
  [SMALL_STATE(175)] = 4137,
  [SMALL_STATE(176)] = 4145,
  [SMALL_STATE(177)] = 4153,
  [SMALL_STATE(178)] = 4159,
  [SMALL_STATE(179)] = 4165,
  [SMALL_STATE(180)] = 4173,
  [SMALL_STATE(181)] = 4179,
  [SMALL_STATE(182)] = 4185,
  [SMALL_STATE(183)] = 4191,
  [SMALL_STATE(184)] = 4199,
  [SMALL_STATE(185)] = 4207,
  [SMALL_STATE(186)] = 4215,
  [SMALL_STATE(187)] = 4222,
  [SMALL_STATE(188)] = 4229,
  [SMALL_STATE(189)] = 4236,
  [SMALL_STATE(190)] = 4243,
  [SMALL_STATE(191)] = 4250,
  [SMALL_STATE(192)] = 4257,
  [SMALL_STATE(193)] = 4264,
  [SMALL_STATE(194)] = 4269,
  [SMALL_STATE(195)] = 4276,
  [SMALL_STATE(196)] = 4280,
  [SMALL_STATE(197)] = 4284,
  [SMALL_STATE(198)] = 4288,
  [SMALL_STATE(199)] = 4292,
  [SMALL_STATE(200)] = 4296,
  [SMALL_STATE(201)] = 4300,
  [SMALL_STATE(202)] = 4304,
  [SMALL_STATE(203)] = 4308,
  [SMALL_STATE(204)] = 4312,
  [SMALL_STATE(205)] = 4316,
  [SMALL_STATE(206)] = 4320,
  [SMALL_STATE(207)] = 4324,
  [SMALL_STATE(208)] = 4328,
  [SMALL_STATE(209)] = 4332,
  [SMALL_STATE(210)] = 4336,
  [SMALL_STATE(211)] = 4340,
  [SMALL_STATE(212)] = 4344,
  [SMALL_STATE(213)] = 4348,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(159),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(190),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(72),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(126),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(127),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(113),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(194),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(148),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(131),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(149),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(112),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(56),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(196),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2),
  [43] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(186),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(5),
  [49] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(72),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(126),
  [55] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(74),
  [58] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(127),
  [61] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(113),
  [64] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(41),
  [67] = {.entry = {.count = 1, .reusable = false}}, SHIFT(192),
  [69] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [73] = {.entry = {.count = 1, .reusable = false}}, SHIFT(70),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(117),
  [77] = {.entry = {.count = 1, .reusable = true}}, SHIFT(104),
  [79] = {.entry = {.count = 1, .reusable = false}}, SHIFT(115),
  [81] = {.entry = {.count = 1, .reusable = false}}, SHIFT(85),
  [83] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(192),
  [86] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(7),
  [89] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(70),
  [92] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(117),
  [95] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(104),
  [98] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(115),
  [101] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 2), SHIFT_REPEAT(85),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2),
  [106] = {.entry = {.count = 1, .reusable = false}}, SHIFT(186),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(165),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(142),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(89),
  [116] = {.entry = {.count = 1, .reusable = false}}, SHIFT(189),
  [118] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [120] = {.entry = {.count = 1, .reusable = false}}, SHIFT(158),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(119),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(157),
  [126] = {.entry = {.count = 1, .reusable = false}}, SHIFT(116),
  [128] = {.entry = {.count = 1, .reusable = false}}, SHIFT(160),
  [130] = {.entry = {.count = 1, .reusable = false}}, SHIFT(188),
  [132] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [134] = {.entry = {.count = 1, .reusable = false}}, SHIFT(50),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(121),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [140] = {.entry = {.count = 1, .reusable = false}}, SHIFT(114),
  [142] = {.entry = {.count = 1, .reusable = false}}, SHIFT(48),
  [144] = {.entry = {.count = 1, .reusable = false}}, SHIFT(156),
  [146] = {.entry = {.count = 1, .reusable = false}}, SHIFT(65),
  [148] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_identifier, 1),
  [152] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_identifier, 1),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_identifier, 1),
  [156] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_identifier, 1),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_identifier, 1),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [162] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(191),
  [165] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [167] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(190),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int16_literal, 2, .production_id = 2),
  [172] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int16_literal, 2, .production_id = 2),
  [174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_paren_expression, 3, .production_id = 3),
  [176] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_paren_expression, 3, .production_id = 3),
  [178] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_uint64_literal, 2, .production_id = 2),
  [180] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_uint64_literal, 2, .production_id = 2),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 2, .production_id = 1),
  [184] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 2, .production_id = 1),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [188] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1),
  [190] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bool_literal, 1),
  [192] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_bool_literal, 1),
  [194] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_type_name, 1),
  [196] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_type_name, 1),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int32_literal, 2, .production_id = 2),
  [200] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int32_literal, 2, .production_id = 2),
  [202] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int64_literal, 2, .production_id = 2),
  [204] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int64_literal, 2, .production_id = 2),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int128_literal, 2, .production_id = 2),
  [208] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int128_literal, 2, .production_id = 2),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_uint32_literal, 2, .production_id = 2),
  [212] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_uint32_literal, 2, .production_id = 2),
  [214] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_identifier, 1),
  [216] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_identifier, 1),
  [218] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_int8_literal, 2, .production_id = 2),
  [220] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_int8_literal, 2, .production_id = 2),
  [222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [224] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_literal, 3, .production_id = 4),
  [226] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_uint128_literal, 2, .production_id = 2),
  [228] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_uint128_literal, 2, .production_id = 2),
  [230] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl, 7, .production_id = 10),
  [232] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_decl, 7, .production_id = 10),
  [234] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [236] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_type_name, 2),
  [238] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_type_name, 2),
  [240] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_uint8_literal, 2, .production_id = 2),
  [242] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_uint8_literal, 2, .production_id = 2),
  [244] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [246] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_infix_operation, 3, .production_id = 5),
  [248] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 4, .production_id = 6),
  [250] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_call, 4, .production_id = 6),
  [252] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_builtin_type, 1),
  [254] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_builtin_type, 1),
  [256] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_uint16_literal, 2, .production_id = 2),
  [258] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_uint16_literal, 2, .production_id = 2),
  [260] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_expression, 6, .production_id = 8),
  [262] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_expression, 6, .production_id = 8),
  [264] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_reference, 1),
  [266] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_reference, 1),
  [268] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [270] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 1),
  [272] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat2, 1),
  [274] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [276] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_decl, 4, .production_id = 7),
  [278] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_type_decl, 4, .production_id = 7),
  [280] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_fn_name, 1),
  [282] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_fn_name, 1),
  [284] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_qualified_fn_name, 2),
  [286] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_qualified_fn_name, 2),
  [288] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_digits, 1),
  [290] = {.entry = {.count = 1, .reusable = true}}, SHIFT(170),
  [292] = {.entry = {.count = 1, .reusable = true}}, SHIFT(171),
  [294] = {.entry = {.count = 1, .reusable = true}}, SHIFT(177),
  [296] = {.entry = {.count = 1, .reusable = true}}, SHIFT(182),
  [298] = {.entry = {.count = 1, .reusable = true}}, SHIFT(181),
  [300] = {.entry = {.count = 1, .reusable = true}}, SHIFT(105),
  [302] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [304] = {.entry = {.count = 1, .reusable = true}}, SHIFT(87),
  [306] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [308] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [310] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [312] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [314] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [316] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [318] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [320] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [322] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [326] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [328] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [330] = {.entry = {.count = 1, .reusable = true}}, SHIFT(154),
  [332] = {.entry = {.count = 1, .reusable = true}}, SHIFT(153),
  [334] = {.entry = {.count = 1, .reusable = true}}, SHIFT(152),
  [336] = {.entry = {.count = 1, .reusable = true}}, SHIFT(151),
  [338] = {.entry = {.count = 1, .reusable = true}}, SHIFT(150),
  [340] = {.entry = {.count = 1, .reusable = false}}, SHIFT(106),
  [342] = {.entry = {.count = 1, .reusable = true}}, SHIFT(135),
  [344] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [346] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [348] = {.entry = {.count = 1, .reusable = true}}, SHIFT(95),
  [350] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [352] = {.entry = {.count = 1, .reusable = true}}, SHIFT(93),
  [354] = {.entry = {.count = 1, .reusable = false}}, SHIFT(155),
  [356] = {.entry = {.count = 1, .reusable = true}}, SHIFT(147),
  [358] = {.entry = {.count = 1, .reusable = true}}, SHIFT(146),
  [360] = {.entry = {.count = 1, .reusable = true}}, SHIFT(143),
  [362] = {.entry = {.count = 1, .reusable = true}}, SHIFT(175),
  [364] = {.entry = {.count = 1, .reusable = true}}, SHIFT(138),
  [366] = {.entry = {.count = 1, .reusable = false}}, SHIFT(47),
  [368] = {.entry = {.count = 1, .reusable = true}}, SHIFT(110),
  [370] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [372] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [374] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [376] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [378] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [380] = {.entry = {.count = 1, .reusable = false}}, SHIFT(187),
  [382] = {.entry = {.count = 1, .reusable = true}}, SHIFT(130),
  [384] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [386] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [388] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [390] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [392] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [394] = {.entry = {.count = 1, .reusable = false}}, SHIFT(101),
  [396] = {.entry = {.count = 1, .reusable = true}}, SHIFT(180),
  [398] = {.entry = {.count = 1, .reusable = true}}, SHIFT(178),
  [400] = {.entry = {.count = 1, .reusable = true}}, SHIFT(137),
  [402] = {.entry = {.count = 1, .reusable = true}}, SHIFT(174),
  [404] = {.entry = {.count = 1, .reusable = true}}, SHIFT(173),
  [406] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2),
  [408] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2), SHIFT_REPEAT(187),
  [411] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_fn_decl_params_repeat1, 2), SHIFT_REPEAT(129),
  [414] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl_params, 1),
  [416] = {.entry = {.count = 1, .reusable = true}}, SHIFT(129),
  [418] = {.entry = {.count = 1, .reusable = false}}, SHIFT(161),
  [420] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_qualified_fn_name_repeat1, 2),
  [422] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_qualified_fn_name_repeat1, 2), SHIFT_REPEAT(196),
  [425] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [427] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_content_repeat1, 2),
  [429] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_content_repeat1, 2), SHIFT_REPEAT(134),
  [432] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string_content, 1),
  [434] = {.entry = {.count = 1, .reusable = true}}, SHIFT(134),
  [436] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [438] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [440] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_fn_decl_param, 5, .production_id = 9),
  [442] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_fn_decl_param, 5, .production_id = 9),
  [444] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [446] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [448] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [450] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [452] = {.entry = {.count = 1, .reusable = true}}, SHIFT(168),
  [454] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [456] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [458] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [460] = {.entry = {.count = 1, .reusable = true}}, SHIFT(139),
  [462] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [464] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [466] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [468] = {.entry = {.count = 1, .reusable = true}}, SHIFT(212),
  [470] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [472] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [474] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [476] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [478] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [480] = {.entry = {.count = 1, .reusable = true}}, SHIFT(145),
  [482] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [484] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [486] = {.entry = {.count = 1, .reusable = true}}, SHIFT(193),
  [488] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [490] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [492] = {.entry = {.count = 1, .reusable = true}}, SHIFT(140),
  [494] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [496] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [498] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [500] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [502] = {.entry = {.count = 1, .reusable = true}}, SHIFT(167),
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
