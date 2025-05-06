#include <tree_sitter/parser.h>
#include <wctype.h>
#include <string.h>

// maximum size for the indentation stack
#define MAX_STACK_SIZE 32

// This should match the order defined in the grammar
enum TokenType {
  INDENT,
  DEDENT,
  FUNCTION_BOUNDARY  // to mark the end of a function call
};

struct Scanner {
  int indent_stack[MAX_STACK_SIZE]; // Stack to track indentation levels
  unsigned int stack_size;          // the current size of the stack (number of elements)
  bool in_dedent;                   // Flag to indicate processing a dedent sequence
};

void *tree_sitter_darklang_external_scanner_create() {
  struct Scanner *scanner = calloc(1, sizeof(struct Scanner));
  scanner->stack_size = 0;  // Start with no indentation levels
  scanner->indent_stack[0] = 0;  // Base indent level is 0
  scanner->in_dedent = false;
  return scanner;
}

// Function to push a new indentation level onto the stack.
void push(struct Scanner *scanner, int indent_length) {
  if (scanner->stack_size < MAX_STACK_SIZE) {
    scanner->indent_stack[++scanner->stack_size] = indent_length; // increment stack size and add the new indentation level
  }
}

// Function to pop the last indentation level from the stack
void pop(struct Scanner *scanner) {
  if (scanner->stack_size > 0) {
    scanner->stack_size--;
  }
}

bool tree_sitter_darklang_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  struct Scanner *scanner = payload;
  bool found_newline = false;

  // Handle function boundary detection
  if (valid_symbols[FUNCTION_BOUNDARY]) {
    // If we're at a newline and FUNCTION_BOUNDARY is a valid symbol, emit it
    if (lexer->lookahead == '\n') {
      lexer->result_symbol = FUNCTION_BOUNDARY;
      return true;
    }
    // If we're at the end of a file and FUNCTION_BOUNDARY is valid, emit it
    if (lexer->eof(lexer)) {
      lexer->result_symbol = FUNCTION_BOUNDARY;
      return true;
    }
  }

  // Check if the scanner is currently handling a block dedentation
  if (scanner->in_dedent) {
    // If there are no more indentations to process or the current column is beyond the last indent level, exit dedent mode
    if (scanner->stack_size == 0 || lexer->get_column(lexer) >= (unsigned int)scanner->indent_stack[scanner->stack_size]) {
      scanner->in_dedent = false;
    } else {
      pop(scanner); // Remove the last indentation level
      lexer->result_symbol = DEDENT;
      return true;
    }
  }

  // New line or end of file handling
  while (lexer->lookahead == '\n') {
    found_newline = true;
    lexer->advance(lexer, true);
    // Skip any following newline characters
    while (lexer->lookahead == '\n') {
      lexer->advance(lexer, true);
    }
  }

  if (found_newline) {
    unsigned int new_indent_length = 0;
    // Count the number of spaces for the new line to determine the new indentation level.
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
      lexer->advance(lexer, true);
      new_indent_length++; // Update the current indentation level
    }

    // Emit DEDENT tokens if the new indentation level is less than the previous levels
    while (scanner->stack_size > 0 && new_indent_length < (unsigned int)scanner->indent_stack[scanner->stack_size]) {
      pop(scanner);
      lexer->result_symbol = DEDENT;
      scanner->in_dedent = true;
      return true;
    }

    // If the new indentation is greater and there was a preceding newline, push this new level onto the stack and emit an INDENT token
    if (new_indent_length > (unsigned int)scanner->indent_stack[scanner->stack_size]) {
      push(scanner, new_indent_length);
      lexer->result_symbol = INDENT;
      return true;
    }

    // Reset the dedentation flag if the indentation level matches but no more DEDENT tokens need to be emitted
    if (scanner->stack_size > 0 && new_indent_length == (unsigned int)scanner->indent_stack[scanner->stack_size]) {
      scanner->in_dedent = false;
      return false;
    }
  }

  // Handle end of file dedentation
  if (lexer->eof(lexer)) {
    while (scanner->stack_size > 0 && valid_symbols[DEDENT]) {
      pop(scanner);
      lexer->result_symbol = DEDENT;
      return true;
    }
  }

  return false; // No tokens to emit
}

unsigned tree_sitter_darklang_external_scanner_serialize(void *payload, char *buffer) {
  struct Scanner *scanner = payload;

  // Calculate the total size to serialize
  unsigned total_size = scanner->stack_size * sizeof(int); // only indent_stack

  if (total_size <= TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
    // Copy indent stack
    memcpy(buffer, scanner->indent_stack, total_size);
    return total_size;
  }

  return 0;
}

void tree_sitter_darklang_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  struct Scanner *scanner = payload;

  // Reset the scanner if deserializing empty data
  if (length == 0) {
    scanner->stack_size = 0;
    scanner->indent_stack[0] = 0;
    scanner->in_dedent = false;
    return;
  }

  unsigned num_ints = length / sizeof(int);

  if (num_ints <= MAX_STACK_SIZE) {
    // Restore indent stack
    memcpy(scanner->indent_stack, buffer, length);
    scanner->stack_size = num_ints;
  }
}

void tree_sitter_darklang_external_scanner_destroy(void *payload) {
  // Free the memory allocated for the scanner
  free(payload);
}
