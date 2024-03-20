// the scanner should be used __very__ sparingly, and any changes should be discussed before embarking on anything new here.
#include <tree_sitter/parser.h>
#include <wctype.h>

enum TokenType
{
  DOC_COMMENT,
};

void *tree_sitter_darklang_external_scanner_create()
{
  return NULL;
}

bool tree_sitter_darklang_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_symbols)
{
  while (iswspace(lexer->lookahead))
    // Skip any whitespace
    lexer->advance(lexer, true);
  // Consume the three slashes
  if (lexer->lookahead == '/')
  {
    // false means don't skip the token
    lexer->advance(lexer, false);

    if (lexer->lookahead == '/')
    {
      lexer->advance(lexer, false);

      if (lexer->lookahead == '/')
      {
        lexer->advance(lexer, false);

        // Consume the rest of the line, until a newline or EOF
        while (lexer->lookahead != '\n' && lexer->lookahead != 0)
        {
          lexer->advance(lexer, false);
        }
        // This is like tagging a piece of text with a sticker that says "This is a doc comment"
        lexer->result_symbol = DOC_COMMENT;
        return true;
      }
    }
  }

  return false; // No valid token was identified
}

unsigned tree_sitter_darklang_external_scanner_serialize(void *payload, char *buffer)
{
  return 0;
}

void tree_sitter_darklang_external_scanner_deserialize(void *payload, const char *buffer, unsigned length)
{
  // no state to deserialize
}

void tree_sitter_darklang_external_scanner_destroy(void *payload)
{
  // nothing to destroy in this case
}
