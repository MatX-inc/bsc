#include <cstring>
#include <ctype.h>

#include "bs_wide_data.h"
#include "bs_mem_defines.h"
#include "bs_mem_file.h"
#include "bs_target.h"

// Helper for reporting an error with position information:
//   "Error: <kind> at line <line> of file '<filename>'\n"
//   "       <detail>\n"
static void report_error(Target& dest, const char* kind,
                         unsigned int line, const char* filename,
                         const char* detail)
{
  dest.write_string("Error: ");
  dest.write_string(kind);
  dest.write_string(" at line ");
  dest.write_decimal(line);
  dest.write_string(" of file '");
  dest.write_string(filename);
  dest.write_string("'\n       ");
  dest.write_string(detail);
  dest.write_char('\n');
}

// Append a string to a fixed-size buffer (always NUL-terminated,
// truncating silently), returning the new length.  Used to build
// parse-error details by hand: the model imports no printf-family
// symbol from a C library.
static unsigned int append_str(char* buf, unsigned int size,
                               unsigned int pos, const char* s)
{
  while ((*s != '\0') && (pos + 1 < size))
    buf[pos++] = *s++;
  if (pos < size)
    buf[pos] = '\0';
  return pos;
}

// Format "Encountered '<c>' when expecting <expecting>." into buf
static void format_unexpected(char* buf, unsigned int size, char c,
                              const char* expecting)
{
  unsigned int pos = append_str(buf, size, 0, "Encountered '");
  if (pos + 1 < size)
  {
    buf[pos++] = c;
    buf[pos] = '\0';
  }
  pos = append_str(buf, size, pos, "' when expecting ");
  pos = append_str(buf, size, pos, expecting);
  append_str(buf, size, pos, ".");
}

// Parser states
typedef enum { START
             , BEGIN_COMMENT
             , IN_CPP_COMMENT
             , IN_C_COMMENT
             , END_C_COMMENT
             , IN_ADDR
             , IN_VALUE
             } tMemParserState;

// Wrapper for callback to handle address entries
const char* processAddress(const char* addr_str, FormatHandler* handler)
{
  tMemFileStatus status = handler->updateAddress(addr_str);
  if (status == MF_BAD_FORMAT)
    return "Malformed address";
  else if (status == MF_OUT_OF_BOUNDS)
    return "Address is outside of the allowed range";
  else
    return NULL;
}

// Wrapper for callback to handle value entries
const char* processData(const char* value_str, FormatHandler* handler)
{
  tMemFileStatus status = handler->setEntry(value_str);
  if (status == MF_BAD_FORMAT)
    return "Malformed value";
  else
    return NULL;
}

// Top-level routine to read a file
void read_mem_file(tSimStateHdl simHdl,
                   const char* filename,
                   const char* memname,
                   FormatHandler* handler)
{
  if (filename == NULL)
    return;

  const struct bs_host_ops* ops = bk_host_ops(simHdl);
  void* ctx = bk_host_ctx(simHdl);
  FileTarget dest(simHdl); // error messages go to the host's stdout

  bs_host_file* in = ops->open(ctx, filename, "r");
  if (!in)
  {
    dest.write_string("Error: failed to open file '");
    dest.write_string(filename);
    dest.write_string("' because ");
    dest.write_string(ops->last_error(ctx));
    dest.write_char('\n');
    return;
  }

  // Parse the file contents, passing addresses and data to the
  // handler as strings.  The current address or value token is
  // accumulated character by character into a fixed buffer (nothing
  // is allocated); BS_MEMFILE_TOKEN_MAX bounds one token -- an
  // address or one entry's value, underscores included -- and a
  // longer token is reported as an error.  The token buffer is
  // static because memory files can be preloaded during model
  // construction, whose stack use is kept small; the kernel is
  // single-threaded, so one buffer suffices.
  char buf[128];
  static char token[BS_MEMFILE_TOKEN_MAX];
  unsigned int token_len = 0;
  char err_buf[96];
  char* cptr;
  unsigned int comment_start_line = 0;
  unsigned int line = 1;
  unsigned int start_line = 1;
  tMemParserState state = START;
  tSInt64 nread;
  while ((nread = ops->read(ctx, in, buf, sizeof(buf) - 1)) > 0)
  {
    buf[nread] = '\0';
    cptr = buf;

    // parse the current buffer contents character-by-character
    while (*cptr != '\0')
    {
      char c = *cptr;

      // accumulate the current token (the state machine below only
      // stays in a token state on token characters)
      if ((state == IN_ADDR) || (state == IN_VALUE))
      {
        bool token_char = isxdigit(c) || (c == '_') ||
                          (c == 'x')  || (c == 'X') ||
                          (c == 'z')  || (c == 'Z');
        if (token_char)
        {
          if (token_len >= (sizeof(token) - 1))
          {
            report_error(dest, "token overflow", start_line, filename,
                         "Address or value token is too long.");
            ops->close(ctx, in);
            return;
          }
          token[token_len++] = c;
        }
      }
      switch (state)
      {
        case START:
        {
          if (c == '/')
          {
            state = BEGIN_COMMENT;
          }
          else if (c == '@')
          {
            state = IN_ADDR;
            token_len = 0;
            start_line = line;
          }
          else if (isxdigit(c))
          {
            state = IN_VALUE;
            token[0] = c;
            token_len = 1;
            start_line = line;
          }
          else if (c == '\n')
          {
            ++line;
            // stay in START state
          }
          else if ((c == '\r') || isblank(c))
          {
            // stay in START state
          }
          else
          {
            format_unexpected(err_buf, sizeof(err_buf), c,
                              "'/', '@', hex digit, end-of-line "
                              "or whitespace");
            report_error(dest, "syntax error", line, filename, err_buf);
            ops->close(ctx, in);
            return;
          }
          break;
        }

        case BEGIN_COMMENT:
        {
          if (c == '/')
          {
            state = IN_CPP_COMMENT;
          }
          else if (c == '*')
          {
            state = IN_C_COMMENT;
            comment_start_line = line;
          }
          else
          {
            report_error(dest, "syntax error", line, filename,
                         "Malformed comment start sequence.");
            ops->close(ctx, in);
            return;
          }
          break;
        }

        case IN_CPP_COMMENT:
        {
          if (c == '\n')
          {
            ++line;
            state = START;
          }
          else
          {
            // stay in IN_CPP_COMMENT state
          }
          break;
        }

        case IN_C_COMMENT:
        {
          if (c == '\n')
          {
            // stay in IN_C_COMMENT state
            ++line;
          }
          else if (c == '*')
          {
            state = END_C_COMMENT;
          }
          else
          {
            // stay in IN_C_COMMENT state
          }
          break;
        }

        case END_C_COMMENT:
        {
          if (c == '/')
          {
            state = START;
          }
          else
          {
            state = IN_C_COMMENT;
          }
          break;
        }

        case IN_ADDR:
        {
          const char* err = NULL;
          if ((c == '\n') || (c == '\r') || isblank(c))
          {
            token[token_len] = '\0';
            err = processAddress(token, handler);
            if (c == '\n') ++line;
            state = START;
          }
          else if (c == '/')
          {
            token[token_len] = '\0';
            err = processAddress(token, handler);
            state = BEGIN_COMMENT;

          }
          else if (isxdigit(c) || (c == '_') ||
                   (c == 'x')  || (c == 'X') ||
                   (c == 'z')  || (c == 'Z'))
          {
            // stay in IN_ADDR state
          }
          else
          {
            format_unexpected(err_buf, sizeof(err_buf), c,
                              "'/', hex digit, end-of-line or whitespace");
            err = err_buf;
          }

          if (err)
          {
            report_error(dest, "address processing error", start_line,
                         filename, err);
            ops->close(ctx, in);
            return;
          }

          break;
        }

        case IN_VALUE:
        {
          const char* err = NULL;
          if ((c == '\n') || (c == '\r') || isblank(c))
          {
            token[token_len] = '\0';
            err = processData(token, handler);
            if (c == '\n') ++line;
            state = START;
          }
          else if (c == '/')
          {
            token[token_len] = '\0';
            err = processData(token, handler);
            state = BEGIN_COMMENT;
          }
          else if (isxdigit(c) || (c == '_') ||
                   (c == 'x')  || (c == 'X') ||
                   (c == 'z')  || (c == 'Z'))
          {
            // stay in IN_VALUE state
          }
          else
          {
            format_unexpected(err_buf, sizeof(err_buf), c,
                              "'/', digit, end-of-line or whitespace");
            err = err_buf;
          }

          if (err)
          {
            report_error(dest, "value processing error", start_line,
                         filename, err);
            ops->close(ctx, in);
            return;
          }

          break;
        }
      }
      ++cptr;
    }
  }

  if (state == IN_C_COMMENT || state == END_C_COMMENT)
  {
    report_error(dest, "syntax error", comment_start_line, filename,
                 "Unterminated C-style comment.");
  }
  else if (state == IN_VALUE)
  {
    token[token_len] = '\0';
    const char* err = processData(token, handler);
    if (err)
    {
      report_error(dest, "value processing error", line, filename, err);
    }
  }

  handler->checkRange(simHdl, filename, memname);

  ops->close(ctx, in);
}

// Utility functions for use in writing FormatHandlers

static unsigned int fromHex(char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'a' && c <= 'f')
    return 10 + (c - 'a');
  else if (c >= 'A' && c <= 'F')
    return 10 + (c - 'A');
  else
    return 0;
}

bool parse_bin(tUInt8* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt8 x = 0;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (c == '0' || c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 1;
      ++bits;
    }
    else if (c == '1')
    {
      x = (x << 1) + 1;
      ++bits;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    if (bits > data_bits)
      return false;
  }

  *value = x;
  return true;
}

bool parse_hex(tUInt8* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt8 x = 0;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (isxdigit(c))
    {
      x = (x << 4) + fromHex(c);
      bits += 4;
    }
    else if (c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 4;
      bits += 4;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    // only an error if we extend beyond the final nibble or
    // there is data in high bits which don't exist in the last
    // nibble.
    if ((bits/4 > (data_bits+3)/4) || ((bits/4 == (data_bits+3)/4) &&
                                       ((data_bits % 4) != 0) &&
                                       ((x >> data_bits) != 0)))
      return false;
  }

  *value = x;
  return true;
}

bool parse_bin(tUInt32* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt32 x = 0;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (c == '0' || c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 1;
      ++bits;
    }
    else if (c == '1')
    {
      x = (x << 1) + 1;
      ++bits;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    if (bits > data_bits)
      return false;
  }

  *value = x;
  return true;
}

bool parse_hex(tUInt32* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt32 x = 0;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (isxdigit(c))
    {
      x = (x << 4) + fromHex(c);
      bits += 4;
    }
    else if (c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 4;
      bits += 4;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    // only an error if we extend beyond the final nibble or
    // there is data in high bits which don't exist in the last
    // nibble.
    if ((bits/4 > (data_bits+3)/4) || ((bits/4 == (data_bits+3)/4) &&
                                       ((data_bits % 4) != 0) &&
                                       ((x >> data_bits) != 0)))
      return false;
  }

  *value = x;
  return true;
}

bool parse_bin(tUInt64* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt64 x = 0llu;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (c == '0' || c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 1;
      ++bits;
    }
    else if (c == '1')
    {
      x = (x << 1) + 1;
      ++bits;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    if (bits > data_bits)
      return false;
  }

  *value = x;
  return true;
}

bool parse_hex(tUInt64* value, const char* str, unsigned int data_bits)
{
  char c;
  unsigned int bits = 0;
  tUInt64 x = 0llu;
  while ((c = *(str++)) != '\0')
  {
    if (c == '_')
    {
      // ignore separator
    }
    else if (isxdigit(c))
    {
      x = (x << 4) + fromHex(c);
      bits += 4;
    }
    else if (c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      x = x << 4;
      bits += 4;
    }
    else if (c == '/' || c == '\n' || c == '\r' || isblank(c))
    {
      break;
    }
    else
      return false;

    // only an error if we extend beyond the final nibble or
    // there is data in high bits which don't exist in the last
    // nibble.
    if ((bits/4 > (data_bits+3)/4) || ((bits/4 == (data_bits+3)/4) &&
                                       ((data_bits % 4) != 0) &&
                                       ((x >> data_bits) != 0llu)))
      return false;
  }

  *value = x;
  return true;
}

bool parse_bin(tUWide* value, const char* str, unsigned int data_bits)
{
  // find the end of the string
  const char* cptr = str;
  while (*cptr != '\0' && *cptr != '\n' && *cptr != '\r' &&
         *cptr != '/'  && !isblank(*cptr))
    ++cptr;

  // parse characters from LSB back
  char c;
  unsigned int word = 0;
  unsigned int idx = 0;
  unsigned int x = 0;
  unsigned int bits = 0;
  while (cptr != str)
  {
    c = *(--cptr);
    if (c == '_')
    {
      // ignore separator
    }
    else if (c == '0' || c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      ++bits;
      ++idx;
    }
    else if (c == '1')
    {
      x |= (1 << idx);
      ++bits;
      ++idx;
    }
    else
      return false;

    if (bits > data_bits)
      return false;

    if (idx == WORD_SIZE)
    {
      (*value)[word++] = x;
      idx = 0;
    }
  }

  // write partial word at end
  if (idx != 0)
    (*value)[word] = x;

  return true;
}

bool parse_hex(tUWide* value, const char* str, unsigned int data_bits)
{
  // find the end of the string
  const char* cptr = str;
  while (*cptr != '\0' && *cptr != '\n' && *cptr != '\r' &&
         *cptr != '/'  && !isblank(*cptr))
    ++cptr;

  // parse characters from LSB back
  char c;
  unsigned int word = 0;
  unsigned int idx = 0;
  unsigned int x = 0;
  unsigned int bits = 0;
  while (cptr != str)
  {
    c = *(--cptr);
    if (c == '_')
    {
      // ignore separator
    }
    else if (isxdigit(c))
    {
      x |= (fromHex(c) << idx);
      bits += 4;
      idx += 4;
    }
    else if (c == 'x' || c == 'X' || c == 'z' || c == 'Z')
    {
      bits += 4;
      idx += 4;
    }
    else
      return false;

    // only an error if we extend beyond the final nibble or
    // there is data in high bits which don't exist in the last
    // nibble.
    if ((bits/4 > (data_bits+3)/4) || ((bits/4 == (data_bits+3)/4) &&
                                       ((data_bits % 4) != 0) &&
                                       ((x >> (data_bits % WORD_SIZE)) != 0)))
      return false;

    if (idx == WORD_SIZE)
    {
      (*value)[word++] = x;
      x = 0;
      idx = 0;
    }
  }

  // write partial word at end
  if (idx != 0)
    (*value)[word] = x;

  return true;
}
