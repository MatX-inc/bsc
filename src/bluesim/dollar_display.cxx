#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <algorithm>

#include "bluesim_kernel_api.h"
#include "bs_str.h"
#include "bs_wide_data.h"
#include "bs_module.h"
#include "bs_target.h"
#include "mem_alloc.h"
#include "portability.h"

/* Formatting in this file works without heap allocation: values are
 * read straight out of the argument list, string arguments are viewed
 * in place or staged in stack arrays sized by their own descriptors,
 * and the output is produced by the hand-rolled printers.  The only
 * allocator use left in this file is the $fopen file table (VLFiles
 * below), which grows through the Bluesim allocator the first time a
 * file-based system task runs.
 */

// This structure is used to record information
// parsed from a format field specifier.
typedef struct
{
  char mode;         // mode character
  int width;         // width specifier (optional)
  int precision;     // precision (optional)
  const char* str;   // pointer to field spec in string (after '%')
  const char* after; // pointer to remainder of string (after mode char)
} tFieldDesc;

// This structure is used to unify different value representations.
// It never owns the wide data it points at: wideVal is either the
// caller's own WideData argument or a view over a stack array in the
// printing routine's frame.
typedef struct
{
  bool         isSigned;
  bool         usingWideVal;
  unsigned int bits;
  union
  {
    bool               bitVal;
    signed long long   sVal;
    unsigned long long uVal;
    WideData*          wideVal;
  } data;
} tValue;

// cast tValue to double as best we can
double tValueToDouble(tValue& v) {
  if(v.usingWideVal) {
    return 0; // too lazy to do conversion right now
  }
  else if(v.bits == 1) {
    return(v.data.bitVal ? 1.0 : 0.0);
  }
  else if(v.isSigned) {
    return ((double)v.data.sVal);
  } else
    return ((double)v.data.uVal);
}

// utility functions for dealing with different digit classes
static bool isOctal(char c) { return ( c >= '0' && c <= '7'); }
static bool isDigit(char c) { return ( c >= '0' && c <= '9'); }
static int fromDigit(char c) { return (c - '0'); }

// Return the number of digits required by the widest N-bit value,
// represented in the specified base.
static unsigned int maxWidth(unsigned int nBits, bool isSigned,
                             unsigned int base=10)
{
  // handle power-of-2 bases
  switch (base)
  {
    case 2:  return nBits;
    case 8:  return (nBits + 2) / 3;
    case 16: return (nBits + 3) / 4;
  }

  // handle base 10;
  unsigned int digits = 0;
  unsigned int sign_digit = isSigned ? 1 : 0;

  if (nBits > 64)
    digits = WideData::max_decimal_digits(nBits, isSigned);
  else
  {
    signed int factor = 3;
    if (nBits > 12)
      factor = 2 - ((nBits - 3) / 10);
    else if (nBits > 0)
      factor = 2;
    digits = (nBits + factor) / 3;
  }

  return sign_digit + digits;
}

// Return the number of digits required to represent a given value
// in the specified base.
static unsigned int numDigits(const tValue& v, unsigned int base=10)
{
  if (v.bits < 2)
    return 1;

  if (v.bits > 64)
  {
    // this is expensive for wide data, so we use a different
    // technique for wide data + base 10
    return 0;
  }

  unsigned int digits = 0;
  unsigned long long x = 0L;

  if (v.isSigned)
  {
    if (base == 10)
    {
      digits = (v.data.sVal < 0ll) ? 1 : 0;  // leading minus sign
      x = llabs(v.data.sVal);
    }
    else
    {
      x = v.data.uVal;
      if (v.bits < 64)
        x &= (1llu << v.bits) - 1;
    }
  }
  else
  {
    x = v.data.uVal;
  }

  if (x == 0)
    digits += 1;

  while (x != 0)
  {
    ++digits;
    x /= base;
  }

  return digits;
}

void pad(signed int requested_width,
         unsigned int max_width,
         unsigned int value_width,
         char c,
         Target* dest)
{
  if (requested_width == 0)
    return; // %0* requests no padding

  // Pad with spaces if the requested width is greater than max_width.
  signed int places = requested_width - ((signed int) max_width);
  if (places > 0)
    dest->write_char(' ',places);

  // Pad with the given character if the value_width is less than max_width
  // and requested_width
  if (requested_width > 0)
    places = std::min(requested_width, (signed int) max_width);
  else
    places = (signed int) max_width;
  places -= (signed int) value_width;
  if (places > 0)
    dest->write_char(c,places);
}

// This class implements a handler for variadic functions
// which correlates the size string information with the
// va_list and safely manages the va_list when passed through
// multiple functions.
class ArgList
{
 private:
  // The size string is a literal in the generated code and outlives
  // the call, so it is walked in place (no copy is made).
  const char*  cptr;
  bool         done;
  va_list*     ap_ptr;
  bool         has_sign;
  bool         is_pointer;
  bool         is_string;
  bool         is_str_tree;
  bool         is_double;
  unsigned int size;

 public:
  ArgList(const char* str, va_list* ap)
    : cptr(str), done(false), ap_ptr(ap)
  {
    next();
  }

 private:
  void next();

 public:
  bool isDone() const           { return done; }
  unsigned int argSize() const  { return size; }
  bool isSigned() const         { return has_sign; }
  bool isPointer() const        { return is_pointer; }
  bool isString() const         { return is_string; }
  bool isStringTree() const     { return is_str_tree; }
  bool isDouble() const         { return is_double; }

  bool getBit();
  unsigned char getUChar();
  signed char getSChar();
  unsigned int getUInt();
  signed int getSInt();
  unsigned long long getULongLong();
  signed long long getSLongLong();
  double getDouble();
  char* getString();
  const tStr* getStringTree();
  void* getPointer();

  void skip() { next(); }
};

void ArgList::next()
{
  if (done)
    return;

  if (*cptr == '\0')
  {
    done = true;
    return;
  }

  // process sign information
  if (*cptr == '-')
  {
    has_sign = true;
    ++cptr;
  } else {
    has_sign = false;
  }

  // process next size value from string
  if (*cptr == '\0')
  {
    done = true;
    return;
  }

  size = 0;
  bool has_size = false;
  while ((*cptr != '\0') && isDigit(*cptr))
  {
    size = size * 10 + fromDigit(*cptr);
    has_size = true;
    ++cptr;
  }

  // process string/pointer/real annotation
  switch (*cptr)
  {
    case 's':
    {
      is_string     = has_size;
      is_str_tree   = !has_size;
      is_pointer    = false;
      is_double     = false;
      ++cptr;
      break;
    }
    case 'p':
    {
      is_pointer    = true;
      is_string     = false;
      is_str_tree   = false;
      is_double     = false;
      ++cptr;
      break;
    case 'r':
      is_double     = true;
      is_pointer    = false;
      is_string     = false;
      is_str_tree   = false;
      // we will convert to a signed long long
      // if a real number is not expected
      has_sign      = true;
      size          = 64;
      break;
    }
    default:
    {
      is_pointer    = false;
      is_string     = false;
      is_str_tree   = false;
      is_double     = false;
    }
  }

  // advance up to next comma
  while ((*cptr != '\0') && (*cptr != ',')) ++cptr;
  if (*cptr == ',') ++cptr;
}

bool ArgList::getBit()
{
  // variable argument lists promote bool to int
  int ret = va_arg(*ap_ptr,int);
  next();
  return (ret != 0);
}

unsigned char ArgList::getUChar()
{
  // variable argument lists promote char to int
  int ret = va_arg(*ap_ptr,int);
  next();
  return (unsigned char) ret;
}

signed char ArgList::getSChar()
{
  // variable argument lists promote char to int
  int ret = va_arg(*ap_ptr,int);
  if ((size < 8) && (ret & (1 << (size - 1))))
    ret |= ~((1 << size) - 1);
  next();
  return (signed char) ret;
}

unsigned int ArgList::getUInt()
{
  unsigned int ret = va_arg(*ap_ptr,unsigned int);
  next();
  return ret;
}

signed int ArgList::getSInt()
{
  signed int ret = va_arg(*ap_ptr,signed int);
  if ((size < 32) && (ret & (1 << (size - 1))))
    ret |= ~((1 << size) - 1);
  next();
  return ret;
}

unsigned long long ArgList::getULongLong()
{
  unsigned long long ret = va_arg(*ap_ptr,unsigned long long);
  next();
  return ret;
}

signed long long ArgList::getSLongLong()
{
  signed long long ret = va_arg(*ap_ptr,signed long long);
  if ((size < 64) && (ret & (1llu << (size - 1))))
    ret |= ~((1llu << size) - 1);
  next();
  return ret;
}

double ArgList::getDouble()
{
  double ret = va_arg(*ap_ptr,double);
  next();
  return ret;
}

char* ArgList::getString()
{
  char* ret = va_arg(*ap_ptr,char*);
  next();
  return ret;
}

const tStr* ArgList::getStringTree()
{
  const tStr* ret = va_arg(*ap_ptr,const tStr*);
  next();
  return ret;
}

void* ArgList::getPointer()
{
  void* ret = va_arg(*ap_ptr,void*);
  next();
  return ret;
}

// Fill a tValue from a non-string argument.  String arguments need
// caller-provided storage; the printing routines consume them with
// FILL_TVALUE_KEEPING_STRINGS below, which stages the string's bytes
// in a stack array in the caller's own frame.
void fill_tValue(tValue& v, ArgList* args, Target* dest)
{
  v.isSigned = args->isSigned();
  v.usingWideVal = args->isPointer();
  v.bits = args->argSize();
  if (args->isPointer())
    v.data.wideVal = (WideData*) args->getPointer();
  else if (args->isDouble()) {
    dest->add_error("unexpected real number argument\n");
    v.data.sVal = (signed long long) args->getDouble();
  }
  else
  {
    if (v.bits == 1)
      v.data.bitVal = args->getBit();
    else if (v.isSigned)
    {
      if (v.bits <= 8)
        v.data.sVal = args->getSChar();
      else if (v.bits <= 32)
        v.data.sVal = args->getSInt();
      else
        v.data.sVal = args->getSLongLong();
    }
    else
    {
      if (v.bits <= 8)
        v.data.uVal = args->getUChar();
      else if (v.bits <= 32)
        v.data.uVal = args->getUInt();
      else
        v.data.uVal = args->getULongLong();
    }
  }
}

// Consume the next argument if it is a string (in either
// representation: a plain character array with a sized descriptor,
// stored in *chars, or a string tree with an unsized descriptor --
// see bs_str.h -- stored in *tree), recording its bit count in
// *bits.  Returns true when a string was consumed; returns false,
// with the argument left unconsumed, when the next argument is not
// a string.
static bool take_string_arg(ArgList* args,
                            const char** chars, const tStr** tree,
                            unsigned int* bits)
{
  *chars = NULL;
  *tree = NULL;
  *bits = 0;
  if (args->isDone())
    return false;
  if (args->isString())
  {
    *bits = args->argSize();
    *chars = args->getString();
    return true;
  }
  if (args->isStringTree())
  {
    *tree = args->getStringTree();
    if (*tree == NULL)
      *chars = "";  // an absent def prints as an empty string
    else
      *bits = 8u * bs_str_len(*tree);
    return true;
  }
  return false;
}

// Fill a non-owning WideData view (whose storage the caller
// provides) with the bytes of a string, most significant byte first
// -- the layout the old heap-owning WideData string constructor
// produced.  The string arrives either as characters or as a string
// tree, whose leaves are walked in place.
static void fill_wide_from_string(WideData& w, const char* str,
                                  const tStr* tree)
{
  if (w.size() == 0)
    return;
  w.clear();
  if (tree != NULL)
  {
    unsigned int nbytes = w.size() / 8;
    tUInt32 pos = 0u;
    while (pos < tree->len)
    {
      tUInt32 off;
      const tStr* leaf = bs_str_leaf_at(tree, pos, &off);
      for (tUInt32 i = 0u; i < leaf->len; ++i)
        w.setByte(nbytes - 1u - (off + i), (unsigned char) leaf->data[i]);
      pos = off + leaf->len;
    }
  }
  else
  {
    const char* cp = str;
    for (unsigned int i = w.size(); i > 0; i -= 8)
      w.setByte((i - 1) / 8, (unsigned char) *(cp++));
  }
}

/* Fill tValue 'v' from the next argument.  A string argument's bytes
 * are staged in a stack array declared here, in the expanding
 * routine's own frame, so the view in 'v' lives exactly as long as
 * the routine that prints it -- this replaces the old heap-owning
 * WideData that string arguments used to allocate.  The array is a
 * VLA sized by the argument's own descriptor; every routine using
 * this macro is charged a VLA allowance by the stack-depth analysis
 * (DYNAMIC_VLA_FUNCTIONS in bluesim_stack_bound.py).
 */
#define FILL_TVALUE_KEEPING_STRINGS(v, args, dest, tag)                 \
  unsigned int tag##_bits = 0;                                          \
  bool tag##_signed = (args)->isSigned();                               \
  const char* tag##_chars = NULL;                                       \
  const tStr* tag##_tree = NULL;                                        \
  bool tag##_is_str =                                                   \
    take_string_arg((args), &tag##_chars, &tag##_tree, &tag##_bits);    \
  unsigned int tag##_store[NUM_WORDS(tag##_bits) + 1];                  \
  WideData tag##_view(tag##_store, tag##_bits);                         \
  if (tag##_is_str)                                                     \
  {                                                                     \
    fill_wide_from_string(tag##_view, tag##_chars, tag##_tree);         \
    (v).isSigned = tag##_signed;                                        \
    (v).usingWideVal = true;                                            \
    (v).bits = tag##_bits;                                              \
    (v).data.wideVal = &tag##_view;                                     \
  }                                                                     \
  else                                                                  \
    fill_tValue((v), (args), (dest))

// Write the characters of a numeric argument into 'buf': the
// argument's bytes starting at the most significant one, with
// leading zero bytes skipped, NUL-terminated.  The caller provides
// at least (argSize() + 7) / 8 + 1 bytes.  This replaces the old
// heap-allocating conversion to a std::string.
static const char* convert_to_chars(ArgList* args, Target* dest, char* buf)
{
  char* p = buf;
  if (!args->isDone() && !args->isString() && !args->isStringTree())
  {
    tValue v;
    fill_tValue(v, args, dest);
    if (v.usingWideVal)
    {
      unsigned int i = (v.data.wideVal->size() + 7) / 8;
      while ((i > 0) && (v.data.wideVal->getByte(i-1) == 0)) --i;
      while (i-- > 0)
        *(p++) = (char) v.data.wideVal->getByte(i);
    }
    else if (v.bits == 1)
    {
      if (v.data.bitVal == 1) *(p++) = 1;
    }
    else
    {
      unsigned int i = (v.bits + 7) / 8;
      while ((i > 0) && (((v.data.uVal >> (8*(i-1))) & 0xFF) == 0)) --i;
      while (i-- > 0)
        *(p++) = (char) ((v.data.uVal >> (8*i)) & 0xFF);
    }
  }
  *p = '\0';
  return buf;
}

// This function is used to handle escape sequences beginning with '\'
const char* handle_escape(const char* cptr, Target* dest)
{
  switch (*cptr)
  {
    case '\0':  // backslash as last character -- no escape
    {
      dest->write_char('\\');
      break;
    }
    case 'n':   // emit newline
    {
      dest->write_char('\n');
      ++cptr;
      break;
    }
    case 't':   // emit tab
    {
      dest->write_char('\t');
      ++cptr;
      break;
    }
    case '\\':  // emit single backslash
    {
      dest->write_char('\\');
      ++cptr;
      break;
    }
    default:
    {
      // parse up to three octal digits and emit corresponding character
      int oct_val = 0;
      if (isOctal(*cptr))
      {
        oct_val = oct_val * 8 + fromDigit(*cptr);
        ++cptr;
        if (isOctal(*cptr))
        {
          oct_val = oct_val * 8 + fromDigit(*cptr);
          ++cptr;
          if (isOctal(*cptr))
          {
            oct_val = oct_val * 8 + fromDigit(*cptr);
            ++cptr;
          }
        }
        dest->write_char(oct_val);
      }
      else
      {
        // non-octal => just print character
        dest->write_char(*cptr);
        ++cptr;
      }
      break;
    }
  }

  return cptr;
}

// Printing routine for %d and %D formats
const char* print_decimal(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  tValue v;
  FILL_TVALUE_KEEPING_STRINGS(v, args, dest, dec);

  /* There are 2 cases:
   *  %d  => width determined by type size, left-padded with spaces
   *  %nd => minimum width is n, left-padded with spaces
   */

  unsigned int field_width = maxWidth(v.bits, v.isSigned);
  if (v.usingWideVal)
  {
    pad(spec.width, field_width, field_width, ' ', dest);
    if (spec.width == 0)
      v.data.wideVal->print_decimal(dest, 0, v.isSigned);
    else
      v.data.wideVal->print_decimal(dest, field_width, v.isSigned);
  }
  else if (v.bits == 1)
  {
    unsigned int value_width = numDigits(v);
    pad(spec.width, field_width, value_width, ' ', dest);
    if (v.isSigned && v.data.bitVal)
      dest->write_char('-');
    dest->write_char(v.data.bitVal ? '1' : '0');
  }
  else
  {
    unsigned int value_width = numDigits(v);
    pad(spec.width, field_width, value_width, ' ', dest);
    unsigned long long x;
    if (v.isSigned)
    {
      x = llabs(v.data.sVal);
      if (v.data.sVal < 0)
      {
        dest->write_char('-');
        --value_width;
      }
    }
    else
      x = v.data.uVal;
    char c;
    for (unsigned long long i = powll(10, (value_width - 1)); i > 0; i /= 10)
    {
      unsigned long long n = x / i;
      c = '0' + (char) n;
      dest->write_char(c);
      x -= n * i;
    }
  }

  return spec.after;
}

// Printing routine for %h and %H formats
const char* print_hex(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  tValue v;
  FILL_TVALUE_KEEPING_STRINGS(v, args, dest, hex);

  /* There are 2 cases:
   *  %h  => width determined by type size, all bits shown
   *  %nh => minimum width is n, left-padded with zeros
   */
  unsigned int field_width = maxWidth(v.bits, v.isSigned, 16);
  if (v.usingWideVal)
  {
    pad(spec.width, field_width, field_width, '0', dest);
    if (spec.width == 0)
      v.data.wideVal->print_hex(dest, 0);
    else
      v.data.wideVal->print_hex(dest, field_width);
  }
  else if (v.bits == 1)
  {
    unsigned int value_width = numDigits(v,16);
    pad(spec.width, field_width, value_width, '0', dest);
    dest->write_char(v.data.bitVal ? '1' : '0');
  }
  else
  {
    unsigned int value_width = numDigits(v,16);
    pad(spec.width, field_width, value_width, '0', dest);
    unsigned long long x = v.data.uVal;
    if (v.bits < 64)
      x &= (1llu << v.bits) - 1;
    char c;
    for (int i = (value_width - 1)*4; i >= 0; i -= 4)
    {
      if (((x >> i) & 0xF) > 9)
        c = 'a' + (char) (((x >> i) & 0xF) - 10);
      else
        c = '0' + (char) ((x >> i) & 0xF);
      dest->write_char(c);
    }
  }

  return spec.after;
}

// Printing routine for %o and %O formats
const char* print_octal(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  tValue v;
  FILL_TVALUE_KEEPING_STRINGS(v, args, dest, oct);

  /* There are 2 cases:
   *  %o  => width determined by type size, all bits shown
   *  %no => minimum width is n, left-padded with zeros
   */
  unsigned int field_width = maxWidth(v.bits, v.isSigned, 8);
  if (v.usingWideVal)
  {
    pad(spec.width, field_width, field_width, '0', dest);
    if (spec.width == 0)
      v.data.wideVal->print_octal(dest, 0);
    else
      v.data.wideVal->print_octal(dest, field_width);
  }
  else if (v.bits == 1)
  {
    unsigned int value_width = numDigits(v,8);
    pad(spec.width, field_width, value_width, '0', dest);
    dest->write_char(v.data.bitVal ? '1' : '0');
  }
  else
  {
    unsigned int value_width = numDigits(v,8);
    pad(spec.width, field_width, value_width, '0', dest);
    unsigned long long x = v.data.uVal;
    if (v.bits < 64)
      x &= (1llu << v.bits) - 1;
    char c;
    for (int i = (value_width - 1)*3; i >= 0; i -= 3)
    {
      c = '0' + (char) ((x >> i) & 0x7);
      dest->write_char(c);
    }
  }

  return spec.after;
}

// Printing routine for %b and %B formats
const char* print_binary(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  tValue v;
  FILL_TVALUE_KEEPING_STRINGS(v, args, dest, bin);

  /* There are 2 cases:
   *  %b  => width determined by type size, all bits shown
   *  %nb => minimum width is n, left-padded with zeros
   */
  unsigned int field_width = maxWidth(v.bits, false, 2);
  if (v.usingWideVal)
  {
    pad(spec.width, field_width, field_width, '0', dest);
    if (spec.width == 0)
      v.data.wideVal->print_binary(dest, 0);
    else
      v.data.wideVal->print_binary(dest, field_width);
  }
  else
  {
    unsigned int value_width = numDigits(v,2);
    pad(spec.width, field_width, value_width, '0', dest);
    char buf[(size_t)value_width + 1];
    buf[value_width] = '\0';
    for (unsigned int bit=0, idx=value_width-1; bit < value_width; ++bit,--idx)
      buf[idx] = ((v.data.uVal & (1llu << bit)) != 0llu) ? '1' : '0';
    dest->write_data(buf,value_width,sizeof(char));
  }

  return spec.after;
}

// Printing routine for %c format
const char* print_char(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  char c = args->getUChar();
  pad(spec.width, 1, 1, ' ', dest);
  dest->write_char(c);
  return spec.after;
}

// Printing routine for %s format
const char* print_string(tFieldDesc& spec, ArgList* args, Target* dest)
{
  if (args->isDone())
  {
    dest->write_char('%');
    return spec.str;  // there is no argument, so do not treat as format
  }

  if (args->isString())
  {
    // a character-array argument: view it in place
    unsigned int str_len = args->argSize() / 8;
    const char* str = args->getString();
    pad(spec.width, str_len, str_len, ' ', dest);
    for (unsigned int i = 0; i < str_len; ++i)
      dest->write_char(str[i]);
  }
  else if (args->isStringTree())
  {
    // a string-tree argument: stream its leaves straight to the
    // target, without flattening (see bs_str.h)
    const tStr* t = args->getStringTree();
    unsigned int str_len = bs_str_len(t);
    pad(spec.width, str_len, str_len, ' ', dest);
    tUInt32 pos = 0u;
    while (pos < str_len)
    {
      tUInt32 off;
      const tStr* leaf = bs_str_leaf_at(t, pos, &off);
      dest->write_data(leaf->data, sizeof(char), leaf->len);
      pos = off + leaf->len;
    }
  }
  else
  {
    // interpret a number as a string: the characters start at the
    // MSB and each byte is treated as a character moving toward the
    // LSB, with leading zero bytes ignored.  The characters are
    // written straight to the target (the old code built them into
    // a heap-allocated intermediate string first).
    tValue v;
    fill_tValue(v, args, dest);
    if (v.usingWideVal)
    {
      unsigned int n = (v.data.wideVal->size() + 7) / 8;
      while ((n > 0) && (v.data.wideVal->getByte(n-1) == 0)) --n;
      pad(spec.width, n, n, ' ', dest);
      while (n-- > 0)
        dest->write_char((char) v.data.wideVal->getByte(n));
    }
    else if (v.bits == 1)
    {
      unsigned int n = (v.data.bitVal == 1) ? 1 : 0;
      pad(spec.width, n, n, ' ', dest);
      if (n > 0)
        dest->write_char(1);
    }
    else
    {
      unsigned int n = (v.bits + 7) / 8;
      while ((n > 0) && (((v.data.uVal >> (8*(n-1))) & 0xFF) == 0)) --n;
      pad(spec.width, n, n, ' ', dest);
      while (n-- > 0)
        dest->write_char((char) ((v.data.uVal >> (8*n)) & 0xFF));
    }
  }

  return spec.after;
}

// Printing routine for %t format
const char* print_time(tFieldDesc& spec, ArgList* args, Target* dest)
{
  // print as decimal, but with a min field width appropriate for a
  // 64 bit value, regardless of the input size (if not specified).
  if (spec.width == -1)
    spec.width = 20;
  return print_decimal(spec, args, dest);
}

// Printing routine for real number formats
// The formatting itself is done by the host through the format_real
// host operation (the host re-uses printf's floating-point printing)
const char* print_real(tFieldDesc& spec, ArgList* args, Target* dest)
{
  // make a NUL-terminated copy of the format field ("%" plus the
  // field spec) on the stack; its length is bounded by the format
  // string in the generated code
  size_t format_size = 2 + (size_t)(spec.after - spec.str);
  char format_copy[format_size];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
  format_copy[0] = '%';
  {
    char* q = format_copy + 1;
    for (const char* cur = spec.str; cur != spec.after; ++cur)
      *(q++) = *cur;
    *q = '\0';
  }

  double v; // value to print

  if (args->isDouble()) {
    v = args->getDouble();
  }
  else {
    // non-real where real expected
    dest->add_error("expected real argument, found non-real argument\n");
    tValue tv;
    FILL_TVALUE_KEEPING_STRINGS(tv, args, dest, real);
    v = tValueToDouble(tv);
  }

  dest->write_real(format_copy, v);

  return spec.after;
}


// Dispatcher which parses field specifier and calls mode-specific fns
const char* handle_format(const char* str, Module* location, ArgList* args,
                          Target* dest)
{
  tFieldDesc spec;
  spec.mode = '\0';
  spec.width = -1;
  spec.precision = -1;
  spec.str = str;
  spec.after = NULL;

  const char* cptr = str;

  // parse width specifier
  while ((*cptr != '\0') && isDigit(*cptr))
  {
    if (spec.width < 0)
      spec.width = fromDigit(*cptr);
    else
      spec.width = spec.width * 10 + fromDigit(*cptr);
    ++cptr;
  }

  // parse precision
  if (*cptr == '.')
  {
    ++cptr;
    while ((*cptr != '\0') && isDigit(*cptr))
    {
      if (spec.precision < 0)
        spec.precision = fromDigit(*cptr);
      else
        spec.precision = spec.width * 10 + fromDigit(*cptr);
      ++cptr;
    }
  }

  // get mode
  spec.mode = *cptr;
  ++cptr;

  // handle the various format modes
  spec.after = cptr;
  switch (spec.mode)
  {
    case '%':
    {
      dest->write_char('%');
      return spec.after;
    }

    case 'b':
    case 'B':
    case 'u': // %u and %z are same as %b since we are only 2-state
    case 'U':
    case 'z':
    case 'Z': return print_binary(spec,args,dest);
    case 'c':
    case 'C': return print_char(spec,args,dest);
    case 'd':
    case 'D': return print_decimal(spec,args,dest);
    case 'h':
    case 'H':
    case 'x':
    case 'X': return print_hex(spec,args,dest);
    case 'm':
    case 'M': { location->write_name(dest); return spec.after; }
    case 'o':
    case 'O': return print_octal(spec,args,dest);
    case 's':
    case 'S': return print_string(spec,args,dest);
    case 't':
    case 'T': return print_time(spec,args,dest);
    case 'f':
    case 'F':
    case 'g':
    case 'G':
    case 'e':
    case 'E': return print_real(spec,args,dest);
    default: // not a supported format code
    {
      dest->write_char('%');
      return spec.str;
    }
  }
}

// This is a generic argument/format string processing routine used
// by many varieties of display and write system tasks.
void format(const char* default_format, Module* location, ArgList* args,
            Target* dest, bool restricted)
{
  unsigned int arg_num = 0;
  while (!args->isDone())
  {
    ++arg_num;
    bool is_str = args->isString() || args->isStringTree();
    bool is_fmt = (!restricted && is_str) || (restricted && (arg_num == 1));
    if (is_fmt)
    {
      // use this argument as a format string

      // Iterate over the string looking for escape and format
      // codes.  When one is found, print any prior characters
      // which haven't been printed and then process the special
      // character, possibly consuming arguments.

      // The format parser needs contiguous, NUL-terminated
      // characters.  A character-array argument is walked in place;
      // a string tree or a numeric value reinterpreted as a format
      // string is staged in stack storage first: one character per
      // byte, plus a NUL terminator, sized by the tree's own byte
      // count or the argument's descriptor (unused -- zero bytes --
      // when the argument is a character array).
      const char* direct = NULL;
      const tStr* tree = NULL;
      if (args->isString())
        direct = args->getString();
      else if (args->isStringTree())
      {
        tree = args->getStringTree();
        if (tree == NULL)
          direct = "";  // an absent def is an empty format
      }
      unsigned int conv_bytes =
        (direct != NULL) ? 0
                         : (tree != NULL) ? bs_str_len(tree)
                                          : (args->argSize() + 7) / 8;
      char conv_buf[conv_bytes + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)

      const char* cptr = NULL;
      if (direct != NULL)
      {
        cptr = direct;
      }
      else if (tree != NULL)
      {
        cptr = bs_str_flatten(tree, conv_buf);
      }
      else
      {
        // The value is not a string but must be interpreted as one
        cptr = convert_to_chars(args, dest, conv_buf);
      }

      unsigned int len = 0;
      while (cptr && cptr[len] != '\0')
      {
        if (cptr[len] == '\\')
        {
          if (len > 0) dest->write_data(cptr, sizeof(char), len);
          cptr = handle_escape((cptr + len + 1), dest);
          len = 0;
        } else if (cptr[len] == '%') {
          if (len > 0) dest->write_data(cptr, sizeof(char), len);
          cptr = handle_format((cptr + len + 1), location, args, dest);
          len = 0;
        } else {
          // ordinary character, just record that we have another
          // character "in the buffer".
          ++len;
        }
      }

      // write any trailing characters
      if (len > 0) dest->write_data(cptr, sizeof(char), len);
    } else if (is_str) {
      // display the argument as a string literal
      handle_format("s", location, args, dest);
    } else {
      // display the argument in default format
      handle_format(default_format, location, args, dest);
    }
  }
}

/*
 * These are the actual system task definitions.
 */


// $display
void dollar_display(tSimStateHdl simHdl,
		    Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("d", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $display with no arguments (just prints a newline)
void dollar_display(tSimStateHdl simHdl, Module* /* unused */)
{
  if (!bk_finished(simHdl))
  {
    FileTarget dest(simHdl);
    dest.write_char('\n');
    dest.handle_errors();
  }
}

// $displayb
void dollar_displayb(tSimStateHdl simHdl,
		     Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("b", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $displayb with no arguments (just prints a newline)
void dollar_displayb(tSimStateHdl simHdl, Module* /* unused */)
{
  if (!bk_finished(simHdl))
  {
    FileTarget dest(simHdl);
    dest.write_char('\n');
    dest.handle_errors();
  }
}

// $displayo
void dollar_displayo(tSimStateHdl simHdl,
		     Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("o", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $displayo with no arguments (just prints a newline)
void dollar_displayo(tSimStateHdl simHdl, Module* /* unused */)
{
  if (!bk_finished(simHdl))
  {
    FileTarget dest(simHdl);
    dest.write_char('\n');
    dest.handle_errors();
  }
}

// $displayh
void dollar_displayh(tSimStateHdl simHdl,
		     Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("h", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $displayh with no arguments (just prints a newline)
void dollar_displayh(tSimStateHdl simHdl, Module* /* unused */)
{
  if (!bk_finished(simHdl))
  {
    FileTarget dest(simHdl);
    dest.write_char('\n');
    dest.handle_errors();
  }
}

// $write
void dollar_write(tSimStateHdl simHdl,
		  Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("d", location, &args, &dest, false);
    dest.handle_errors();
  }

  va_end(ap);
}

// $write with no arguments (has no effect)
void dollar_write(tSimStateHdl /* unused */, Module* /* unused */)
{
  return;
}

// $writeb
void dollar_writeb(tSimStateHdl simHdl,
		   Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("b", location, &args, &dest, false);
    dest.handle_errors();
  }

  va_end(ap);
}

// $writeb with no arguments (has no effect)
void dollar_writeb(tSimStateHdl /* unused */, Module* /* unused */)
{
  return;
}

// $writeo
void dollar_writeo(tSimStateHdl simHdl,
		   Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("o", location, &args, &dest, false);
    dest.handle_errors();
  }

  va_end(ap);
}

// $writeo with no arguments (has no effect)
void dollar_writeo(tSimStateHdl /* unused */, Module* /* unused */)
{
  return;
}

// $writeh
void dollar_writeh(tSimStateHdl simHdl,
		   Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("h", location, &args, &dest, false);
    dest.handle_errors();
  }

  va_end(ap);
}

// $writeh with no arguments (has no effect)
void dollar_writeh(tSimStateHdl /* unused */, Module* /* unused */)
{
  return;
}

// Copy a formatted string from the destination buffer into the
// target memory with proper alignment, left-padded zeros,
// no terminator, etc.
void copy_back(void* target, unsigned int bits, BufferTarget* dest)
{
  unsigned int n = dest->length();
  const char* s = dest->str();

  if (bits <= 8)
  {
    *((tUInt8*) target) = *s;
  }
  else if (bits <= 32)
  {
    tUInt32 x = 0;
    while (n--)
      x |= (*(s++) << (8*n));
    *((tUInt32*) target) = x;
  }
  else if (bits <= 64)
  {
    tUInt64 x = 0llu;
    while (n--)
      x |= ((tUInt64)(*(s++)) << (8*n));
    *((tUInt64*) target) = x;
  }
  else
  {
    WideData* x = (WideData*) target;
    if (bits > (8*n))
      x->clear(8*n);
    while (n--)
      x->setByte(n,*(s++));
  }
}

// $swrite
void dollar_swriteAV(tSimStateHdl simHdl,
		     Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);

    // first argument is destination
    if (args.isDone() || !args.isPointer())
    {
      // this is an error
    }
    else
    {
      unsigned int bits = args.argSize();
      void* target = args.getPointer();
      // the string buffer lives on the stack, sized by the
      // destination argument's own width (one character per byte,
      // plus the terminator BufferTarget maintains)
      char dest_store[(bits + 7) / 8 + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
      BufferTarget dest(simHdl, dest_store, (bits + 7) / 8);

      // remaining arguments are for format
      format("d", location, &args, &dest, false);

      copy_back(target, bits, &dest);
      dest.handle_errors();
    }
  }

  va_end(ap);
}

// $swriteb
void dollar_swritebAV(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);

    // first argument is destination
    if (args.isDone() || !args.isPointer())
    {
      // this is an error
    }
    else
    {
      unsigned int bits = args.argSize();
      void* target = args.getPointer();
      // the string buffer lives on the stack, sized by the
      // destination argument's own width (one character per byte,
      // plus the terminator BufferTarget maintains)
      char dest_store[(bits + 7) / 8 + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
      BufferTarget dest(simHdl, dest_store, (bits + 7) / 8);

      // remaining arguments are for format
      format("b", location, &args, &dest, false);

      copy_back(target, bits, &dest);
      dest.handle_errors();
    }
  }

  va_end(ap);
}

// $swriteo
void dollar_swriteoAV(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);

    // first argument is destination
    if (args.isDone() || !args.isPointer())
    {
      // this is an error
    }
    else
    {
      unsigned int bits = args.argSize();
      void* target = args.getPointer();
      // the string buffer lives on the stack, sized by the
      // destination argument's own width (one character per byte,
      // plus the terminator BufferTarget maintains)
      char dest_store[(bits + 7) / 8 + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
      BufferTarget dest(simHdl, dest_store, (bits + 7) / 8);

      // remaining arguments are for format
      format("o", location, &args, &dest, false);

      copy_back(target, bits, &dest);
      dest.handle_errors();
    }
  }

  va_end(ap);
}

// $swriteh
void dollar_swritehAV(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);

    // first argument is destination
    if (args.isDone() || !args.isPointer())
    {
      // this is an error
    }
    else
    {
      unsigned int bits = args.argSize();
      void* target = args.getPointer();
      // the string buffer lives on the stack, sized by the
      // destination argument's own width (one character per byte,
      // plus the terminator BufferTarget maintains)
      char dest_store[(bits + 7) / 8 + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
      BufferTarget dest(simHdl, dest_store, (bits + 7) / 8);

      // remaining arguments are for format
      format("h", location, &args, &dest, false);

      copy_back(target, bits, &dest);
      dest.handle_errors();
    }
  }

  va_end(ap);
}


// $sformat
void dollar_sformatAV(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);

    // first argument is destination
    if (args.isDone() || !args.isPointer())
    {
      // this is an error
    }
    else
    {
      unsigned int bits = args.argSize();
      void* target = args.getPointer();
      // the string buffer lives on the stack, sized by the
      // destination argument's own width (one character per byte,
      // plus the terminator BufferTarget maintains)
      char dest_store[(bits + 7) / 8 + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
      BufferTarget dest(simHdl, dest_store, (bits + 7) / 8);

      // remaining arguments are for format
      format("d", location, &args, &dest, true);

      copy_back(target, bits, &dest);
      dest.handle_errors();
    }
  }

  va_end(ap);
}

// $info
void dollar_info(tSimStateHdl simHdl,
		 Module* location, const char* size_str, ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("d", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $info with no arguments
void dollar_info(tSimStateHdl simHdl, Module* location)
{
  dollar_display(simHdl, location);
}

// $warning
void dollar_warning(tSimStateHdl simHdl,
		    Module* location, const char* size_str, ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("d", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $warning with no arguments
void dollar_warning(tSimStateHdl simHdl, Module* location)
{
  dollar_display(simHdl, location);
}

// $error
void dollar_error(tSimStateHdl simHdl,
		  Module* location, const char* size_str, ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);
    format("d", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
  }

  va_end(ap);
}

// $error with no arguments
void dollar_error(tSimStateHdl simHdl, Module* location)
{
  dollar_display(simHdl, location);
}

// $fatal
void dollar_fatal(tSimStateHdl simHdl,
		  Module* location, const char* size_str, ...)
{
  va_list ap;
  va_start(ap,size_str);

  if (!bk_finished(simHdl))
  {
    ArgList args(size_str, &ap);
    FileTarget dest(simHdl);

    // first argument is the exit status
    tValue v;
    FILL_TVALUE_KEEPING_STRINGS(v, &args, &dest, fat);

    int status = 0;
    if (v.usingWideVal)
      status = v.data.wideVal->extract32(31,0);
    else if (v.bits == 1)
      status = v.data.bitVal ? 1 : 0;
    else
      status = v.data.sVal;

    // Display message and then finish simulation
    format("d", location, &args, &dest, false);
    dest.write_char('\n');
    dest.handle_errors();
    bk_fatal_now(simHdl, status);
  }

  va_end(ap);
}

// $fatal must always have an argument (the status)

/*
 * Utility class for dealing with file-related system tasks.
 */

// Verilog has 2 concepts of file pointer: file handle, similar to C and mcd,
// multi-channel descriptors, which can represent multiple files.
// Here we define 2 containers for holding these files, and some functions
// to access them.
//
// The files themselves are host streams: they are opened, written and
// closed through the host operations registered with bk_sync_init().
// Because these system tasks are not passed a simulation handle, the
// process-wide host ops (bk_host_ops(NULL)) are used, and the standard
// streams are registered lazily on first use (the host ops do not
// exist yet when this file's static storage is initialized).
class VLFiles {
private:
  // MCD keys are one-hot in a 31-bit space, so at most 31 MCD files
  // can be live at once and their table is a fixed array; fd keys are
  // indices, so their table grows on demand through the Bluesim
  // allocator.  All members are constant-initialized: the static
  // instance below runs no constructor and makes no allocator calls
  // when the model is loaded.
  bs_host_file*  mcdfiles[31] = {} ;
  tUInt32        mcd_count = 0 ;
  bs_host_file** fdfiles = NULL ;
  tUInt32        fd_count = 0 ;
  tUInt32        fd_capacity = 0 ;
  bool std_registered = false ;

  const static tUInt32 fdbase = 0x80000000 ;

  // number of allocator words holding 'n' file pointers
  static unsigned int fd_table_words( tUInt32 n )
  {
    return (unsigned int) ((n * sizeof(bs_host_file*) +
                            sizeof(unsigned int) - 1) /
                           sizeof(unsigned int)) ;
  }

  void append_fd( bs_host_file* file )
  {
    if ( fd_count == fd_capacity ) {
      tUInt32 new_capacity = (fd_capacity == 0) ? 16 : 2 * fd_capacity ;
      bs_host_file** bigger =
        (bs_host_file**) alloc_mem( fd_table_words( new_capacity )) ;
      for ( tUInt32 i = 0 ; i < fd_count ; i = i + 1 )
        bigger[i] = fdfiles[i] ;
      if ( fdfiles != NULL )
        free_mem( fdfiles, fd_table_words( fd_capacity )) ;
      fdfiles = bigger ;
      fd_capacity = new_capacity ;
    }
    fdfiles[fd_count] = file ;
    fd_count = fd_count + 1 ;
  }

  void ensure_std_registered()
  {
    if (std_registered)
      return ;
    const struct bs_host_ops* ops = bk_host_ops(NULL) ;
    if (ops == NULL)
      return ; // no simulation has been initialized yet
    void* ctx = bk_host_ctx(NULL) ;
    std_registered = true ; // set first: registerFile calls back here
    // preopened and registered files.
    registerFile( true, ops->std_stream( ctx, BS_HOST_STDOUT )) ;
    registerFile( false, ops->std_stream( ctx, BS_HOST_STDIN )) ;
    registerFile( false, ops->std_stream( ctx, BS_HOST_STDOUT )) ;
    registerFile( false, ops->std_stream( ctx, BS_HOST_STDERR )) ;
  }

public:
  // The implicit constructor and destructor are used: all members are
  // constant-initialized, and the system closes any open files for us.

  // After a call to the open host operation, store the stream handle
  tUInt32 registerFile ( bool mcd, bs_host_file* file )
  {
    ensure_std_registered() ;
    tUInt32 key = 0 ;
    if ( file == 0 ) {
      key = 0 ;
    } else if ( mcd && (mcd_count < 31 )) {
      mcdfiles[mcd_count] = file ;
      mcd_count = mcd_count + 1 ;
      key = 0x01 << (mcd_count - 1)  ;
    } else if ( mcd ) {
      for( tUInt32 i = 0 ; i <  mcd_count ; i = i + 1 ) {
        if ( mcdfiles[i] == 0 ){
          mcdfiles[i] = file ;
          key = 0x01 << i;
          break ;
        }
      }
    } else {
      append_fd( file ) ;
      key = fdbase + (fd_count - 1);
    }
    return key ;
  }

  // The largest number of files one key can name: an MCD key is a
  // 31-bit one-hot mask (at most 31 files) and an fd key names
  // exactly one file.  Callers provide a result array of this size.
  static const unsigned int MAX_FILES_PER_KEY = 31 ;

  // MCD can cause multiple files to be specified.  Fills the
  // caller's fixed result array (MAX_FILES_PER_KEY entries suffice)
  // and returns the number of files found.
  unsigned int findFiles( bs_host_file* result[], tUInt32 key )
  {
    ensure_std_registered() ;
    unsigned int count = 0 ;
    if ( key >= fdbase ) {     // fd type
      if ( fdfiles[key - fdbase] != 0 )
        result[count++] = fdfiles[key - fdbase] ;
      // XXX check for valid index done by stl?
    } else { // mcd type
      tUInt32 position = 0  ;
      while (key != 0 ) {
        if ( (key & 0x01) && mcdfiles[position] ) {
          result[count++] = mcdfiles[position] ;
        }
        key = key >> 1 ;
        position = position + 1 ;
      }

    }
    return count ;
  }

  // Flush every registered file.  The fd table has no fixed bound,
  // so this iterates the tables in place instead of copying them
  // into a caller's array.
  void flushAll()
  {
    ensure_std_registered() ;
    const struct bs_host_ops* ops = bk_host_ops(NULL) ;
    if ( ops == NULL )
      return ;
    void* ctx = bk_host_ctx(NULL) ;
    for ( tUInt32 i = 0 ; i < fd_count ; i = i + 1 )
      ops->flush( ctx, fdfiles[i] ) ;
    for ( tUInt32 i = 0 ; i < mcd_count ; i = i + 1 )
      ops->flush( ctx, mcdfiles[i] ) ;
  }
  void closeFiles( tUInt32 key )
  {
    ensure_std_registered() ;
    const struct bs_host_ops* ops = bk_host_ops(NULL) ;
    void* ctx = bk_host_ctx(NULL) ;
    if ( ops == NULL )
      return ;
    // Don't close stdin, stdout or stderr
    if ( key > 0x80000002 ) {     // fd type
      if ( fdfiles[key - fdbase] != 0 )
        {
          ops->close( ctx, fdfiles[key - fdbase] ) ;
          fdfiles[key - fdbase] = 0 ;
        }
      // XXX check for valid index done by stl?
    } else if ( key < 0x8000000 ) { // mcd type
      tUInt32 position = 1  ;
      key = key >> 1 ;          // remove stdout
      while (key != 0 ) {
        if ( (key & 0x01) && mcdfiles[position] )
          {
            ops->close( ctx, mcdfiles[position] ) ;
            mcdfiles[position] = 0 ;
          }
        key = key >> 1 ;
        position = position + 1 ;
      }
    }
  }

  bs_host_file* getFD( tUInt32 key )
  {
    ensure_std_registered() ;
    bs_host_file *res =  0 ;
      if (( key >= fdbase ) && (fdfiles[key - fdbase] != 0 ))  {
        res = fdfiles[key - fdbase]  ;
      }
      return res ;
  }

} ; // end class

// Now create a global VLFiles
static VLFiles vlfile ;

/*
 * These are the "file" based system tasks
 */

// Read one string argument off an ArgList.  Generated code passes
// string literals as plain character arrays (with a sized
// descriptor, already NUL-terminated; stored in *chars) and string
// defs as string trees (with an unsized descriptor; stored in
// *tree).  The names used here (file names, open modes) have
// C-string semantics, so the caller flattens a tree into a stack
// buffer of bs_str_len(*tree) + 1 bytes.  Both outputs are NULL for
// a missing or non-string argument.
static void string_arg(ArgList* args, const char** chars, const tStr** tree)
{
  *chars = NULL ;
  *tree = NULL ;
  if (args->isDone())
    return ;
  if (args->isString())
    *chars = args->getString() ;
  else if (args->isStringTree())
    *tree = args->getStringTree() ;
  else
    args->skip() ;
}

// $fopen( filename ) opens a multi-channel descriptor;
// $fopen( filename, mode ) opens an fd-style descriptor.
tUInt32 dollar_fopen(const char* size_str, ...)
{
  va_list ap;
  va_start(ap, size_str);
  ArgList args(size_str, &ap);
  const char* fname_chars; const tStr* fname_tree;
  string_arg(&args, &fname_chars, &fname_tree);
  bool mcd = args.isDone();
  const char* mode_chars = mcd ? "w" : NULL; const tStr* mode_tree = NULL;
  if (!mcd)
    string_arg(&args, &mode_chars, &mode_tree);
  va_end(ap);

  // flatten tree-valued names into stack storage with C-string
  // semantics (VLAs, see DYNAMIC_VLA_FUNCTIONS)
  char fname_buf[bs_str_len(fname_tree) + 1];
  char mode_buf[bs_str_len(mode_tree) + 1];
  const char* filename = (fname_tree != NULL)
                           ? bs_str_flatten(fname_tree, fname_buf)
                           : fname_chars;
  const char* mode = (mode_tree != NULL)
                       ? bs_str_flatten(mode_tree, mode_buf)
                       : mode_chars;

  const struct bs_host_ops* ops = bk_host_ops(NULL);
  if ((ops == NULL) || (filename == NULL) || (mode == NULL))
    return 0 ;
  bs_host_file* nowopened =
    ops->open(bk_host_ctx(NULL), filename, mode);
  tUInt32 key = vlfile.registerFile(mcd, nowopened);
  return key ;
}

// $fclose(filehandle)
void dollar_fclose(const char* /*unused*/, tUInt32 filehandle)
{
  vlfile.closeFiles( filehandle ) ;
}

// $fflush( filehandle )
void dollar_fflush(const char* /*unused*/, tUInt32 filehandle)
{
  bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
  unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;
  const struct bs_host_ops* ops = bk_host_ops(NULL);
  if (ops == NULL)
    return ;
  for ( unsigned int i = 0 ; i < nfiles ; i ++ )
    ops->flush( bk_host_ctx(NULL), files[i] );
}


// $fflush()
void dollar_fflush()
{
  vlfile.flushAll() ;
}


// $fdisplay
void dollar_fdisplay(tSimStateHdl simHdl,
		     Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("d", location, &local_args, &dest, false);
      dest.write_char('\n');
      dest.handle_errors();

      va_end(ap);
    }
  }
}


// $fdisplayb
void dollar_fdisplayb(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("b", location, &local_args, &dest, false);
      dest.write_char('\n');
      dest.handle_errors();

      va_end(ap);
    }
  }
}


// $fdisplayo
void dollar_fdisplayo(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("o", location, &local_args, &dest, false);
      dest.write_char('\n');
      dest.handle_errors();

      va_end(ap);
    }
  }
}

// $fdisplayh
void dollar_fdisplayh(tSimStateHdl simHdl,
		      Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("h", location, &local_args, &dest, false);
      dest.write_char('\n');
      dest.handle_errors();

      va_end(ap);
    }
  }
}

// $fwrite
void dollar_fwrite(tSimStateHdl simHdl,
		   Module* location, /*tUInt32 filehandle, */ const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("d", location, &local_args, &dest, false);
      dest.handle_errors();

      va_end(ap);
    }
  }
}

// $fwriteb
void dollar_fwriteb(tSimStateHdl simHdl,
		    Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("b", location, &local_args, &dest, false);
      dest.handle_errors();

      va_end(ap);
    }
  }
}

// $fwriteo
void dollar_fwriteo(tSimStateHdl simHdl,
		    Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("o", location, &local_args, &dest, false);
      dest.handle_errors();

      va_end(ap);
    }
  }
}

// $fwriteh
void dollar_fwriteh(tSimStateHdl simHdl,
		    Module* location, const char* size_str ...)
{
  if (!bk_finished(simHdl))
  {
    va_list ap;
    va_start(ap,size_str);
    ArgList args(size_str, &ap);

    bs_host_file* files[VLFiles::MAX_FILES_PER_KEY] ;
    tUInt32 filehandle = args.getUInt() ;

    va_end(ap) ;

    unsigned int nfiles = vlfile.findFiles( files, filehandle ) ;

    for( unsigned int i = 0 ; i < nfiles ; i ++ )
    {
      // Reset the arg and continue
      va_start( ap, size_str) ;
      ArgList local_args(size_str, &ap);
      local_args.getUInt() ;

      FileTarget dest(simHdl, files[i]);
      format("h", location, &local_args, &dest, false);
      dest.handle_errors();

      va_end(ap);
    }
  }
}

tSInt32 dollar_fgetc ( const char* /*Unused*/, const tUInt32 filehandle )
{
  bs_host_file * infile = vlfile.getFD( filehandle ) ;
  int res = -1 ;
  const struct bs_host_ops* ops = bk_host_ops(NULL);
  if ( infile && (ops != NULL) )
  {
    char c ;
    if ( ops->read( bk_host_ctx(NULL), infile, &c, 1 ) == 1 )
      res = (unsigned char) c ;
  }

  return res ;
}

// $ungetc( char, file )
tSInt32 dollar_ungetc(  const char* size_str, const char back, const tUInt32 filehandle )
{
  bs_host_file * infile = vlfile.getFD( filehandle ) ;
  int res = -1 ;
  const struct bs_host_ops* ops = bk_host_ops(NULL);
  if ( infile && (ops != NULL) )
    res = ops->unget_char( bk_host_ctx(NULL), infile, back ) ;

  return res ;
}
