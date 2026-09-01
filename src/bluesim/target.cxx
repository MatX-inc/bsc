#include <cerrno>
#include <cstring>

#include "bs_target.h"

// Base class helpers

// Record an error message in the fixed error storage (truncating an
// over-long message; dropping the message when the storage is full)
void Target::add_error(const char* error)
{
  if (num_errors == MAX_ERRORS)
    return;
  char* slot = errors[num_errors];
  unsigned int i = 0;
  while ((error[i] != '\0') && (i < MAX_ERROR_LEN - 1))
  {
    slot[i] = error[i];
    ++i;
  }
  slot[i] = '\0';
  ++num_errors;
}

// Report accumulated errors on the host's stdout stream, most
// recently added first (the order the old error list produced)
void Target::handle_errors()
{
  while (num_errors > 0) {
    const char* msg = errors[--num_errors];
    if (host_ops != NULL)
    {
      struct bs_host_file* out = host_ops->std_stream(host_ctx, BS_HOST_STDOUT);
      host_ops->write(host_ctx, out, "Output error: ", 14);
      host_ops->write(host_ctx, out, msg, strlen(msg));
    }
  }
}

// Write a NUL-terminated string
void Target::write_string(const char* str)
{
  unsigned int len = strlen(str);
  if (len > 0)
    write_data(str, sizeof(char), len);
}

// Write an unsigned decimal number ("%llu")
void Target::write_decimal(tUInt64 value)
{
  char buf[20]; // a 64-bit value has at most 20 decimal digits
  unsigned int digits = 0;
  do {
    buf[digits++] = '0' + (char)(value % 10llu);
    value /= 10llu;
  } while (value != 0llu);
  while (digits > 0)
    write_char(buf[--digits]);
}

// Write an unsigned hexadecimal number, in lower case, zero-padded
// on the left to at least min_digits digits ("%0*llx")
void Target::write_hex(tUInt64 value, unsigned int min_digits)
{
  unsigned int digits = 1;
  for (tUInt64 x = value >> 4; x != 0llu; x >>= 4)
    ++digits;
  if (min_digits > digits)
    write_char('0', min_digits - digits);
  while (digits > 0)
  {
    --digits;
    unsigned int nibble = (unsigned int)((value >> (4*digits)) & 0xF);
    write_char((nibble > 9) ? ('a' + (char)(nibble - 10))
                            : ('0' + (char)nibble));
  }
}

// Write one real (double) value, formatted by the host
void Target::write_real(const char* format, double value)
{
  char buf[256];
  int len = -1;
  if (host_ops != NULL)
    len = host_ops->format_real(host_ctx, buf, sizeof(buf), format, value);

  if (len < 0)
  {
    // compose the message in fixed storage (add_error() truncates
    // an over-long format harmlessly)
    char msg[MAX_ERROR_LEN];
    unsigned int pos = 0;
    static const char head[] = "printing real number with format ";
    static const char tail[] = " failed\n";
    for (unsigned int i = 0; head[i] != '\0'; ++i)
      if (pos < MAX_ERROR_LEN - 1) msg[pos++] = head[i];
    for (unsigned int i = 0; format[i] != '\0'; ++i)
      if (pos < MAX_ERROR_LEN - 1) msg[pos++] = format[i];
    for (unsigned int i = 0; tail[i] != '\0'; ++i)
      if (pos < MAX_ERROR_LEN - 1) msg[pos++] = tail[i];
    msg[pos] = '\0';
    add_error(msg);
  }
  else if (((size_t) len) < sizeof(buf))
  {
    write_data(buf, sizeof(char), len);
  }
  else
  {
    // the formatted value did not fit in the local buffer; retry
    // with a stack array sized by the reported length (which the
    // requested field width in the format string bounds)
    char big_buf[len + 1];  // VLA (see DYNAMIC_VLA_FUNCTIONS)
    if (host_ops->format_real(host_ctx, big_buf, len + 1, format, value) == len)
      write_data(big_buf, sizeof(char), len);
  }
}

// FileTarget implementation simply forwards output to a host file

FileTarget::FileTarget(const struct bs_host_ops* ops, void* ctx,
		       struct bs_host_file* file)
  : Target(ops, ctx), out(file)
{
}

FileTarget::FileTarget(tSimStateHdl simHdl, struct bs_host_file* file)
  : Target(bk_host_ops(simHdl), bk_host_ctx(simHdl)), out(file)
{
}

FileTarget::FileTarget(tSimStateHdl simHdl, tHostStdStream which)
  : Target(bk_host_ops(simHdl), bk_host_ctx(simHdl))
{
  out = host_ops->std_stream(host_ctx, which);
}

FileTarget::~FileTarget()
{
}

void FileTarget::write_char(char c)
{
  host_ops->write(host_ctx, out, &c, 1);
}

void FileTarget::write_char(char c, unsigned int count)
{
  char buf[64];
  memset(buf, c, std::min(count, 64u));
  while (count > 0)
  {
    unsigned int n = std::min(count, 64u);
    host_ops->write(host_ctx, out, buf, n);
    count -= n;
  }
}

void FileTarget::write_data(const void* data,
			    unsigned int size, unsigned int num)
{
  if (!host_ops->write(host_ctx, out, (const char*) data, size * num))
  {
    // report the failure on the host's stderr stream, in the manner
    // of perror("FileTarget::write_data")
    struct bs_host_file* err = host_ops->std_stream(host_ctx, BS_HOST_STDERR);
    const char* reason = strerror(errno);
    host_ops->write(host_ctx, err, "FileTarget::write_data: ", 24);
    host_ops->write(host_ctx, err, reason, strlen(reason));
    host_ops->write(host_ctx, err, "\n", 1);
  }
}

// Buffer target stores output in a fixed-size buffer.
// It is constructed to mimic Verilog assignment rules in which
// assigning a string to a buffer which is too small truncates
// the string by removing leading characters.  We achieve this
// efficiently by treating the target as a circular buffer.

BufferTarget::BufferTarget(tSimStateHdl simHdl, char* storage,
                           unsigned int size)
  : Target(bk_host_ops(simHdl), bk_host_ctx(simHdl))
{
  // the caller's storage contains one extra space for the null
  // terminator; it is borrowed, never freed
  buf_size = size + 1;
  buffer = storage;
  start = 0;
  end = 0;
  buffer[end] = '\0';
}

BufferTarget::~BufferTarget()
{
}

void BufferTarget::write_char(char c)
{
  // overwrite the null terminator and move the terminator
  // forward one space (possibly overwriting the beginning of
  // the string).
  buffer[end++] = c;
  if (end == buf_size) end = 0;
  if (end == start) start = (start + 1) % buf_size;
  buffer[end] = '\0';
}

void BufferTarget::write_char(char c, unsigned int count)
{
  // write 'count' copies of 'c', add a null terminator
  // and adjust the start and end index values.
  unsigned int bytes = std::min(count, (buf_size-1));
  // only buf_size - end bytes are contiguous before wrapping
  unsigned int back_bytes = std::min(bytes, (buf_size-end));
  unsigned int wrapped_bytes = bytes - back_bytes;
  unsigned int freespace = buf_size - 1 - length();
  if (back_bytes > 0)
    memset(buffer + end, c, back_bytes);
  if (wrapped_bytes > 0)
    memset(buffer, c, wrapped_bytes);
  end = (end + bytes) % buf_size;
  if (bytes > freespace)
    start = (start + bytes - freespace) % buf_size;
  buffer[end] = '\0';
}

void BufferTarget::write_data(const void* data,
			      unsigned int size, unsigned int num)
{
  // write size * num bytes of data, add a null terminator
  // and adjust the start and end index values.
  unsigned int bytes = std::min(size*num, (buf_size-1));
  unsigned int lost  = (size*num) - bytes;
  // only buf_size - end bytes are contiguous before wrapping
  unsigned int back_bytes = std::min(bytes, (buf_size-end));
  unsigned int wrapped_bytes = bytes - back_bytes;
  unsigned int freespace = buf_size - 1 - length();
  const char* ptr = (const char*) data;
  if (back_bytes > 0)
    memmove(buffer + end, ptr + lost, back_bytes);
  if (wrapped_bytes > 0)
    memmove(buffer, ptr + lost + back_bytes, wrapped_bytes);
  end = (end + bytes) % buf_size;
  if (bytes > freespace)
    start = (start + bytes - freespace) % buf_size;
  buffer[end] = '\0';
}

const char* BufferTarget::str()
{
  // Fix up circular buffer so that string is contiguous.
  // This is an in-place permutation of the string.
  unsigned int len = length();
  unsigned int base = 0;
  unsigned int lo   = start;
  unsigned int hi   = buf_size-1;

  while (base != lo)
  {
    unsigned int in_order = 1 + hi - lo;
    unsigned int out_of_order = lo - base;
    unsigned int n = (out_of_order < in_order) ? out_of_order : in_order;

    for (unsigned int i = 0; i < n; ++i)
    {
      char tmp = buffer[base + i];
      buffer[base + i] = buffer[lo + i];
      buffer[lo + i] = tmp;
    }

    base += n;
    if (n != in_order)
      lo += n;
  }

  start = 0;
  end   = len;
  return buffer;
}

unsigned int BufferTarget::length() const
{
  if (end >= start)
    return (end - start);
  else
    return (end + buf_size - start);
}
