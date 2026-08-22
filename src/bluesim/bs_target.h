#ifndef __TARGET_H__
#define __TARGET_H__

#include <algorithm>
#include <list>
#include <string>

#include "bluesim_kernel_api.h"

// This class abstracts the target location for output.
// It is used to allow the same functions to generate strings
// to a file or to a buffer.
//
// All file output is performed through the host operations
// registered with bk_sync_init() (see bluesim_host_ops.h).
class Target
{
private:
  std::list<std::string> errors;

protected:
  const struct bs_host_ops* host_ops;
  void* host_ctx;

public:
  Target(const struct bs_host_ops* ops, void* ctx)
    : errors(std::list<std::string>()), host_ops(ops), host_ctx(ctx) {};
  virtual ~Target() { handle_errors(); };

  void add_error(const char* error) {
    errors.push_front(error);
  };

  void handle_errors();

  virtual void write_char(char c) = 0;
  virtual void write_char(char c, unsigned int count) = 0;
  virtual void write_data(const void* data, unsigned int size, unsigned int num) = 0;

  // Formatting helpers (the runtime's hand-rolled printers, used in
  // place of the printf family for the formats the runtime needs).

  // write a NUL-terminated string
  void write_string(const char* str);

  // write an unsigned decimal number ("%llu")
  void write_decimal(tUInt64 value);

  // write an unsigned hexadecimal number, in lower case, zero-padded
  // on the left to at least min_digits digits ("%0*llx")
  void write_hex(tUInt64 value, unsigned int min_digits = 1);

  // write one real (double) value, formatted by the host through the
  // format_real host operation ('format' is a printf-style format
  // string with exactly one %e/%f/%g conversion)
  void write_real(const char* format, double value);
};

// Send output to a host file
class FileTarget : public Target
{
private:
  struct bs_host_file* out;
public:
  FileTarget(const struct bs_host_ops* ops, void* ctx,
	     struct bs_host_file* file);
  FileTarget(tSimStateHdl simHdl, struct bs_host_file* file);
  FileTarget(tSimStateHdl simHdl, tHostStdStream which = BS_HOST_STDOUT);
  ~FileTarget();
  void write_char(char c);
  void write_char(char c, unsigned int count);
  void write_data(const void* data, unsigned int size, unsigned int num);
};

// Capture output in a string
class BufferTarget : public Target
{
private:
  char* buffer;
  unsigned int buf_size;
  unsigned int start;
  unsigned int end;
public:
  BufferTarget(tSimStateHdl simHdl, unsigned int size);
  ~BufferTarget();
  void write_char(char c);
  void write_char(char c, unsigned int count);
  void write_data(const void* data, unsigned int size, unsigned int num);
  const char* str();
  unsigned int length() const;
};

#endif /* __TARGET_H__ */
