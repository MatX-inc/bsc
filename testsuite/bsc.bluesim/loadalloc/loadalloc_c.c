/* The foreign function imported by sysLoadAlloc.bsv.  Its string
 * argument reaches it as a NUL-terminated C string copied by the
 * runtime's copy_arg().
 */

#include <string.h>

unsigned int loadalloc_strlen(char* s)
{
  return (unsigned int) strlen(s);
}
