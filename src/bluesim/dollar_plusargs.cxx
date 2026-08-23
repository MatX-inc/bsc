#include <cstdarg>

#include "bluesim_kernel_api.h"
#include "bs_str.h"

bool dollar_test_dollar_plusargs(tSimStateHdl simHdl,
				 const char* size_str, ...)
{
  va_list ap;
  va_start(ap, size_str);
  const char* chars = NULL;
  const tStr* tree = NULL;
  /* A sized descriptor ("40s") marks a plain character-array literal
   * (already NUL-terminated); the unsized form ("s") marks a string
   * tree def (see bs_str.h).
   */
  if ((size_str != NULL) && (size_str[0] >= '0') && (size_str[0] <= '9'))
    chars = va_arg(ap, const char*);
  else
    tree = va_arg(ap, const tStr*);
  va_end(ap);

  /* a tree-valued name is flattened into stack storage with C-string
   * semantics (a VLA, see DYNAMIC_VLA_FUNCTIONS) */
  char name_buf[bs_str_len(tree) + 1];
  const char* name = (tree != NULL) ? bs_str_flatten(tree, name_buf)
                                    : chars;

  return (name != NULL) && (bk_match_argument(simHdl, name) != NULL);
}
