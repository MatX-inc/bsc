/* Flat C accessors for libfst's fstHier record, for use from
 * fstscopes.hs via the FFI: the record contains a union, which the
 * Haskell FFI cannot portably access without a helper.
 */

#include <stdlib.h>

#include "fstapi.h"

/* 0 = scope, 1 = upscope, 2 = var, 4 = a variable's type-name
 * attribute (written by fstWriterCreateVar2 just before the variable
 * it describes), 5 = a source path being named, 6 = the source stem
 * and 7 = the instantiation stem of the scope that follows (written by
 * fstWriterSetSourceStem and fstWriterSetSourceInstantiationStem),
 * 3 = anything else */
int bsc_fsthier_kind(struct fstHier *h)
{
    switch (h->htyp) {
    case FST_HT_SCOPE:   return 0;
    case FST_HT_UPSCOPE: return 1;
    case FST_HT_VAR:     return 2;
    case FST_HT_ATTRBEGIN:
        if (h->u.attr.typ != FST_AT_MISC) return 3;
        switch (h->u.attr.subtype) {
        case FST_MT_SUPVAR:      return 4;
        case FST_MT_PATHNAME:    return 5;
        case FST_MT_SOURCESTEM:  return 6;
        case FST_MT_SOURCEISTEM: return 7;
        default:                 return 3;
        }
    default:             return 3;
    }
}

/* a path-name attribute's number, a stem's line */
unsigned long long bsc_fsthier_attr_arg(struct fstHier *h)
{
    return h->u.attr.arg;
}

/* the path number a stem refers to */
unsigned long long bsc_fsthier_attr_path(struct fstHier *h)
{
    return h->u.attr.arg_from_name;
}

const char *bsc_fsthier_attr_name(struct fstHier *h)
{
    return h->u.attr.name;
}

/* the variable's kind, as VCD spells it */
const char *bsc_fsthier_var_type_name(struct fstHier *h)
{
    switch (h->u.var.typ) {
    case FST_VT_VCD_REG:       return "reg";
    case FST_VT_VCD_WIRE:      return "wire";
    case FST_VT_VCD_PORT:      return "port";
    case FST_VT_VCD_PARAMETER: return "parameter";
    case FST_VT_VCD_INTEGER:   return "integer";
    case FST_VT_VCD_REAL:      return "real";
    case FST_VT_VCD_EVENT:     return "event";
    default:                   return "other";
    }
}

/* NULL when the variable has no recorded direction */
const char *bsc_fsthier_var_direction_name(struct fstHier *h)
{
    switch (h->u.var.direction) {
    case FST_VD_INPUT:  return "input";
    case FST_VD_OUTPUT: return "output";
    case FST_VD_INOUT:  return "inout";
    default:            return NULL;
    }
}

const char *bsc_fsthier_scope_name(struct fstHier *h)
{
    return h->u.scope.name;
}

/* NULL when the scope has no component (module type) recorded */
const char *bsc_fsthier_scope_component(struct fstHier *h)
{
    return (h->u.scope.component_length > 0) ? h->u.scope.component : NULL;
}

const char *bsc_fsthier_var_name(struct fstHier *h)
{
    return h->u.var.name;
}

unsigned bsc_fsthier_var_length(struct fstHier *h)
{
    return h->u.var.length;
}

unsigned bsc_fsthier_var_handle(struct fstHier *h)
{
    return h->u.var.handle;
}

int bsc_fsthier_var_is_alias(struct fstHier *h)
{
    return h->u.var.is_alias;
}
