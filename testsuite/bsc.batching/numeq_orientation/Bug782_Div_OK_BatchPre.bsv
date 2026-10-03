// Mentions the type-variable names of Bug782_Div_OK_Batch in reverse
// textual order.  The -u dependency scan parses imports in the order the
// root names them, so with this package imported first those names are
// interned in this order before Bug782_Div_OK_Batch is parsed.  The NumEq
// orientation in genNumEqInsts used to follow intern order, and in this
// order a module that typechecks on its own failed with T0030.

Integer int_ex = 0;
Integer fra_q = 0;
Integer int_q = 0;
Integer fra_d = 0;
Integer int_d = 0;
Integer fra_n = 0;
Integer int_n = 0;
