#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
  /* .C calls */
  extern void calcSE(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"calcSE", (DL_FUNC) &calcSE, 11},
  {NULL, NULL, 0}
};

void R_init_LRE(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}