#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP posterior_ps_tail_select(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"posterior_ps_tail_select", (DL_FUNC) &posterior_ps_tail_select, 2},
    {NULL, NULL, 0}
};

void R_init_posterior(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
