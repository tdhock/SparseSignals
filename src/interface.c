/* -*- compile-command: "R CMD INSTALL .. && R CMD check .." -*- */
#include <R.h>
#include <Rinternals.h>
#include "SparseSignals.h"

SEXP 
add_SparseSignals_interface
(SEXP x, SEXP y){
    SEXP z, class, names, first, after, value;
    int    *x_first = INTEGER(VECTOR_ELT(x, 0));
    int    *x_after = INTEGER(VECTOR_ELT(x, 1));
    double *x_value =    REAL(VECTOR_ELT(x, 2));
    int    *y_first = INTEGER(VECTOR_ELT(y, 0));
    int    *y_after = INTEGER(VECTOR_ELT(y, 1));
    double *y_value =    REAL(VECTOR_ELT(y, 2));

    int x_size = length(VECTOR_ELT(x, 0)), y_size = length(VECTOR_ELT(y, 0));
    int z_size_max = (x_size + y_size)*2;
    int    *z_first = (int*)    R_alloc(z_size_max, sizeof(int));
    int    *z_after = (int*)    R_alloc(z_size_max, sizeof(int));
    double *z_value = (double*) R_alloc(z_size_max, sizeof(double));
    //Rprintf("z_size_max=%d\n", z_size_max);
    int z_size = add_SparseSignals(
	x_first, x_after, x_value, x_size,
	y_first, y_after, y_value, y_size,
	z_first, z_after, z_value);
    //Rprintf("z_size=%d\n", z_size);
    if(z_size == ERROR_POSITIONS_DECREASING)error("positions decreasing");
    if(z_size < 0)error("undefined error code %d", z_size);

    PROTECT(z = allocVector(VECSXP, 3));

    PROTECT(names = allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("first"));
    SET_STRING_ELT(names, 1, mkChar("after"));
    SET_STRING_ELT(names, 2, mkChar("value"));
    namesgets(z, names);

    PROTECT(first = allocVector(INTSXP, z_size));
    PROTECT(after = allocVector(INTSXP, z_size));
    PROTECT(value = allocVector(REALSXP, z_size));
    for(int i=0; i<z_size; i++){
	//Rprintf("%d %d %f\n", z_first[i], z_after[i], z_value[i]);
	INTEGER(first)[i]  = z_first[i];
	INTEGER(after)[i]  = z_after[i];
	REAL(value)[i] = z_value[i];
    }
    SET_VECTOR_ELT(z, 0, first);
    SET_VECTOR_ELT(z, 1, after);
    SET_VECTOR_ELT(z, 2, value);

    UNPROTECT(5);
    return z;
}
