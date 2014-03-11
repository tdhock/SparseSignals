/* -*- compile-command: "gcc main.c SparseSignals.c -o test && ./test" -*- */
#include <stdio.h>
#include <stdlib.h>
#include "SparseSignals.h"
int main(void){
    int x_first[]={10,  150};
    int x_after[]={100, 200};
    int y_first[]={10, 190};
    int y_after[]={160, 250};
    double x_value[]={5.5, 5.5};
    double y_value[]={-5.5, 10};
    int x_size=2, y_size=2, i;
    int z_size_max = x_size + y_size;
    int    *z_first = (int*)    malloc(z_size_max * sizeof(int));
    int    *z_after = (int*)    malloc(z_size_max * sizeof(int));
    double *z_value = (double*) malloc(z_size_max * sizeof(double));
    int z_size = add_SparseSignals(
	x_first, x_after, x_value, x_size,
	y_first, y_after, y_value, y_size,
	z_first, z_after, z_value);
    for(i=0; i<z_size;i++){
	printf("%4d %4d %f\n", z_first[i], z_after[i], z_value[i]);
    }
    return 0;
}
