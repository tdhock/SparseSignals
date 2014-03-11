#include <stdio.h>
#include "SparseSignals.h"

int add_SparseSignals(
    int *x_first, int *x_after, double *x_value, int x_size,
    int *y_first, int *y_after, double *y_value, int y_size,
    int *z_first, int *z_after, double *z_value){
    int x_start=1, x_pos, y_start=1, y_pos, z_size=0, e_pos,
	last_pos=0, new_first, new_after, last_x_pos=0, last_y_pos=0;
    double e_value, value=0;
    while(x_size || y_size){
	if(x_start){
	    x_pos = *x_first;
	}else{
	    x_pos = *x_after;
	}
	if(y_start){
	    y_pos = *y_first;
	}else{
	    y_pos = *y_after;
	}
	// make sure they are never the smallest if there are no more 
	// data left.
	if(x_size == 0){
	    x_pos = y_pos + 1;
	}
	if(y_size == 0){
	    y_pos = x_pos + 1;
	}
	if(x_pos < last_x_pos || y_pos < last_y_pos){
	    /* printf("should be x %d >= %d, y %d >= %d\n",  */
	    /* 	   x_pos, last_x_pos, y_pos, last_y_pos); */
	    return ERROR_POSITIONS_DECREASING;
	}
	last_y_pos = y_pos;
	last_x_pos = x_pos;
	/* printf("x_size=%d y_size=%d x_pos=%d y_pos=%d\n",  */
	/*        x_size, y_size, x_pos, y_pos); */
	if(x_pos < y_pos){
	    e_pos = x_pos;
	    if(x_start){
		e_value = *x_value;
		x_first++;
		x_start = 0;
	    }else{
		e_value = - *x_value;
		x_after++;
		x_start = 1;
		x_size--;
		x_value++;
	    }
	}else{
	    e_pos = y_pos;
	    if(y_start){
		e_value = *y_value;
		y_first++;
		y_start = 0;
	    }else{
		e_value = - *y_value;
		y_after++;
		y_start = 1;
		y_size--;
		y_value++;
	    }
	}
	//printf("%10d=last_pos %10d=e_pos %10.2f=value %10.2f=e_value\n", 
	//     last_pos, e_pos, value, e_value);
	if(last_pos != e_pos){
	    if(value != 0){
		//printf("%10.2f=value pos change %5d to %5d\n", 
		//     value, last_pos, e_pos);
		z_value[z_size] = value;
		z_first[z_size] = last_pos;
		z_after[z_size] = e_pos;
		z_size++;
	    }
	}
	value += e_value;
	last_pos = e_pos;
    }
    return z_size;
}
