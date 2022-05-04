#include <R.h>
#include <Rinternals.h>
#include <strings.h>

// x and y are positions of lines that need to be drawn
// The function finds y positions that allow this
// Note that for each pair of x1 and x2, x1 must be smaller
// than x1
SEXP arrange_lines(SEXP r_x1, SEXP r_x2){
  if(!isReal(r_x1) || !isReal(r_x2))
    error("Both arguments should be real values\n");
  if(length(r_x1) != length(r_x2) || length(r_x1) == 0)
    error("Arguments should have the same non-zero length\n");
  int n = length(r_x1);
  double *x1 = REAL(r_x1);
  double *x2 = REAL(r_x2);
  // assign a vector of ints..
  SEXP r_y = PROTECT(allocVector(INTSXP, n));
  int *y = INTEGER(r_y);
  // use a char array as a boolean vector. It's ugly, but
  // it seems to work. We probably have a bit of a slow
  // down due to the repeated calls to bzero. There should
  // be a better way of handling it... We could easily use
  // bit bucket as well, but that's probably more trouble than it's
  // worth.
  unsigned char *forbidden_y = malloc(n); // use as a boolean vector 
  bzero((void*)y, sizeof(int) * n);
  bzero((void*)forbidden_y, n);
  for(int i=1; i < n; ++i){
    int max_y = 0;
    for(int j=0; j < i; ++j){
      if( x2[j] >= x1[i] && x2[i] >= x1[j]){
	forbidden_y[y[j]] = 1;
	max_y = y[j] > max_y ? y[j] : max_y;
      }
    }
    for(int j=0; j < i; ++j){
      if(!forbidden_y[j]){
	y[i] = j;
	break;
      }
      // if no empty slot found, set to position to i
      y[i] = (y[i] == 0) ? i : y[i];
    }
    bzero((void*)forbidden_y, n);
  }
  free(forbidden_y);
  UNPROTECT(1);
  return(r_y);
}
