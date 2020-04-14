#include <R.h>
#include <Rinternals.h>


// a and b are matrices containing beg and end coordinates
// The begin part of a must be sorted from beginning to end 
// This determines which elements of a
// overlap with elements in b
// are contained by elements in b
// contain elements in b
// returning a single integer vector of bitwise flags
// This should guarantee all overlaps
// but it does not report the relative locations of all
// overlaps. 
SEXP range_overlap(SEXP a_r, SEXP b_r){
  if(!isReal(a_r) || !isReal(b_r))
    error("Both arguments should be real vectors");
  
  SEXP a_dim_r = getAttrib(a_r, R_DimSymbol);
  SEXP b_dim_r = getAttrib(b_r, R_DimSymbol);
  if( length(a_dim_r) != 2 || length(b_dim_r) != 2 )
    error("Both arguments should be matrices");

  int *a_dim = INTEGER(a_dim_r);
  int *b_dim = INTEGER(b_dim_r);

  double *a_beg = REAL( a_r );
  double *a_end = a_beg + a_dim[0];

  double *b_beg = REAL( b_r );
  double *b_end = b_beg + b_dim[0];

  // There is an R function to determine whether an R object is sorted,
  // But I do not know what that means;

  // the indices
  int a_i = 0;
  int b_i = 0;
  
  SEXP overlaps_r = PROTECT(allocMatrix( INTSXP, a_dim[0], 3 ));
  int *overlaps = INTEGER( overlaps_r );
  int *ol_i = overlaps + a_dim[0];
  int *ol_n = overlaps + 2 * a_dim[0];
  memset( (void*)overlaps, 0, sizeof(int) * a_dim[0] * 3);

  // let us set the colnames:
  const char *colnames[3] = {"flag", "i", "n"};
  SEXP dimnames_r = PROTECT( allocVector(VECSXP, 2) );
  SEXP colnames_r = PROTECT( allocVector(STRSXP, 3) );
  for(int i=0; i < 3; i++)
    SET_STRING_ELT( colnames_r, i, mkChar( colnames[i] ) );
  SET_VECTOR_ELT( dimnames_r, 1, colnames_r );
  setAttrib( overlaps_r, R_DimNamesSymbol, dimnames_r );

  // increment the lower
  // requires care to avoid infinite loops;
  while(a_i < a_dim[0] && b_i < b_dim[0]){
    // is there any overlap between the two
    for(int j=b_i; j < b_dim[0] && b_beg[j] < a_end[a_i]; ++j){
      int olap = (b_end[j] >= a_beg[a_i] && a_end[a_i] >= b_beg[j]) ? 1 : 0;
      int a_contained = (b_end[j] >= a_end[a_i] && b_beg[j] <= a_beg[a_i]) ? 2 : 0;
      int b_contained = (a_end[a_i] >= b_end[j] && a_beg[a_i] <= b_beg[j]) ? 4 : 0;
      overlaps[a_i] |= (olap | a_contained | b_contained);
      // set the starting index by subtracting the number of overlapping
      // ranges.
      if(olap){
	ol_i[a_i] = j + 1 - ol_n[a_i];
	++ol_n[a_i];
      }
    }
    ++a_i;
    if( b_end[b_i] < a_beg[a_i] )
      ++b_i;
  }
  UNPROTECT(3);
  return( overlaps_r );
}
