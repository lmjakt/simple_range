#include <R.h>
#include <Rinternals.h>

// 1 : not sorted
// 0 : sorted
int is_unsorted(double *v, size_t l){
  for(size_t i=1; i < l; ++i){
    if( v[i] < v[i-1] ){
      Rprintf("%f < %f at position %d and %d\n", v[i], v[i-1], i, i+1);
      return(1);
    }
  }
  return(0);
}

// a and b are matrices containing beg and end coordinates
// The begin part of a must be sorted from beginning to end 
// This function determines which elements of a:
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
  
  SEXP a_dim_r = PROTECT(getAttrib(a_r, R_DimSymbol));
  SEXP b_dim_r = PROTECT(getAttrib(b_r, R_DimSymbol));
  if( length(a_dim_r) != 2 || length(b_dim_r) != 2 ){
    UNPROTECT(2);
    error("Both arguments should be matrices");
  }

  int *a_dim = INTEGER(a_dim_r);
  int *b_dim = INTEGER(b_dim_r);

  double *a_beg = REAL( a_r );
  double *a_end = a_beg + a_dim[0];

  double *b_beg = REAL( b_r );
  double *b_end = b_beg + b_dim[0];

  // There is an R function to determine whether an R object is sorted,
  // But I have not found the documentation for it;
  // so using my own above:
  // do I need to UNPROTECT here?
  if( is_unsorted( a_beg, a_dim[0] ) ||
      is_unsorted( b_beg, b_dim[0] ) )
    error("Range begins must be sorted");
  

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
    // This used to be a single if statement giving no more than a single
    // increment of b_i for each iteration of a_i. That seems wasteful, especially when
    // a is sparse compared t b. Change to a while loop
    while( b_end[b_i] < a_beg[a_i] && b_i < b_dim[0] )
      ++b_i;
  }
  UNPROTECT(5);
  return( overlaps_r );
}

// Returns the indices of the first range where end >= pos
// and the last range where beg <= pos
// as as matrix with n columns and 2 rows
// Note that a single index will be returned for each point
// even if it falls within several overlapping ranges.
// The following must all be TRUE
// ranges_r: a 2 column matrix giving the begin and end coordinates
//           of ranges.
//           end > begin
// ranges_r must be sorted and should only have unique coordinates (i.e.
// cannot have coordinates from several chromosomes unless these have been
// adjusted by adding cumulative offsets to each chromosome (but don't do this
// unless you have a really good reason for doing it.
// points_r: a sorted range of points
// All values must be 64 bit REALs.
// DOES NOT DO THE LOGICAL THING WITH OVERLAPPING RANGES; REPORTS ONLY THE FIRST ONE!
SEXP points_in_ranges(SEXP ranges_r, SEXP points_r){
  if(!isReal(ranges_r) || !isReal(points_r))
    error("Both arguments should be real vectors");
  
  SEXP ranges_dim_r = PROTECT(getAttrib(ranges_r, R_DimSymbol));
  if( length(ranges_dim_r) != 2 ){
    UNPROTECT(1);
    error("ranges_r should be a matrix");
  }
  int *ranges_dim = INTEGER(ranges_dim_r);
  if(ranges_dim[0] < 1 || ranges_dim[1] != 2){
    UNPROTECT(1);
    error("ranges_r should be a matrix with 2 columns and at least one row");
  }
  int point_n = length(points_r);
  if(point_n < 1){
    UNPROTECT(1);
    error("you should specify at least one point");
  }
  double *r_beg = REAL(ranges_r);
  double *r_end = r_beg + ranges_dim[0];
  double *points = REAL(points_r);

  if( is_unsorted(r_beg, ranges_dim[0]) || is_unsorted(points, point_n) ){
    UNPROTECT(1);
    error("both the ranges and the points must be sorted");
  }

  SEXP point_i_r = PROTECT(allocMatrix(INTSXP, point_n, 2));
  int *point_b = INTEGER(point_i_r);
  int *point_e = point_b + point_n;

  int j = 0;  // the range index
  for(int i=0; i < point_n; ++i){
    while(j < ranges_dim[0] && r_end[j] < points[i])
      ++j;

    point_e[i] = j+1;
    int k = j;
    while(k > 0 && r_beg[k] > points[i])
      --k;

    point_b[i] = r_beg[k] < points[i] ? k+1 : k;
    j = k;
  }
  UNPROTECT(2);
  return(point_i_r);
}
