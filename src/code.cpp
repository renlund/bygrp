#include <Rcpp.h>
using namespace Rcpp;

//' @rdname CppFnc
//' @details contiguous: this function checks if g's unique values are
//'   contiguously arranged
//' @export
// [[Rcpp::export]]
bool contiguous(IntegerVector g, bool error = false) {
  std::unordered_set<int> seen;
  int n = g.size();
  if(n == 0){
    stop("grouping of length 0");
  }
  bool pass = true;
  seen.insert(g[0]);
  for(int i = 1; i<n; i++){
    if(g[i] != g[i-1]){
      if(!seen.insert(g[i]).second){
	if(error) stop("grouping not contiguous");
	pass = false;
	break;
      }
    }
  }
  return pass;
}

template <class V, class E>
V sum_template(V x, IntegerVector g, bool na_rm = false,
	       bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  V y(n);
  int i = 0;
  int G = g[0];
  E S;
  int j;
  if(no_na){
    // this part if one can know that there are no missing values
    while(i < n){
      j = i;
      S = 0;
      while( (i < n) & (g[i] == G) ){
	S = S + x[i];
	i++;
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  } else {
    // this part if there could be missing values
    int counter;
    int na_counter;
    while(i < n){
      j = i;
      S = 0;
      counter = 0;
      na_counter = 0;
      while( (i < n) & (g[i] == G) ){
	counter++;
	if(V::is_na(x[i])){
	  na_counter++;
	} else {
	  S = S + x[i];
	}
	i++;
      }
      if(!na_rm){
	if(na_counter > 0) S = NA_REAL;
      } else {
	if(na_counter == counter){
	  if(na_opt){
	    S = NA_REAL;
	  } else {
	    S = 0; // Note: S would be 0 anyway
	  }
	}
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  }
  return y;
}

//' @rdname CppFnc
//' @details sum_g: sum numeric values by group
//' @export
// [[Rcpp::export]]
NumericVector sum_g (NumericVector x, IntegerVector g, bool na_rm = false,
		     bool na_opt = false, bool no_na = false){
  NumericVector r = sum_template <NumericVector, double> (x, g, na_rm, na_opt, no_na);
  return r;
}

//' @rdname CppFnc
//' @details sumi_g: sum integer values by group
//' @export
// [[Rcpp::export]]
IntegerVector sumi_g (IntegerVector x, IntegerVector g, bool na_rm = false,
		      bool na_opt = false, bool no_na = false){
  IntegerVector r = sum_template <IntegerVector, int> (x, g, na_rm, na_opt, no_na);
  return r;
}

template <class V, class E>
V max_template(V x, IntegerVector g, bool na_rm = false,
	bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  V y(n);
  int i = 0;
  int G = g[0];
  E S;
  int j;
  if(no_na){
    // this part if one can know that there are no missing values
    while(i < n){
      j = i;
      S = x[i];
      i++;
      while( (i < n) & (g[i] == G) ){
	if(x[i] > S) S = x[i];
	i++;
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  } else {
    // this part if there could be missing values
    int counter;
    int na_counter;
    while(i < n){
      j = i;
      S = R_NegInf;
      counter = 0;
      na_counter = 0;
      while( (i < n) & (g[i] == G) ){
	counter++;
	if(V::is_na(x[i])){
	  na_counter++;
	} else {
	  if(x[i] > S) S = x[i];
	}
	i++;
      }
      if(!na_rm){
	if(na_counter > 0) S = NA_REAL;
      } else {
	if(na_counter == counter){
	  if(na_opt){
	    S = NA_REAL;
	  } else {
	    S = R_NegInf; // Note: S would be R_NegInf anyway
	  }
	}
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  }
  return y;
}

//' @rdname CppFnc
//' @details max_g: calculate the maximum numeric value by group
//' @export
// [[Rcpp::export]]
NumericVector max_g (NumericVector x, IntegerVector g, bool na_rm = false,
		     bool na_opt = false, bool no_na = false){
  NumericVector r = max_template <NumericVector, double> (x, g, na_rm, na_opt, no_na);
  return r;
}

//' @rdname CppFnc
//' @details maxi_g: calculate the maximum integer value by group
//' @export
// [[Rcpp::export]]
IntegerVector maxi_g (IntegerVector x, IntegerVector g, bool na_rm = false,
		      bool na_opt = false, bool no_na = false){
  IntegerVector r = max_template <IntegerVector, int> (x, g, na_rm, na_opt, no_na);
  return r;
}

template <class V, class E>
V min_template(V x, IntegerVector g, bool na_rm = false,
	       bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  V y(n);
  int i = 0;
  int G = g[0];
  E S;
  int j;
  if(no_na){
    // this part if one can know that there are no missing values
    while(i < n){
      j = i;
      S = x[i];
      i++;
      while( (i < n) & (g[i] == G) ){
	if(x[i] < S) S = x[i];
	i++;
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  } else {
    // this part if there could be missing values
    int counter;
    int na_counter;
    while(i < n){
      j = i;
      S = max(na_omit(x)); // R_PosInf <-- fails for integers??
      counter = 0;
      na_counter = 0;
      while( (i < n) & (g[i] == G) ){
	counter++;
	if(V::is_na(x[i])){
	  na_counter++;
	} else {
	  if(x[i] < S) S = x[i];
	}
	i++;
      }
      if(!na_rm){
	if(na_counter > 0) S = NA_REAL;
      } else {
	if(na_counter == counter){
	  if(na_opt){
	    S = NA_REAL;
	  } else {
	    S = R_PosInf;
	  }
	}
      }
      for(int k = j; k < i; k++){
	y[k] = S;
      }
      if(i < n) G = g[i];
    }
  }
  return y;
}

//' @rdname CppFnc
//' @details min_g: calculate the minimum numeric value by group
//' @export
// [[Rcpp::export]]
NumericVector min_g (NumericVector x, IntegerVector g, bool na_rm = false,
		     bool na_opt = false, bool no_na = false){
  NumericVector r = min_template <NumericVector, double> (x, g, na_rm, na_opt, no_na);
  return r;
}

//' @rdname CppFnc
//' @details mini_g: calculate the minimum integer value by group
//' @export
// [[Rcpp::export]]
IntegerVector mini_g (IntegerVector x, IntegerVector g, bool na_rm = false,
		      bool na_opt = false, bool no_na = false){
  IntegerVector r = min_template <IntegerVector, int> (x, g, na_rm, na_opt, no_na);
  return r;
}

template <class V, class E>
LogicalVector dup_template(V x, IntegerVector g, bool na_rm){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if(n == 0){
    warning("input of length 0");
    LogicalVector wtf(0);
    return wtf;
  }
  LogicalVector out(n, false);
  std::unordered_set<E> seen;
  int i = 0;
  int j;
  int nas;
  while(i < n){
    seen.insert(x[i]);
    nas = 0;
    j = i;
    i++;
    while( (i < n) & (g[i] == g[j]) ) {
      if(!V::is_na(x[i])){
	out[i] = !seen.insert(x[i]).second;
      } else if(!na_rm){
	if(nas >= 1) out[i] = true;
	nas++;
      }
      i++;
    }
    seen.erase(seen.begin(), seen.end());
  }
  return out;
}

//' @rdname CppFnc
//' @details dup_g: calculate a logical vector indicating duplicated values in x by group
//' @export
// [[Rcpp::export]]
LogicalVector dup_g(SEXP v, IntegerVector g, bool na_rm = false) {
  switch (TYPEOF(v)) {
  case INTSXP:  return dup_template<IntegerVector, int> (v, g, na_rm);
  case REALSXP: return dup_template<NumericVector, double> (v, g, na_rm);
  case STRSXP:  return dup_template<CharacterVector, String> (v, g, na_rm);
  case LGLSXP:  return dup_template<LogicalVector, bool> (v, g, na_rm);
  default: {
    std::cout << "not defined for input " << Rf_type2char(TYPEOF(v)) <<  std::endl;
    return LogicalVector(0);
  }
  }
}

//' @rdname CppFnc
//' @details g_id: character grouping as integers
//' @export
// [[Rcpp::export]]
IntegerVector g_id(CharacterVector gch) {
  int n = gch.size();
  if(n == 0){
    warning("grouping of length 0");
    IntegerVector wtf(0);
    return wtf;
  }
  IntegerVector ans(n);
  int grp = 1;
  ans[0] = grp;
  for (int i=1; i < n; i++) {
    bool same = gch[i]==gch[i-1];
    ans[i] = (grp+=!same);
  }
  return(ans);
}

//' @rdname CppFnc
//' @details g_n: number of observations by group
//' @export
// [[Rcpp::export]]
IntegerVector g_n(IntegerVector g) {
  int n = g.size();
  if(n == 0){
    warning("grouping of length 0");
    IntegerVector wtf(0);
    return wtf;
  }
  IntegerVector one(n, 1);
  IntegerVector r = sumi_g(one, g, false, false, true);
  return r;
}

//' @rdname CppFnc
//' @details g_row: row number by group
//' @export
//[[Rcpp::export]]
IntegerVector g_row (IntegerVector g) {
  int n = g.size();
  if(n == 0){
    warning("grouping of length 0");
    IntegerVector wtf(0);
    return wtf;
  }
  IntegerVector r(n, 1);
  int j = 1;
  for(int i = 1; i < n; i++){
    if( g[i] == g[i-1] ){
      j++;
    } else {
      j = 1;
    }
    r[i] = j;
  }
  return r;
}
