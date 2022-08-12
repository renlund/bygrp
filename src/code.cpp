#include <Rcpp.h>
using namespace Rcpp;

//' @rdname CppFnc
//' @details map character grouping variable into consecutive integer values, while also checking for contiguity
//' @return integer vector
//' @export
// [[Rcpp::export]]
IntegerVector g_char2int(CharacterVector ch, int start = 1) {
  std::unordered_set<String> seen;
  int n = ch.size();
  if(n == 0){
    warning("grouping of length 0");
    IntegerVector wtf(0);
    return wtf;
  }
  IntegerVector id(n, start);
  seen.insert(ch[0]);
  for(int i = 1; i<n; i++){
    if( ch[i] != ch[i-1] ) {
      if( !seen.insert(ch[i]).second ) {
	stop("grouping not contiguous");
      }
      start++;
    }
    id[i] = start;
  }
  return id;
}

//' @rdname CppFnc
//' @details this function checks if x's unique values are contiguously arranged
//' @return a boolean
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

//' @rdname CppFnc
//' @details sum numeric values by group
//' @return numeric vector of same length as x
//' @export
// [[Rcpp::export]]
NumericVector sum_g(NumericVector x, IntegerVector g, bool na_rm = false,
		    bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  NumericVector y(n);
  int i = 0;
  int G = g[0];
  double S;
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
	if(NumericVector::is_na(x[i])){
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
//' @details number of observations by group
//' @return numeric vector
//' @export
// [[Rcpp::export]]
NumericVector n_g(IntegerVector g){
  int n = g.size();
  if(n == 0){
    warning("grouping of length 0");
    NumericVector wtf(0);
    return wtf;
  }
  NumericVector one(n, 1.0);
  NumericVector r = sum_g(one, g, false, false, true);
  return r;
}

//' @rdname CppFnc
//' @details calculate the maximum numeric value by group
//' @return numeric vector of same length as x
//' @export
// [[Rcpp::export]]
NumericVector max_g(NumericVector x, IntegerVector g, bool na_rm = false,
		    bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  NumericVector y(n);
  int i = 0;
  int G = g[0];
  double S;
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
	if(NumericVector::is_na(x[i])){
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
//' @details calculate the minimum numeric value by group
//' @return numeric vector of same length as x
//' @export
// [[Rcpp::export]]
NumericVector min_g(NumericVector x, IntegerVector g, bool na_rm = false,
		    bool na_opt = false, bool no_na = false){
  int n = x.size();
  int m = g.size();
  if( n != m ) stop("grouping not of same length as input");
  if( n == 0 ) {
    warning("input has length 0");
    return x;
  }
  NumericVector y(n);
  int i = 0;
  int G = g[0];
  double S;
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
      S = R_PosInf;
      counter = 0;
      na_counter = 0;
      while( (i < n) & (g[i] == G) ){
	counter++;
	if(NumericVector::is_na(x[i])){
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
	    S = R_PosInf; // Note: S would be R_PosInf anyway
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

// // [[Rcpp::export]]
// LogicalVector dup_g(CharacterVector x, IntegerVector g, bool na_rm = false) {
//   int n = x.size();
//   int m = g.size();
//   if( n != m ) stop("grouping not of same length as input");
//   if(n == 0){
//     warning("input of length 0");
//     LogicalVector wtf(0);
//     return wtf;
//   }
//   LogicalVector out(n, false);
//   std::unordered_set<String> seen;
//   int i = 0;
//   int j;
//   while(i < n){
//     // std::cout << x[i] << std::endl;
//     seen.insert(x[i]);
//     j = i;
//     i++;
//     while( (i < n) & (g[i] == g[j]) ) {
//       if(!CharacterVector::is_na(x[i]) & !na_rm){
// 	out[i] = !seen.insert(x[i]).second;
//       }
//       i++;
//     }
//     seen.erase(seen.begin(), seen.end());
//   }
//   return out;
// }

template <class V, class E>
LogicalVector dup_template(V x, IntegerVector g, bool na_rm = false){
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
  while(i < n){
    // std::cout << x[i] << std::endl;
    seen.insert(x[i]);
    j = i;
    i++;
    while( (i < n) & (g[i] == g[j]) ) {
      if(!V::is_na(x[i]) & !na_rm){
	out[i] = !seen.insert(x[i]).second;
      }
      i++;
    }
    seen.erase(seen.begin(), seen.end());
  }
  return out;
}

//' @rdname CppFnc
//' @details check for duplicated values in x within values of g
//' @return boolean vector
//' @export
// [[Rcpp::export]]
LogicalVector dup_g(SEXP x) {
  switch (TYPEOF(x)) {
    case INTSXP:  return dup_template<IntegerVector, int> (x);
    case REALSXP: return dup_template<NumericVector, double> (x);
    case STRSXP:  return dup_template<CharacterVector, String> (x);
    case LGLSXP:  return dup_template<LogicalVector, bool> (x);
    default: {
      std::cout << "not defined for input (" << TYPEOF(x) << ")" << std::endl;
      return LogicalVector(0);
    }
  }
}
