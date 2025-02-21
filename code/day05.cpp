// We can now use the BH package
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
// #include <string.h>
// #include <boost/integer/common_factor.hpp>
// #include <boost/lexical_cast.hpp>  	// one file, automatically found for me

using namespace Rcpp;
// using boost::lexical_cast;
// using boost::bad_lexical_cast;

// [[Rcpp::export]]
LogicalVector is_between(NumericVector x, double min, double max) {
  LogicalVector out (x.size()); 
  for (auto i = 0; i < x.size(); ++i) {
    out[i] = x[i] >= min && x[i] <= max; 
  }
  return out; 
}

// NumericVector get_idx_map(
//     NumericVector input, 
//     LogicalVector betw, 
//     double min_names
// ) {
//   NumericVector input_betw = input[betw]; 
//   for (auto i = 0; i < input_betw.size(); ++i) {
//     input_betw[i] = input_betw[i] - min_names + 1; 
//   }
//   return input_betw; 
// }

// // [[Rcpp::export]]
// NumericVector cppmatch(NumericVector input, List maps) {
//   // Initialisations
//   NumericVector out (input.size()); 
//   
//   for (auto i = 0; i < maps.length(); i++) {
//     Rcout << "Entering the loop with i = " << i << "\n"; 
//     NumericVector map = maps[i]; 
//     Rcout << "Map is equal to " << map[0] << "\n";
//     CharacterVector map_names = map.names(); 
//     Rcout << "Map name is equal to " << map_names[0] << "\n"; 
//     double min_names = lexical_cast<double>(as<std::string>(map_names[0]));
//     Rcout << "Min name is equal to " << min_names << "\n"; 
//     double max_names = lexical_cast<double>(as<std::string>(map_names[map.size() - 1]));
//     Rcout << "Max name is equal to " << min_names << max_names << "\n"; 
//     LogicalVector betw = is_between(input, min_names, max_names); 
//     if (is_false(any(betw))) {
//       continue; 
//     }
//     NumericVector idx_map = get_idx_map(input, betw, min_names);  
//     out[betw] = map[idx_map]; 
//   }
//   
//   return out;
// }
