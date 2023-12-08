#include <Rcpp.h>
using namespace Rcpp;

CharacterVector get_last_letter(CharacterVector x) {
  CharacterVector out(x.size());
  for (auto i = 0; i < x.size(); i++) {
    std::string s = as<std::string>(x(i));
    out(i) = s.substr(2, 2);
  }
  return out;
}

IntegerVector whichcpp(LogicalVector x) {
  IntegerVector out = Rcpp::seq(0, x.size() - 1); 
  return out[x]; 
}

// [[Rcpp::export]]
int aoc(
    CharacterMatrix node_names, 
    CharacterVector instructions, 
    IntegerVector idx_starting_node, 
    int version = 1
  ) {
  // Initialisations
  int count = 0;
  int idx_instruction = 0; 
  int instruction_pos = 1; 
  String instruction;
  CharacterVector current_node; 
  CharacterVector next_node; 
  
  // Processing the idx of starting nodes 
  // CharacterVector ll = get_last_letter(node_names(_, 0)); 
  // IntegerVector idx_starting_nodes = whichcpp(equal_to_letter(ll, "A")); 
  
  // Rcout << "Idx of the starting nodes (-1) is: " << idx_starting_nodes << "\n";
  
  while (true) {
    if (count % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    
    count++; 
    // Rcout << "Count is equal to " << count << "\n"; 
    idx_instruction = count % instructions.size() - 1; 
    if (idx_instruction == -1) {
      idx_instruction = 1; 
    }
    instruction = instructions(idx_instruction);
    
    if (instruction == "L") {
      instruction_pos = 1; 
    } else {
      instruction_pos = 2; 
    }
    
    // Rcout << "The instruction position is " << instruction_pos << "\n";
    
    current_node = node_names(_, 0);
    current_node = current_node[idx_starting_node];  
    
    // Rcout << "The current node is " << current_node << "\n"; 
    
    next_node = node_names(_, instruction_pos); 
    next_node = next_node[idx_starting_node]; 
    
    if (version == 1) {
      CharacterVector target_node = CharacterVector("ZZZ");
      if (as<std::string>(next_node) == as<std::string>(target_node)) {
        break; 
      }
    } else if (version == 2) {
      CharacterVector target_node = CharacterVector("Z"); 
      if (as<std::string>(get_last_letter(next_node)) == as<std::string>(target_node)) {
        break; 
      }
    }
     
    // Update starting nodes
    LogicalVector aasd = in(node_names(_, 0), next_node);
    idx_starting_node = whichcpp(aasd); 
  }
  
  return count;
}
