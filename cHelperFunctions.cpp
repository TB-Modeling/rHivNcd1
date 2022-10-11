/*****************************************
 *
 *  HIVNCD 2022
 *  cHelperFunctions.cpp
 *
 ****************************************/
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;



//Global variables to use in this code:
//we can either pass &mc as an input to everyfunction or just specify the variables that we need here
int NUM_SEXES=2;
int NUM_AGE_GROUPS=17;
int NUM_HIV_STATES=4;
int NUM_NCD_STATES=4;


//Error messages
string errAgeDist = "Error:agegroup is greter than max agegroup";

// [[Rcpp::export]]
vector<double> cReturnAgDist(List &pop){
  vector<double> res(NUM_AGE_GROUPS,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    if (as<int>(p["agegroup"])>NUM_AGE_GROUPS) throw logic_error(errAgeDist); //@JP: this doesnt really work
    else{
      res[as<int>(p["agegroup"])-1]++; 
    }}
  return(res);
}

// [[Rcpp::export]]
vector<double> cReturnSex(List &pop){
  vector<double> res(NUM_SEXES,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    res[as<int>(p["sex"])-1]++; 
  }
  return(res);
}

// [[Rcpp::export]]
vector<vector<double>> cReturnSexAgeDist(List &pop){
  vector<vector<double>> res(2,vector<double>(NUM_AGE_GROUPS,0));
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    // if (as<int>(p["agegroup"])>NUM_AGE_GROUPS) throw logic_error(errAgeDist); //@JP: this doesnt really work
    // else{
    res[as<int>(p["sex"])-1][as<int>(p["agegroup"])-1]++; 
  }
  // }
  return(res);
}

// [[Rcpp::export]]
vector<double> cReturnHivStates(List &pop){
  vector<double> res(NUM_HIV_STATES,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    res[as<int>(p["hivState"])-1]++; 
  }
  return(res);
}

// [[Rcpp::export]]
vector<double> cReturnNcdStates(List &pop){
  vector<double> res(NUM_NCD_STATES,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    res[as<int>(p["ncdState"])-1]++; 
  }
  return(res);
}

// [[Rcpp::export]]
vector<vector<double>> cReturnHivNcdStates(List &pop){
  vector<vector<double>> res(NUM_HIV_STATES,vector<double> (NUM_NCD_STATES,0));
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    res[as<int>(p["hivState"])-1][as<int>(p["ncdState"])-1]++; 
  }
  return(res);
}




//---------------
// [[Rcpp::export]]
void testCube( arma::cube &a) {
  //cube is a 3D obj
  Rprintf("Array Passed\n");
  const auto slice_size = a.n_slices;
  const auto row_size = a.n_rows;
  const auto col_size = a.n_cols;
  
  Rcout << slice_size << " " << row_size << " " << col_size << " " << "\n";
  
  for (int i = 0; i < slice_size; i++) {
    const auto slice = a.slice(i);
    
    Rcout << "New Slice : " << std::endl;
    for (int j = 0; j < col_size; j++) {
      
      for (int k = 0; k < row_size; k++ ) {
        Rcout << std::setw(5) << slice(k,j); //setw puts the output in a fixed width field
      }
      Rcout << std::endl;
    }
  }
}