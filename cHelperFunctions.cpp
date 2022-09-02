/*****************************************
 *
 *  HIVNCD 2022
 *  cHelperFunctions.cpp
 *
 ****************************************/

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//Global variables to use in this code:
const int NUM_HIV_STATES=4;
const int NUM_NCD_STATES=4;
const int NUM_AGE_GROUPS=18;

//Error messages
string errAgeDist = "Error:agegroup is greter than max agegroup";

// [[Rcpp::export]]
vector<double> cReturnHivStates(List &pop){
  vector<double> res(NUM_HIV_STATES,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    res[as<int>(p["hivState"])]++; //@JP: how do we deal with null values if there is a problem?
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
    res[as<int>(p["ncdState"])]++; 
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
    res[as<int>(p["hivState"])][as<int>(p["ncdState"])]++; 
  }
  return(res);
}
// [[Rcpp::export]]
vector<double> cReturnAgeDist(List &pop){
  vector<double> res(NUM_AGE_GROUPS,0);
  const int popsize = pop.size();
  //Loop through the population
  for (int i = 0; i < popsize; i++) {
    Environment p = pop[i];
    if (as<int>(p["agegroup"])>NUM_AGE_GROUPS) throw logic_error(errAgeDist); //@JP: this doesnt really work
    else{
    res[as<int>(p["agegroup"])]++; 
    }}
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
      res[as<int>(p["sex"])][as<int>(p["agegroup"])]++; 
    }
  // }
  return(res);
}

//---------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
void cPrintVector2D(vector<vector<double>> v){
  for(int i=0;i<v.size();i++){
    for(int j=0; j<v[1].size();j++ ){
      Rcout<<v[i][j]<<" ";
    }
    Rcout << std::endl;
  }
}
// [[Rcpp::export]]
void cPrintVector3D(vector<vector<vector<double>>> v){
  for(int k=0;k<v[1][1].size();k++){
    Rcout<<k<<" ..."<<endl;
    for(int i=0;i<v.size();i++){
    for(int j=0; j<v[1].size();j++ ){
        Rcout<<v[i][j][k]<<" ";
    }
    Rcout << std::endl;
  }
  }
}

// [[Rcpp::export]]
vector<vector<double>> cReadArray2D_byCol( NumericVector &a, const IntegerVector &dim ) {
  const auto array_size = a.size();
  const auto dim_size = dim.size();
  Rcout<<"Array Passed: array size= "<<array_size<<" dim size= "<< dim_size<< "\n";
  Rcout <<"Dims= "<< dim[0] << " " << dim[1] << "\n";
  //
  if (dim_size != 2) {
    throw std::invalid_argument("This is meant to work with 2D data");
  }

  int dim1=dim[0];
  int dim2=dim[1];
  vector<vector<double>> v(dim1,vector<double> (dim2,0) );
  // printVector2D(v);

  for (int j=1; j<=dim2;j++ ) {
    for (int i=1; i<=dim1;i++ ) {
      int id= (j-1)*(dim1) +i-1 ;
      // Rcout<<i<<" "<<j<<" "<<id <<" "<<a[id]<< "\n";
      v[i-1][j-1]=a[id];
      }}

  cPrintVector2D(v);
  return v;
}

// [[Rcpp::export]]
vector<vector<vector<double>>> cReadArray3D_byCol( NumericVector &a, const IntegerVector &dim ) {
  const auto array_size = a.size();
  const auto dim_size = dim.size();
  Rcout<<"Array Passed: array size= "<<array_size<<" dim size= "<< dim_size<< "\n";
        Rcout <<"Dims= "<< dim[0] << " " << dim[1] << " " << dim[2] << "\n";
    //
    if (dim_size != 3) {
      throw std::invalid_argument("This is meant to work with 3D data");
    }
    
    int dim1=dim[0];
    int dim2=dim[1];
    int dim3=dim[2];
    vector<vector<vector<double>>> v(dim1,vector<vector<double>> (dim2,vector<double> (dim1,0)) );
    // printVector2D(v);
    
    for (int k=1; k<=dim3;k++ ) {
      for (int j=1; j<=dim2;j++ ) {
      for (int i=1; i<=dim1;i++ ) {
        int id= (k-1)*(dim1*dim2)+(j-1)*(dim1) +i  -1 ; //-1 is to turn index 1 to 0
        // Rcout<<i<<" "<<j<<" "<<id <<" "<<a[id]<< "\n";
        v[i-1][j-1][k-1]=a[id];
      }}}
    
    cPrintVector3D(v);
 return v;
}

