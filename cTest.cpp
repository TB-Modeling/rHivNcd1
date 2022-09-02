#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

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
