//*****************************************
// *
// *  HIVNCD 2022
// *  cCoreFunctions.cpp
// *
//****************************************/
#include<Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List rcpp_initialize_population(
                int tborn,
                IntegerVector &vIds,
                IntegerVector &vSexes,
                IntegerVector &vAges,
                IntegerVector &vHivState,
                IntegerVector &vNcdState,
                IntegerVector &vtDiabInc,
                IntegerVector &vtHypInc,
                IntegerVector &vtDiabHypInc){
        List res(vIds.size());
        Environment g_env = Environment::global_env();
        Environment p_env = g_env["PERSON"];
        Function new_person = p_env["new"];
        
        for(unsigned int i = 0; i < vIds.size(); i++){
                Environment new_p;
                new_p = new_person(tborn,
                                   vIds[i],
                                       vSexes[i],
                                             vAges[i],
                                                  vHivState[i],
                                                           vNcdState[i],
                                                                    vtDiabInc[i],
                                                                             vtHypInc[i],
                                                                                     vtDiabHypInc[i]);
                res[i] = new_p;
        }
        
        return res;
}
