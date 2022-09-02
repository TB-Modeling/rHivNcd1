/*****************************************
 *
 *  HIVNCD 2022
 *  cCoreFunctions.cpp
 *
 ****************************************/

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
void modelHivProg(List &pop,
                  List &mc,
                  List &stats,
                  List &hivInputs,
                  const int tick){
        
        //Get the model constants we will be using
        // const int HIV_NEG=as<int>(mc["HIV.NEG"]);
        // const int HIV_UNDIAG=as<int>(mc["HIV.UNDIAG"]);
        // const int HIV_DIAG_UNSUPP=as<int>(mc["HIV.DIAG_UNSUPP"]);
        // const int HIV_SUPP=as<int>(mc["HIV.SUPP"]);
        
        //STAT COLLECTION #@JP: can we initialize these to 0 and then add them to stats at the end
        int nHiv_inc=0;
        int nHiv_diag=0;
        int nHiv_artInitiation=0;
        int nHiv_artDisengagement=0;
        
        //HIV INPUTS: prop of people experiencing each transitions //@JP: How to collapse input for C++?
        vector<vector<int>> vvInc;//=cReadArray2D_byCol(hivInputs["mat.hiv.initial.prevalence.ratio"]);
        vector<vector<int>> vvDiag;//=cReadArray2D_byCol(hivInputs["XXX"]);
        vector<vector<int>> vvSupp;//=cReadArray2D_byCol(hivInputs["XXX"]);
        vector<vector<int>> vvDiseng;//=cReadArray2D_byCol(hivInputs["XXX"]);
        
        //Get the population size
        const int popsize = pop.size();
        
        //MODEL RISK OF EVENTS AND MARK FOR TRANSITION 
        for (int i=0; i<popsize; i++){
                Environment p=pop[i];
                int hivStatus=as<int>(p["hivState"]);
                switch (hivStatus){
                case 0://HIV_NEG:
                        if (rand() <  vvInc[as<int>(p["sex"])][as<int>(p["agegroup"])] ) {
                                p["bMarkedHivInc"]=true; //@JP: how to manipulate person's attrebutes from C++'
                        }
                        break;
                case 1://HIV_UNDIAG:
                        if (rand()  <  vvDiag[as<int>(p["sex"])][as<int>(p["agegroup"])] ) {
                                p["bMarkedHivDiag"]=true; 
                        }
                        break;
                case 2://HIV_DIAG_UNSUPP:
                        if (rand()  <  vvSupp[as<int>(p["sex"])][as<int>(p["agegroup"])] ) {
                                p["bMarkedHivSupp"]=true; 
                        }
                        break;
                case 3://HIV_SUPP:
                        if (rand()  <  vvDiseng[as<int>(p["sex"])][as<int>(p["agegroup"])] ) {
                                p["bMarkedHivDiseng"]=true; 
                        }
                        break;
                default:
                        cout << "Error: HIV state didnt match any cases";
                }
        }
        
        //MODEL EVENTS THAT ARE MARKED 
        
        
        //UPDATE STATS:
        stats["hiv.inc"]=as<int>(stats["hiv.inc"])+nHiv_inc;
        stats["hiv.diag"]=as<int>(stats["hiv.diag"])+nHiv_diag;
        stats["hiv.art.initiation"]=as<int>(stats["hiv.art.initiation"])+nHiv_artInitiation;
        stats["hiv.disengagement"]=as<int>(stats["hiv.disengagement"])+nHiv_artDisengagement;
        
}

// [[Rcpp::export]]
void modelNcdProg(List &pop,
                  List &mc,
                  List &stats,
                  const int tick){
        
        //GET MODEL CONSTANTS
        
        //STAT COLLECTION
        int nDiab_inc=0;
        int nHyp_inc=0;
        
        //Get the population size
        const int popsize = pop.size();
        
        //MODEL RISK OF EVENTS AND MARK FOR TRANSITION 
        for (int i=0; i<popsize; i++){
                Environment p=pop[i];
        //compute risk of diab and hyp for person p
        }
        
        //MODEL EVENTS THAT ARE MARKED 
        
        
        //UPDATE STATS:
        stats["diab.inc"]=as<int>(stats["diab.inc"])+nDiab_inc;
        stats["hyp.inc"]=as<int>(stats["hyp.inc"])+nHyp_inc;
}

// [[Rcpp::export]]
void modelBirths(List &pop,
                  List &mc,
                  List &stats,
                  const int tick){
}
// [[Rcpp::export]]
void modelDeathsAging(List &pop,
                 List &mc,
                 List &stats,
                 const int tick){
}
