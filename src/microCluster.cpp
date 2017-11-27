#include "microCluster.hpp"
#include <math.h>
#include "Util.hpp"

using namespace Rcpp;
MicroCluster::MicroCluster (Rcpp::List tokens, int time){
  //init weight
  this->weight = 1;
  // set time of last update
  this->time = time;
  //split and tokenize sentence
  this->tf = tokens;
}

MicroCluster::MicroCluster (Rcpp::List tokens, int time, double weight){
  //init weight
  this->weight = weight;
  // set time of last update
  this->time = time;
  //split and tokenize sentence
  this->tf = tokens;
}


void MicroCluster::merge(MicroCluster mc, int t, int omega, double lambda){
  //update weights
  this->fade(t, omega, lambda, this->termFading);
  mc.fade(t, omega, lambda, this->termFading);

  //sum cluster weights
  this->weight = this->weight + mc.weight;

  //sum tokens of both MCs
  Rcpp::StringVector names = this->tf.names();
  Rcpp::StringVector namesMC = mc.tf.names();

  //Union
  Rcpp::StringVector v = Rcpp::union_(names,namesMC);

  Rcpp::List finalList =Rcpp::List::create();

  for(int i = 0;i<v.size();i++){
    Rcpp::String name = v(i);
    double tfVal=0;
    double mctfVal=0;
    if(tf.containsElementNamed(v(i))) tfVal = tf[name];
    if(mc.tf.containsElementNamed(v(i))) mctfVal = mc.tf[name];

    finalList[name]=tfVal + mctfVal;
    // finalList.push_back(Rcpp::Named(name, (int)tf[name] + (int)mc.tf[name]));
  }

  //update time
  this->time = t;
}

void MicroCluster::fade(int tnow, double omega, double lambda, bool termFading){
  //fade cluster
  this->weight = this->weight *  pow(2,(-lambda * (tnow-this->time)));

  //fade tokens
  //Here we fade each single entry in our TF vector according to lambda and the passed time
  //From last index to first one in order to prevent problems with indices
  if(termFading){
    for(int i = tf.size()-1; i>=0;i--){
      double tf = (double)this->tf[i];
      tf = tf  * pow(2,-lambda*(tnow-this->time));
      if(tf<=omega){
        this->tf.erase(i);
      } else {
        this->tf[i] = tf;
      }
    }
  }
  // update time of last update
  this->time = tnow;

}

double MicroCluster::distance(MicroCluster mc,Rcpp::List idf){
  Rcpp::List tfidf = clone(this->tf);
  // Firt we calcualte the TF-IDF of the current element
  Rcpp::StringVector namestf = this->tf.names();
  for(int i =0;i<namestf.size();i++){
    Rcpp::String name = namestf[i];
    if(!idf.containsElementNamed(namestf[i])){
      tfidf[name]=0; // if unknown token, idf is zero
    } else{
      tfidf[name]=(double)this->tf[name]*(double)idf[name];
    }
  }
  // Now we calculate TF-IDF for the second element
  Rcpp::List tfidfMC = clone(mc.tf);
  Rcpp::StringVector namestfMC = mc.tf.names();
  for(int i =0;i<namestfMC.size();i++){
    Rcpp::String name = namestfMC[i];
    if(!idf.containsElementNamed(namestfMC[i])){
      tfidfMC[name]=0; // if unknown token, idf is zero
    } else {
      tfidfMC[name]=(double)mc.tf[name]*(double)idf[name];
    }
  }

  // Now we calculate the Cosine similarity between two micro clusters (This and mc)
  //Get the names
  Rcpp::StringVector namestfidf = tfidf.names();
  Rcpp::StringVector namestfidfMC = tfidfMC.names();

  //Union of the names
  Rcpp::StringVector v = Rcpp::union_(namestfidf,namestfidfMC);

  // Now we calculate the dot product
  Rcpp::List finalList = Rcpp::List::create();

  for(int i = 0;i<v.size();i++){
    Rcpp::String name = v(i);
    // finalList.push_back(Rcpp::Named(name, (int)tfidf[name] * (int)tfidfMC[name]));
    if(!tfidf.containsElementNamed(v(i)) || !tfidfMC.containsElementNamed(v(i))){
      finalList[name]=0; // if one vector does not contain token, dot product is zero at that position
    } else{
      finalList[name]=(double)tfidf[name] * (double)tfidfMC[name];
    }
  }

  double lengthTfIDF = Utility::Util::vectorLengthEuclid(tfidf);
  double lengthTfIDFMC = Utility::Util::vectorLengthEuclid(tfidfMC);

  double sum = 0;
  for(int i=0; i<finalList.size(); i++){
    sum+=(double)finalList[i];
  }
  if(lengthTfIDF == 0 || lengthTfIDFMC == 0){
    return 1; // if length zero set distnace to 1
  } else{
    return (1 - (sum / (lengthTfIDF*lengthTfIDFMC)));
  }

}
