#include "microCluster.hpp"
#include <math.h>
#include <unordered_map>

using namespace Rcpp;



MicroCluster::MicroCluster (Rcpp::List tokens, int time, double weight){
  //init weight
  this->weight = weight;
  // set time of last update
  this->time = time;
  // write list of tokens into hash map
  Rcpp::StringVector names = tokens.names();
  for(int i=0; i<names.size(); i++){
    Rcpp::String name = names(i);
    this->tf[name] = tokens[name];
  }

}

Rcpp::List MicroCluster::getTf(){
  Rcpp::List result;
  for (std::unordered_map<std::string, double>::iterator it = this->tf.begin(); it != this->tf.end(); it++ ){
    result[it->first] = it->second;
  }
  return(result);
}

void MicroCluster::unmerge(MicroCluster* mc, int t, int omega, double lambda, bool termFading){
  //update weights
  this->fade(t, omega, lambda, termFading);
  mc->fade(t, omega, lambda, termFading);

  this->time = t;

  //subtract cluster weights
  this->weight = this->weight - mc->weight;

  // iterate tokens of mc
  for (std::unordered_map<std::string, double>::iterator it = mc->tf.begin(); it != mc->tf.end(); it++ ){
    // search key in current map (this)
    std::unordered_map<std::string, double>::iterator valIt = this->tf.find(it->first);
    if ( valIt != this->tf.end() ) {
      // if element found: subtract elements
      valIt->second -= it->second;
      // remove insufficient elements
      if(valIt->second <= omega){
        this->tf.erase(valIt);
      }
    }
  }
}

void MicroCluster::merge(MicroCluster* mc, int t, int omega, double lambda, bool termFading){

  //update weights
  this->fade(t, omega, lambda, termFading);
  mc->fade(t, omega, lambda, termFading);

  this->time = t;

  //sum cluster weights
  this->weight = this->weight + mc->weight;


  // iterate tokens of mc
  for (std::unordered_map<std::string, double>::iterator it = mc->tf.begin(); it != mc->tf.end(); it++ ){
    // search key in current map (this)
    std::unordered_map<std::string, double>::iterator valIt = this->tf.find(it->first);
    if ( valIt == this->tf.end() ) {
      // if element not found: insert element
      this->tf[it->first] = it->second;
    } else {
      // if element found: sum elements
      valIt->second += it->second;
    }
  }


}


void MicroCluster::fade(int tnow, double omega, double lambda, bool termFading){

  //fade cluster
  this->weight = this->weight * pow(2,(-lambda * (tnow-this->time)));

  //fade tokens
  if(termFading){

    // iterate all tokens
    std::unordered_map<std::string, double>::iterator it = this->tf.begin();
    while(it != this->tf.end()){
      // fade entry
      it->second = it->second * pow(2,-lambda*(tnow-this->time));
      if(it->second <= omega){
        // if weight below threshold: remove
        it = this->tf.erase(it);
      } else{
        it++;
      }
    }

  }

  // update time of last update
  this->time = tnow;

}


double MicroCluster::distance(MicroCluster* mc, std::unordered_map<std::string, double> idf){

  double sum = 0.0;
  double tfidfLength = 0.0;
  double MCtfidfLength = 0.0;

  // iterate tokens of tf
  for (std::unordered_map<std::string, double>::iterator tfIt = this->tf.begin(); tfIt != this->tf.end(); tfIt++ ){

    // check if idf is available
    std::unordered_map<std::string, double>::iterator idfIt = idf.find(tfIt->first);
    if(idfIt != idf.end()){

      // search key in other map
      std::unordered_map<std::string, double>::iterator mcIt = mc->tf.find(tfIt->first);
      if(mcIt != mc->tf.end()){

        // if idf available: calculate dot product
        sum += (tfIt->second * idfIt->second) * (mcIt->second * idfIt->second);

      }

      // while looping we can also calculate the vector length
      tfidfLength += (tfIt->second * idfIt->second) * (tfIt->second * idfIt->second);
    }




  }
  tfidfLength = sqrt(tfidfLength);

  // loop to calculate the other vector length
  for (std::unordered_map<std::string, double>::iterator mcIt = mc->tf.begin(); mcIt != mc->tf.end(); mcIt++ ){
    // if also in other map, check if idf also available
    std::unordered_map<std::string, double>::iterator idfIt = idf.find(mcIt->first);
    if(idfIt != idf.end()){
      MCtfidfLength += (mcIt->second * idfIt->second) * (mcIt->second * idfIt->second);
    }
  }
  MCtfidfLength = sqrt(MCtfidfLength);

  // finally calculate cosine similarity
  if(tfidfLength == 0 || MCtfidfLength == 0){
    return 1; // if length zero set distance to 1
  } else{
    return (1 - (sum / (tfidfLength*MCtfidfLength)));
  }


}
