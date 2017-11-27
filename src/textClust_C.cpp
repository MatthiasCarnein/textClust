#include <Rcpp.h>
#include "microCluster.hpp"

using namespace Rcpp;

class textClust {
public:

  double r;
  double lambda;
  int tgap;
  bool updateAll;
  bool verbose;
  double omega;
  int t;
  bool upToDate;
  bool termFading;
  Rcpp::List micro;


  textClust(double r_, double lambda_, int tgap_, bool updateAll_, bool verbose_, bool termFading_) :
    r(r_),
    lambda(lambda_),
    tgap(tgap_),
    updateAll(updateAll_),
    verbose(verbose_) {
    omega = pow(2, (-1*lambda * tgap));
    t = 0;
    upToDate=1;
    termFading = termFading_;
  };

  textClust(Rcpp::List serialized) {

    // reconstruct micro clusters
    Rcpp::List tfs = serialized["micro_tfs"];
    Rcpp::IntegerVector times = serialized["micro_times"];
    Rcpp::NumericVector weights = serialized["micro_weights"];

    for(int i=0; i<tfs.size(); i++){
      MicroCluster mc(tfs[i], times[i], weights[i]);
      micro.push_back(mc);
    }

    // reconstruct parameters
    r = serialized["r"];
    lambda = serialized["lambda"];
    tgap = serialized["tgap"];
    updateAll = serialized["updateAll"];
    verbose = serialized["verbose"];
    omega = serialized["omega"];
    t = serialized["t"];
    upToDate = serialized["upToDate"];
    termFading = serialized["termFading"];
  };


  Rcpp::List serialize(){

    // decompose micro clusters
    Rcpp::List tfs(micro.size());
    Rcpp::IntegerVector times(micro.size());
    Rcpp::NumericVector weights(micro.size());

    for(unsigned int i=0; i<micro.size(); i++){
      tfs[i] = static_cast <MicroCluster>(micro[i]).tf;
      weights[i] = static_cast <MicroCluster>(micro[i]).weight;
      times[i] = static_cast <MicroCluster>(micro[i]).time;
    }

    return Rcpp::List::create(
      Rcpp::Named("r") = r,
      Rcpp::Named("lambda") = lambda,
      Rcpp::Named("tgap") = tgap,
      Rcpp::Named("updateAll") = updateAll,
      Rcpp::Named("verbose") = verbose,
      Rcpp::Named("omega") = omega,
      Rcpp::Named("t") = t,
      Rcpp::Named("upToDate") = upToDate,
      Rcpp::Named("termFading") = termFading,
      Rcpp::Named("micro_tfs") = tfs,
      Rcpp::Named("micro_times") = times,
      Rcpp::Named("micro_weights") = weights
    );
  }


  void update(Rcpp::List x){

    // increment time
    this->t++;

    // if message contains tokens
    if(x.size()){

      // create temporary mc from text and timestamp
      MicroCluster mc(x, this->t);

      // Calculate IDF
      Rcpp::List idf = this->calculateIDF(this->get_microclusters());

      if(!updateAll){
        // only update closest mc
        int j = -1;
        double dist = this->r;
        // find closest mc within r
        for(unsigned int i=0; i < micro.size(); i++){
          double d = mc.distance(this->micro[i], idf);
          if(d <= dist){
            dist = d;
            j=i;
          }
        }
        if(j != -1){
          if(verbose) std::cout << "Merge observation " << this->t << " into Micro Cluster " << j  << std::endl;
          MicroCluster* microj = this->micro[j];
          microj->merge(mc, this->t, this->omega, this->lambda);
        } else{
          if(verbose) std::cout << "Use observation " << this->t <<" to create new Micro Cluster" << std::endl;
          this->micro.push_back(mc);
        }
      } else{
        // update all within r
        int merged=0;
        for(unsigned int i=0; i < micro.size(); i++){
          double d = mc.distance(this->micro[i], idf);
          if(d <= this->r){
            if(verbose) std::cout << "Merge observation " << this->t << " into Micro Cluster " << i  << std::endl;
            MicroCluster* microi = this->micro[i];
            microi->merge(mc, this->t, this->omega, this->lambda);
            merged=1;
          }
        }
        if(!merged){
          if(verbose) std::cout << "Use observation " << this->t <<" to create new Micro Cluster" << std::endl;
          this->micro.push_back(mc);
        }
      }
    } else if(verbose){
      std::cout << "Observation " << this->t <<" does not contain any token." << std::endl;
    }

    // remove outlier every tgap
    if(this->t % this->tgap == 0){
      this->cleanup();
    }
  }


  Rcpp::List get_microclusters(){
    // Rcpp::List result(this->micro.size());
    // for(unsigned int i=0; i<this->micro.size();i++){
    //   result[i] = this->micro[i];
    // }
    return(micro);
  }


  Rcpp::NumericVector get_microweights(){
    Rcpp::NumericVector weights(this->micro.size());
    for(unsigned int i=0; i<this->micro.size(); i++){
      MicroCluster* microi = this->micro[i];
      weights[i] = microi->weight;
    }
    return(weights);
  }


  Rcpp::NumericMatrix dist(Rcpp::List clusters){
    // calcualte pairwise distances bewteen all mcs
    int numMicro = clusters.size();
    Rcpp::NumericMatrix distance(numMicro, numMicro);
    Rcpp::List idf = this->calculateIDF(clusters);
    for(int i=0; i<distance.nrow(); i++){
      for(int j=i+1; j<distance.ncol(); j++){
        if(i==j){
          distance(i,j)=0; // avoid floating point precision on diagonal
        } else{
          MicroCluster* microi = clusters[i];
          double d = microi->distance(clusters[j], idf);
          distance(i,j) = d; // lower part
          distance(j,i) = d; // upper part
        }
      }
    }
    return(distance);
  }

  void updateWeights(){

    // fade Clusters
    for(int i=micro.size()-1; i>=0; i--){
      MicroCluster* microi = this->micro[i];
      microi->fade(this->t, this->omega, this->lambda, this->termFading);
      // remove insufficient weight or empty clusters
      if(microi->weight <= this->omega || microi->tf.size()==0){
        this->micro.erase(i);
      }
    }
  }

  Rcpp::List calculateIDF(Rcpp::List clusters){

    // calculate the IDF for all mcs
    Rcpp::List result;
    // iterate over all micro clusters
    for(unsigned int i=0; i<clusters.size(); i++){
      MicroCluster* microi = clusters[i];
      Rcpp::CharacterVector keys = microi->tf.names();
      for(int j=0; j<keys.size(); j++){
        std::string key; // init first to help compiler
        key = keys[j];
        if(result.containsElementNamed(keys[j])){
          double temp = result[key]; // help compiler
          result[key] = temp+1;
        } else{
          result[key] = 1;
        }
      }
    }
    // We take 1+log.. because otherwise single documents would always be zero
    // TODO can we just add +1?
    for(int i=0; i<result.size(); i++){
      double res = result[i];
      result[i] = 1+log(clusters.size()/res);
    }
    return(result);
  }


  int findClosestMC(Rcpp::List tf, Rcpp::List idf){

    // create temporary mc from text and timestamp
    MicroCluster mc(tf, this->t);

    // find cloest mc
    double dist=1;
    unsigned int j = -1;
    for(unsigned int i=0; i<this->micro.size(); i++){
      double d = mc.distance(this->micro[i], idf);
      if(d <= dist){
        dist = d;
        j=i;
      }
    }
    return(j); // C indexing
  }

  double findClosestDist(Rcpp::List tf, Rcpp::List idf, Rcpp::List centers){

    // create temporary mc from text and timestamp
    MicroCluster mc(tf, this->t);

    // find cloest mc
    double dist=1.0;
    for(unsigned int i=0; i<centers.size(); i++){
      double d = mc.distance(centers[i], idf);
      if(d <= dist){
        dist = d;
      }
    }
    return(dist);
  }



private:

  void cleanup(){

    if(this->verbose) std::cout << "Cleanup" << std::endl;
    this->updateWeights();

    Rcpp::List idf = this->calculateIDF(this->get_microclusters());
    // for every mc
    unsigned int i=0;
    while(i<micro.size()){
      // for every following mc
      unsigned int j=i+1;
      while(j < micro.size()){
        MicroCluster* microi = this->micro[i];
        double d = microi->distance(this->micro[j], idf); // calc distance
        if(d <= r){
          if(this->verbose) std::cout << "Merging " << j << " into " << i << std::endl;
          microi->merge(this->micro[j], this->t, this->omega, this->lambda); //merge mc into the other
          this->micro.erase(j); // remove the old mc
        } else{
          j++; // if none was removed, move to next
        }
      }
      i++;
    }
  }

};



// Allows to return class objects to R
RCPP_EXPOSED_CLASS(MicroCluster)
  RCPP_EXPOSED_CLASS(textClust)

  // Expose class members and methods to R
  RCPP_MODULE(MOD_textClust){
    using namespace Rcpp;

    class_<textClust>("textClust")
      .constructor<double, double, int, bool, bool,bool>()
      .constructor<Rcpp::List>()

    .field("r", &textClust::r)
    .field("lambda", &textClust::lambda)
    .field("tgap", &textClust::tgap)
    .field("updateAll", &textClust::updateAll)
    .field("verbose", &textClust::verbose)
    .field("omega", &textClust::omega)
    .field("t", &textClust::t)
    .field("upToDate", &textClust::upToDate)
    .field("termFading", &textClust::termFading)
    .field("micro", &textClust::micro)

    .method("update", &textClust::update)
    .method("get_microclusters", &textClust::get_microclusters)
    .method("get_microweights", &textClust::get_microweights)
    .method("dist", &textClust::dist)
    .method("updateWeights", &textClust::updateWeights)
    .method("calculateIDF", &textClust::calculateIDF)
    .method("findClosestMC", &textClust::findClosestMC)
    .method("findClosestDist", &textClust::findClosestDist)
    .method("serialize", &textClust::serialize)
    ;

    class_<MicroCluster>("MicroCluster")
      .constructor<Rcpp::List, int>()
      .field("tf", &MicroCluster::tf)
      .field("time", &MicroCluster::time)
      .field("weight", &MicroCluster::weight)
      .method("merge", &MicroCluster::merge)
      .method("distance",&MicroCluster::distance)
    ;

  }



