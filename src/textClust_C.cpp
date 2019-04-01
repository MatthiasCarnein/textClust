#include <Rcpp.h>
#include "microCluster.hpp"
#include <unordered_map>


using namespace Rcpp;

class textClust {
public:

  double r;
  double lambda;
  int tgap;
  bool verbose;
  double omega;
  int t;
  int n;
  bool upToDate;
  bool upToDateWeights;
  bool termFading;
  std::vector<MicroCluster*> micro;
  std::unordered_map<std::string, int> assignment;
  std::vector<std::string> assignmentOrder;


  textClust(double r, double lambda, int tgap, bool verbose, bool termFading){
    this->r = r;
    this->lambda = lambda;
    this->tgap = tgap;
    this->verbose = verbose;
    this->omega = 0;
    if(lambda!=0) this->omega = pow(2, (-1*lambda * tgap));
    this->t = 0;
    this->n = 0;
    this->upToDate=1;
    this->upToDateWeights=1;
    this->termFading = termFading;
  };


  void removeObservation(std::string id, int time, Rcpp::List parent){
    if(parent.size()){
      std::unordered_map<std::string,int>::iterator it = this->assignment.find(id);
      // if we have previously processed the parent
      if(it != this->assignment.end()){
        int i = this->assignment[id];
        // and if cluster still exists
        if(i != NA_INTEGER){
          // remove from existing cluster
          MicroCluster* mc = new MicroCluster(parent, time, 1);

          if(verbose) std::cout << "Remove observation from conversation " << id <<" in cluster " << i << std::endl;
          this->micro[i]->unmerge(mc, this->t, this->omega, this->lambda, this->termFading);
          // remove if insufficient weight or empty
          if(this->micro[i]->weight <= this->omega || this->micro[i]->tf.size()==0){
            if(verbose) std::cout << "Remove cluster " << i << std::endl;

            delete this->micro[i];
            this->micro.erase(this->micro.begin()+i); // remove cluster
            for (std::unordered_map<std::string, int>::iterator it = this->assignment.begin(); it != this->assignment.end(); it++ ){
              if(it->second == i) it->second = NA_INTEGER; // remove cluster assignment
              if(it->second > i) it->second--; // due to removed cluster
            }
          }
          delete mc;
        }
      }
    }
  }



  void update(Rcpp::List x, std::string id, int time =-1){

    this->n++;

    // decide whether we fade on real time or not
    if(time == -1){
      this->t = n;
    } else{
      this->t = time;
    }

    this->upToDateWeights=0;

    if(this->n%100 == 0) std::cout << "Observation " << this->n << " at time " << this->t << std::endl;

    // if message contains tokens
    if(x.size()){

      // create temporary mc from text and timestamp
      MicroCluster* mc = new MicroCluster(x, this->t, 1);

      // Calculate IDF
      std::unordered_map<std::string, double> idf = this->calculateIDF(this->micro);

      // only update closest mc
      int j = -1;
      double dist = this->r;
      // find closest mc within r
      for(unsigned int i=0; i < this->micro.size(); i++){
        double d = mc->distance(this->micro[i], idf);
        if(d <= dist){
          dist = d;
          j=i;
        }
      }
      if(j != -1){
        if(verbose) std::cout << "Merge observation " << this->n << " at time " << this->t << " into Micro Cluster " << j << " at distance " << dist << " (Conversation " << id << ")" << std::endl;
        this->micro[j]->merge(mc, this->t, this->omega, this->lambda, this->termFading);
        delete mc;
        this->assignment[id]=j;
      } else{
        if(verbose) std::cout << "Use observation " << this->n << " at time " << this->t << " to create new Micro Cluster " << this->micro.size() << " (Conversation " << id << ")" << std::endl;
        this->micro.push_back(mc);
        this->assignment[id]=this->micro.size()-1;
      }
    } else{
      this->assignment[id]=NA_INTEGER;
      if(verbose) std::cout << "Observation " << this->n << " at time " << this->t << " does not contain any token." << " (Conversation " << id << ")" << std::endl;
    }
    this->assignmentOrder.push_back(id);


    // remove outlier every tgap
    if(this->n % this->tgap == 0){
      if(verbose) Rcpp::Rcout<<"cleanup"<<std::endl;
      this->cleanup();
    }
  }


  std::vector<MicroCluster> get_microclusters(){
    if(!this->upToDateWeights) this->updateWeights();
    std::vector<MicroCluster> result;
    result.reserve(this->micro.size());
    for(unsigned int i=0; i<this->micro.size(); i++){
      MicroCluster tmp = *(this->micro[i]);
      result.push_back(tmp);
    }
    return result;
  }


  Rcpp::NumericVector get_microweights(){
    if(!this->upToDateWeights) this->updateWeights();
    Rcpp::NumericVector weights(this->micro.size());
    for(unsigned int i=0; i<this->micro.size(); i++){
      weights[i] = this->micro[i]->weight;
    }
    return(weights);
  }

  Rcpp::IntegerVector get_assignment(){

    Rcpp::IntegerVector mapResult(this->assignment.size());
    Rcpp::CharacterVector mapResultNames(this->assignment.size());
    std::unordered_map<std::string, int>::iterator it;
    for (unsigned int i=0; i<this->assignmentOrder.size(); i++){ // iterate insertion order
      it = assignment.find(assignmentOrder[i]); // and look up value in map
      mapResult[i] = it->second;
      mapResultNames[i] = it->first;
    }
    mapResult.attr("names") = mapResultNames;
    return mapResult;
  }



  Rcpp::NumericMatrix dist(Rcpp::List clusters){

    // convert R stucture to internal representation
    std::vector<MicroCluster*> clustersPtr;
    clustersPtr.reserve(clusters.size());
    for(int i=0; i<clusters.size(); i++){
      clustersPtr.push_back(clusters[i]); // this implicitly casts to a pointer
    }

    // calcualte pairwise distances bewteen all mcs
    Rcpp::NumericMatrix distance(clusters.size(), clusters.size());
    std::unordered_map<std::string, double> idf = this->calculateIDF(clustersPtr);

    for(int i=0; i<distance.nrow(); i++){
      for(int j=i+1; j<distance.ncol(); j++){
        if(i==j){
          distance(i,j)=0; // avoid floating point precision on diagonal
        } else{
          double d = clustersPtr[i]->distance(clustersPtr[j], idf);
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
      this->micro[i]->fade(this->t, this->omega, this->lambda, this->termFading);
      // remove insufficient weight or empty clusters
      if(this->micro[i]->weight <= this->omega || this->micro[i]->tf.size()==0){
        if(verbose) std::cout << "Remove cluster " << i << std::endl;

        delete this->micro[i];
        this->micro.erase(this->micro.begin()+i); // remove cluster

        for (std::unordered_map<std::string, int>::iterator it = this->assignment.begin(); it != this->assignment.end(); it++ ){
          if(it->second == i) it->second = NA_INTEGER; // remove cluster assignment
          if(it->second > i) it->second--; // due to removed cluster
        }
      }
    }
    this->upToDateWeights=1;
  }

  SEXP precalculateIDF(Rcpp::List clusters){

    // convert R stucture to internal representation
    std::vector<MicroCluster*> clustersPtr;
    clustersPtr.reserve(clusters.size());
    for(int i=0; i<clusters.size(); i++){
      clustersPtr.push_back(clusters[i]); // this implicitly casts to a pointer
    }

    std::unordered_map<std::string, double> idf = this->calculateIDF(clustersPtr);
    std::unordered_map<std::string, double>* idf2 = new std::unordered_map<std::string, double>(idf);

    return Rcpp::XPtr<std::unordered_map<std::string, double> >(idf2);
  }



  std::unordered_map<std::string, double> calculateIDF(std::vector<MicroCluster*> clusters){

    std::unordered_map<std::string, double> result;

    // iterate all micro clusters
    for(unsigned int i=0; i<clusters.size(); i++){
      // iterate tokens
      for (std::unordered_map<std::string, double>::iterator it = clusters[i]->tf.begin(); it != clusters[i]->tf.end(); it++ ){

        // check if token already exists
        std::unordered_map<std::string, double>::iterator resultIt = result.find(it->first);
        if(resultIt != result.end()){
          resultIt->second += 1;
        } else{
          result[it->first] = 1;
        }
      }
    }

    // total document / documents with term
    // We also take 1+log.. because otherwise single documents would always be zero
    for (std::unordered_map<std::string, double>::iterator it = result.begin(); it != result.end(); it++ ){
      it->second = 1+log(clusters.size()/it->second);
    }

    return(result);
  }


  int findClosestMC(Rcpp::List tf, SEXP idfPtr){

    Rcpp::XPtr<std::unordered_map<std::string, double> > temp(idfPtr);
    std::unordered_map<std::string, double> idf = *temp;



    // create temporary mc from text and timestamp
    MicroCluster mc(tf, this->t, 1);

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

  double findClosestDist(Rcpp::List tf, Rcpp::List clusters, SEXP idfPtr){


    // convert R stucture to internal representation
    std::vector<MicroCluster*> clustersPtr;
    clustersPtr.reserve(clusters.size());
    for(int i=0; i<clusters.size(); i++){
      clustersPtr.push_back(clusters[i]); // this implicitly casts to a pointer
    }

    Rcpp::XPtr<std::unordered_map<std::string, double> > temp(idfPtr);
    std::unordered_map<std::string, double> idf = *temp;

    // create temporary mc from text and timestamp
    MicroCluster mc(tf, this->t, 1);

    // find cloest mc
    double dist=1.0;
    for(unsigned int i=0; i<clustersPtr.size(); i++){
      double d = mc.distance(clustersPtr[i], idf);
      if(d <= dist){
        dist = d;
      }
    }
    return(dist);
  }


  MicroCluster mergeClusters(Rcpp::List clusters){

    MicroCluster mc = clusters[0]; // init with first
    for(int i=1; i<clusters.size(); i++){
      mc.merge(clusters[i], this->t, this->omega, this->lambda, this->termFading); // this implicitly casts clusters[i] to a pointer
    }
    return(mc);
  }


  Rcpp::List get_globalWeight(std::string token){
    Rcpp::List result;

    for(unsigned int i=0; i<this->micro.size(); i++){
      MicroCluster tmp = *(this->micro[i]);
      std::unordered_map<std::string, double> tf = tmp.tf;

      double sum = 0;

      // check if token already exists
      std::unordered_map<std::string, double>::iterator it = tf.find(token);
      if(it != tf.end()){
        sum += it->second;
      }
      result[token] = sum;
    }
    return(result);
  }

private:

  void cleanup(){

    if(this->verbose) std::cout << "Cleanup" << std::endl;
    this->updateWeights();

    std::unordered_map<std::string, double> idf = this->calculateIDF(this->micro);
    // for every mc
    unsigned int i=0;
    while(i<micro.size()){
      // for every following mc
      unsigned int j=i+1;
      while(j < micro.size()){
        double d = this->micro[i]->distance(this->micro[j], idf); // calc distance
        if(d <= r){
          if(this->verbose) std::cout << "Merging cluster " << j << " into cluster " << i << std::endl;
          this->micro[i]->merge(this->micro[j], this->t, this->omega, this->lambda, this->termFading); //merge mc into the other
          delete this->micro[j];
          this->micro.erase(this->micro.begin()+j); // remove the old mc

          // update cluster assignment
          for (std::unordered_map<std::string, int>::iterator it = this->assignment.begin(); it != this->assignment.end(); it++ ){
            if(it->second == (int)j) it->second = (int)i; // update cluster assignment
            if(it->second > (int)j) it->second--; // due to removed cluster
          }
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
      .constructor<double, double, int, bool,bool>()
    // .constructor<Rcpp::List>()

    .field("r", &textClust::r)
    .field("lambda", &textClust::lambda)
    .field("tgap", &textClust::tgap)
    .field("verbose", &textClust::verbose)
    .field("omega", &textClust::omega)
    .field("t", &textClust::t)
    .field("n", &textClust::n)
    .field("upToDate", &textClust::upToDate)
    .field("termFading", &textClust::termFading)
    // .field("micro", &textClust::micro)
    // .field("assignment", &textClust::assignment)

    .method("update", &textClust::update)
    .method("get_microclusters", &textClust::get_microclusters)
    .method("get_microweights", &textClust::get_microweights)
    .method("dist", &textClust::dist)
    .method("updateWeights", &textClust::updateWeights)
    .method("findClosestMC", &textClust::findClosestMC)
    .method("findClosestDist", &textClust::findClosestDist)
    .method("mergeClusters", &textClust::mergeClusters)
    .method("precalculateIDF", &textClust::precalculateIDF)
    .method("get_assignment", &textClust::get_assignment)
    .method("removeObservation", &textClust::removeObservation)
    ;

    class_<MicroCluster>("MicroCluster")
      .constructor<Rcpp::List, int, double>()
      .field("time", &MicroCluster::time)
      .field("weight", &MicroCluster::weight)
      .method("merge", &MicroCluster::merge)
      .method("getTf",&MicroCluster::getTf)
    ;

  }



