#ifndef textClust_MICROCLUSTER_H
#define textClust_MICROCLUSTER_H

#include<iostream>
#include <vector>
#include <algorithm>
#include <Rcpp.h>
#include <string>

using namespace Rcpp;
    class MicroCluster {
    public:
      MicroCluster (Rcpp::List tokens, int time);
      MicroCluster (Rcpp::List tokens, int time, double weight);
      MicroCluster (Rcpp::List tokens, int time, bool termFading);

    public:
        /**
         * Merges the current micro cluster with another micro cluster
         */
        void merge(MicroCluster mc, int t, int omega, double lambda);

        /**
         * Fades the weight of a micro-cluster and the tokens within
         */
        void fade(int tnow, double omega, double lambda, bool termFading);

        /**
         * Calcualtes the distance from the current micro cluster to another micro-cluster
         */
        double distance(MicroCluster mc,Rcpp::List idf);

        // Term frequency of current micro cluster
        Rcpp::List tf ;
        // Time of last update
        int time;
        // MC weight
        double weight;
        // Should term be faded
        bool termFading;

    };



#endif //textClust_MICROCLUSTER_H
