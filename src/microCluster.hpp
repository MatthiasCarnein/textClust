#ifndef textClust_MICROCLUSTER_H
#define textClust_MICROCLUSTER_H

#include<iostream>
#include <vector>
#include <algorithm>
#include <Rcpp.h>
#include <string>
#include <unordered_map>

using namespace Rcpp;
    class MicroCluster {
    public:
      MicroCluster (Rcpp::List tokens, int time, double weight);

    public:
        /**
         * Unmerges / removes a micro cluster from the current micro cluster
         */
        void unmerge(MicroCluster* mc, int t, int omega, double lambda, bool termFading);

        /**
         * Merges the current micro cluster with another micro cluster
         */
        void merge(MicroCluster* mc, int t, int omega, double lambda, bool termFading);

        /**
         * Fades the weight of a micro-cluster and the tokens within
         */
        void fade(int tnow, double omega, double lambda, bool termFading);

        Rcpp::List getTf();

        /**
         * Calcualtes the distance from the current micro cluster to another micro-cluster
         */
        double distance(MicroCluster* mc, std::unordered_map<std::string, double> idf);

        // Term frequency of current micro cluster
        std::unordered_map<std::string, double> tf;

        // Time of last update
        int time;
        // MC weight
        double weight;

    };



#endif //textClust_MICROCLUSTER_H
