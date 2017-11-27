#' Reference Class textClust
#'
#' Maintains the current clustering and implements the stream package interfaces.
#'
#' @field textClust_C Exposed C++ class
#' @field nmin min number of ngrams to use
#' @field nmax max number of ngrams to use
#' @field k number of clusters
#' @field h height for hierarchical clustering
#' @field upToDate logical whether macro-clusters are up to date or need to be recomputed
#' @field microMacroAssignment assignment vector associating micro-clusters to a maco-cluster
#' @field hc hclust object
#' @field throughput stores the throughput of messages
#' @field termFading logical whether individual terms should be faded
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @export textClust_R
textClust_R <- setRefClass("textClust_R",
                                    fields = list(
                                      textClust_C ="ANY",
                                      nmin="integer",
                                      nmax="integer",
                                      k="integer",
                                      h="numeric",
                                      upToDate = "logical",
                                      microMacroAssignment = "integer",
                                      hc="ANY",
                                      throughput = "numeric",
                                      termFading = "logical"
                                    ))


#' Constructor of textClust
#'
#'
#' @name textClust_R_initialize
#'
#' @param r radius threshold
#' @param lambda decay rate
#' @param tgap Time-interval for outlier detection and clean-up
#' @param nmin min number of ngrams to use
#' @param nmax max number of ngrams to use
#' @param updateAll logical whether a new observations is used to update all micro-clusters within \code{r} or just the closest.
#' @param k number of clusters
#' @param h height for hierarchical clustering
#' @param verbose logical whether to be more verbose
#' @param termFading logical whether individual terms should be faded
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @return refernce class object
NULL
textClust_R$methods(
  initialize = function(r, lambda, tgap, nmin, nmax, updateAll, k, h, verbose, termFading) {
    ## fields in the r context
    nmin <<- as.integer(nmin)
    nmax <<- as.integer(nmax)
    k <<- as.integer(k)
    h <<- as.numeric(h)
    throughput <<-numeric()
    microMacroAssignment <<- integer()
    ## rest is passed to C class
    textClust_C <<- new(textClust, r, lambda, tgap, updateAll, verbose, termFading) ## Exposed C class
    .self
  }
)


#' Clustering procedure of textClust
#'
#' Main clustering procedure of textClust
#'
#' @name textClust_R_cluster
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @param newdata matrix of data with three columns (time, username, message)
NULL
textClust_R$methods(
  cluster = function(newdata) {

    textClust_C$upToDate <<- FALSE

    colnames(newdata) = c("time", "user", "message")

    ## consider points one by one
    apply(newdata, 1, function(x){

      if(textClust_C$verbose) print(paste("message:", x["message"]))

      ## current time
      startTime = Sys.time()

      ## split and tokenize sentence
      tokens = tokenize_ngrams(x["message"], n = nmax, n_min = nmin, lowercase=TRUE, simplify = TRUE)

      ## count term frequency
      tf = as.list(table(tokens))

      ## insert into closest MC
      textClust_C$update(tf)

      ## getting messages per second
      difference = difftime(Sys.time(),startTime,units="secs")
      throughput[textClust_C$t] <<- difference
      if(textClust_C$verbose){
        print(paste("Difference: ",difference))
        # print(paste("msg/s: ",counter))
      }


    })
  }
)


#' Serialize DSC_textClust_R object
#'
#' Exports the C++ Object of DSC_textClust_R to an R-List
#'
#' @name textClust_R_serialize
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
NULL
textClust_R$methods(
  serialize = function() {
    textClust_C$serialize()
  }
)


#' Deserialize DSC_textClust_R object
#'
#' Imports a serialized R-List back into an C++ Object
#'
#' @name textClust_R_deserialize
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
NULL
textClust_R$methods(
  deserialize = function(serialized) {
    textClust_C <<- new(textClust, serialized)
  }
)


#' Calculate Inverted Document Frequency
#'
#' Calculates the Inverted Document Frequency, i.e. the popularity of words accross the entire corpus.
#'
#' @name textClust_R_calculateIDF
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @return Inverted Document Frequency as a vector
#'
NULL
textClust_R$methods(
  calculateIDF = function(clusters=.self$get_microclusters()) {
    textClust_C$calculateIDF(clusters)
  }
)

#' get_microclusters
#'
#' Getter for micro-clusters of textClust
#'
#' @name textClust_R_get_microclusters
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @return list of micro-clusters
NULL
textClust_R$methods(
  get_microclusters = function() {
    textClust_C$get_microclusters()
  }
)


#' get_microweights
#'
#' Returns the weights of micro-clusters of an textClust_R object
#'
#' @name textClust_R_get_microweights
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
NULL
textClust_R$methods(
  get_microweights = function() {
    textClust_C$get_microweights()
  }
)


#' count Terms
#'
#' Counts the number of terms in the micro or macro clusters
#'
#' @name textClust_R_countTerms
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
NULL
textClust_R$methods(
  countTerms = function(type="micro") {
    if(type=="micro") clusters = .self$get_microclusters()
    if(type=="macro") clusters = .self$get_macroclusters(merge=T)
    sum(sapply(clusters, function(x){
      length(x$tf)
    }))
  }
)



#' Representative Words for cluster
#'
#' Returns the num words with the heighest weight for each micro-cluster
#'
#' @name textClust_R_getRepresentatives
#'
#' @param num number of words to return
#' @param type return representatives for micro or macro clusters
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @return a list containing the words for each micro-cluster
#'
NULL
textClust_R$methods(
  getRepresentatives = function(num=1, type="micro"){

    if(type=="micro"){
      clusters = .self$get_microclusters()
    } else{
      clusters = .self$get_macroclusters(merge=T)
    }

    ## for every cluster get the num entries with the heighest weight
    lapply(clusters, function(x){
      tf = sort(unlist(x$tf))
      tf[seq_len(min(num, length(tf)))]
    })
  }
)



#' Pairwise distances between micro-cluster
#'
#' Returns a pairwise distance matrix between all micro-clusters .
#'
#' @name textClust_R_dist
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @return distance matrix of class \code{dist}
#'
NULL
textClust_R$methods(
  dist = function(clusters=.self$get_microclusters()){
    distance = textClust_C$dist(clusters)
    return(stats::as.dist(distance,diag=T))
  }
)



#' Micro To Macro
#'
#' Assignment vector that associates each micro-cluster to a macro cluster
#'
#' @name textClust_R_microToMacro
#'
#' @param micro vector of micro clusters ids to retrieve the assignment for
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @return microToMacro assignment
#'
NULL
textClust_R$methods(
  microToMacro=function(micro=NULL,...){
    .self$updateMacroClusters()

    if(!is.null(micro)){
      microMacroAssignment[micro]
    } else {
      microMacroAssignment
    }
  }
)


#' Micro To Macro
#'
#' Assignment vector that associates each micro-cluster to a macro cluster
#'
#' @name textClust_R_get_macroclusters
#'
#' @param micro vector of micro clusters ids to retrieve the assignment for
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @return microToMacro assignment
#'
NULL
textClust_R$methods(
  get_macroclusters=function(merge=T,...){

    ## update all weights
    textClust_C$updateWeights()

    ## recluster
    .self$updateMacroClusters()

    microClusters = .self$get_microclusters()

    ## group mc by cluster
    clusters = lapply(seq_len(k), function(x){
      microClusters[microMacroAssignment==x]
    })
    clusters = clusters[sapply(clusters, function(x){length(x)>0})]
    ## return as list
    if(!merge){
      return(clusters)
    }
    ## return as merged mc
    lapply(clusters, function(x){
      mc = new(MicroCluster, list(),textClust_C$t)
      ## workaround: init with first mc to avoid empty name-strings in C
      mc$tf = x[[1]]$tf
      mc$weight = x[[1]]$weight
      for(cluster in x[-1]){
        mc$merge(cluster, textClust_C$t, textClust_C$omega, textClust_C$lambda)
      }
      return(mc)
    })
  }
)




#' get_macroclusters
#'
#' Getter for weights of macro-clusters of textClust_R
#'
#' @name textClust_get_microweights
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @return list of macro-clusters
textClust_R$methods(
  get_macroweights=function(...){

    ## update all weights
    textClust_C$updateWeights()

    ## recluster
    .self$updateMacroClusters()

    ## extract weights
    weights = .self$get_microweights()

    ## sum weights per cluster
    sapply(seq_len(k), function(x){
      sum(weights[microMacroAssignment==x])
    })
  }
)



#' Update macro clusters
#'
#' Updates the macro clusters by running hierarchical clustering
#'
#' @name textClust_R_updateMacroClusters
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
NULL
textClust_R$methods(
  updateMacroClusters=function(...){
    if(is.na(k) && is.na(h)) stop("Either h or k needs to be specified to perform macro clustering.")
    if(!textClust_C$upToDate){
      distance = .self$dist() ## get distance matrix
      hc <<- hclust(distance,method="single") ## hierarchical clustering
      if(!is.na(k)){
        microMacroAssignment <<- cutree(hc, k = min(k,length(textClust_C$get_microclusters())))
      } else{
        microMacroAssignment <<- cutree(hc, h = h)
      }
      textClust_C$upToDate <<- TRUE
    }
  }
)


#' Returns Throughput
#'
#' Returns the number of processed messages per Second
#'
#' @name textClust_R_getThroughputPerSecond
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
NULL
textClust_R$methods(
    getThroughputPerSecond = function(){
      res = numeric()
      sum = 0
      messages = 0
      j = 1
      for(i in 1:length(.self$throughput)){
        sum = sum + .self$throughput[i]
        messages = messages+1
        if(sum>1){
          res[j] = messages
          j = j+1
          messages = 0
          sum = 0
        }
      }
      return(data.frame(time=seq_along(res),throughput=res))
    }
  )


#' Seconds per messages
#'
#' Returns the number of seconds used to process a message
#'
#' @name textClust_R_getSecondsPerMessage
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
NULL
textClust_R$methods(
  getSecondsPerMessage = function(){

    data = data.frame(time=seq_along(.self$throughput),throughput=.self$throughput)

    seq = seq(1, nrow(data), by=100)
    do.call(rbind, lapply(seq_along(seq[-1]), function(i){
      return(data.frame(time=seq[i+1], secondsPerMessage=sum(data$throughput[seq[i]:seq[i+1]])))
    }))
  }
)
