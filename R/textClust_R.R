#' Reference Class textClust
#'
#' Maintains the current clustering and implements the stream package interfaces.
#'
#' @field C Exposed C++ class
#' @field nmin min number of ngrams to use
#' @field nmax max number of ngrams to use
#' @field k number of clusters
#' @field h height for hierarchical clustering
#' @field microMacroAssignment assignment vector associating micro-clusters to a maco-cluster
#' @field throughput stores the throughput of messages
#' @field termFading logical whether individual terms should be faded
#' @field stopword chracter vector of stopwords to remove
#' @field linkage method for hierarchical clustering
#' @field weightedReclustering logical whether reclustering should consider cluster weights
#' @field minWeight minimum weight of micro clusters to be used for reclustering
#' @field textCol index of column that contains the text
#' @field timeCol index of column that contains the timestamps
#' @field timeFormat string formatting of time Column
#' @field timePrecision Precision of fading, either seconds, minutes, hours or days
#' @field fadeNaturalTime Logical whether Natural Time or Number of observations should be used for fading
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @export textClust_R
textClust_R <- setRefClass("textClust_R",
                           fields = list(
                             C ="ANY",
                             nmin="integer",
                             nmax="integer",
                             k="integer",
                             h="numeric",
                             microMacroAssignment = "integer",
                             throughput = "numeric",
                             termFading = "logical",
                             stopword = "character",
                             linkage="character",
                             weightedReclustering="logical",
                             minWeight="numeric",
                             textCol = "integer",
                             timeCol = "integer",
                             currenttime = "character",
                             hc = "ANY",
                             groupByCol = "integer",
                             parentTextCol = "integer",
                             parentTimeCol = "integer",
                             timeFormat = "character",
                             timePrecision = "character",
                             fadeNaturalTime = "logical"
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
#' @param k number of clusters
#' @param h height for hierarchical clustering
#' @param verbose logical whether to be more verbose
#' @param termFading logical whether individual terms should be faded
#' @param stopword chracter vector of stopwords to remove
#' @param linkage method for hierarchical clustering
#' @param weightedReclustering logical whether reclustering should consider cluster weights
#' @param minWeight minimum weight of micro clusters to be used for reclustering
#' @param textCol index of column that contains the text which should be clustered
#' @param timeCol index of column that contains timestamps
#' @param timeFormat string formatting of time Column
#' @param timePrecision Precision of fading, either seconds, minutes, hours or days
#' @param fadeNaturalTime Logical whether Natural Time or Number of observations should be used for fading
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @return reference class object
NULL
textClust_R$methods(
  initialize = function(r, lambda, tgap, nmin, nmax, k, h, verbose, termFading, stopword, linkage, weightedReclustering, minWeight, textCol, timeCol, groupByCol, parentTextCol, parentTimeCol, timeFormat, timePrecision, fadeNaturalTime) {
    ## fields in the r context
    nmin <<- as.integer(nmin)
    nmax <<- as.integer(nmax)
    k <<- as.integer(k)
    h <<- as.numeric(h)
    throughput <<-numeric()
    microMacroAssignment <<- integer()
    stopword <<- as.character(stopword)
    linkage <<- as.character(linkage)
    weightedReclustering <<- as.logical(weightedReclustering)
    minWeight <<- as.numeric(minWeight)
    textCol <<- as.integer(textCol)
    timeCol <<- as.integer(timeCol)
    currenttime <<- NA_character_
    hc <<- NULL
    groupByCol <<- as.integer(groupByCol)
    parentTextCol <<- as.integer(parentTextCol)
    parentTimeCol <<- as.integer(parentTimeCol)
    timeFormat <<- as.character(timeFormat)
    timePrecision <<- as.character(timePrecision)
    fadeNaturalTime <<- as.logical(fadeNaturalTime)

    ## rest is passed to C class
    C <<- new(textClust, as.numeric(r), as.numeric(lambda), as.integer(tgap), as.logical(verbose), as.logical(termFading)) ## Exposed C class
    .self
  }
)


#' Clustering procedure of textClust
#'
#' Main clustering procedure of textClust
#'
#' @name textClust_R_cluster
#'
#' @param newdata matrix of text in a single column
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @importFrom tokenizers tokenize_ngrams
NULL
textClust_R$methods(
  cluster = function(newdata) {

    C$upToDate <<- FALSE

    ## consider points one by one
    for(i in seq_len(nrow(newdata))){
      if(is.na(.self$groupByCol)){
        id = as.character(.self$C$n) ## incremental id (i.e. individual posts)
      } else{
        id = as.character(newdata[i, .self$groupByCol]) ## from data (i.e. group by common id)
      }

      ## remove from existing cluster
      if(!is.na(.self$groupByCol) & !is.na(.self$parentTextCol) & !is.na(.self$parentTimeCol)){
        parent = newdata[i,.self$parentTextCol]
        time = newdata[i,.self$parentTimeCol]

        if(parent != "" & time != ""){
          if(C$verbose) print(paste("Remove Text:", parent))

          time = as.POSIXct(strptime(time, .self$timeFormat))
          if(.self$timePrecision=="days"){
            time = as.integer(as.integer(time)/60/60/24)
          } else if(.self$timePrecision == "hours"){
            time = as.integer(as.integer(time)/60/60)
          } else if(.self$timePrecision == "minutes"){
            time = as.integer(as.integer(time)/60)
          } else{
            time = as.integer(time)
          }

          tokens = tokenize_ngrams(parent, n = nmax, n_min = nmin, lowercase=TRUE, simplify = TRUE, stopwords=.self$stopword)
          tf = as.list(table(tokens))

          .self$C$removeObservation(id, time, tf)
        }
      }


      ## insert as new
      x = newdata[i,.self$textCol]

      ## if a timecolumn is specified update the field
      if(!is.na(.self$timeCol)){
        currenttime <<- as.character(newdata[i,.self$timeCol])
      }

      if(C$verbose) print(paste("Insert Text:", x))

      ## current time
      #startTime = Sys.time()
      ## split and tokenize sentence
      tokens = tokenize_ngrams(x, n = nmax, n_min = nmin, lowercase=TRUE, simplify = TRUE, stopwords=.self$stopword)

      ## count term frequency
      tf = as.list(table(tokens))

      time = as.POSIXct(strptime(currenttime, .self$timeFormat))
      if(.self$timePrecision=="days"){
        time = as.integer(trunc(as.integer(time)/60/60/24))
      } else if(.self$timePrecision == "hours"){
        time = as.integer(trunc(as.integer(time)/60/60))
      } else if(.self$timePrecision == "minutes"){
        time = as.integer(trunc(as.integer(time)/60))
      } else if(.self$timePrecision == "seconds"){
        time = as.integer(time)
      }

      ## insert into closest MC
      if(!is.na(.self$timeCol) && .self$fadeNaturalTime){
        C$update(tf, id, time)
      } else{
        C$update(tf, id, -1)
      }

      ## getting messages per second
      #difference = difftime(Sys.time(),startTime,units="secs")
      #throughput[C$t] <<- difference
      # if(C$verbose){
      #   print(paste("Time Difference: ",difference))
      #   # print(paste("msg/s: ",counter))
      # }


    }
  }
)


#' get_microclusters
#'
#' Getter for micro-clusters of textClust, returns interfaced C objects
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
    clusters = .self$C$get_microclusters()
    return(clusters)
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
    weights = .self$C$get_microweights()
    return(weights)
  }
)


#' get_assignment
#'
#' Returns the cluster assignment of text to a respective micro-clusters
#'
#' @name textClust_R_get_assignment
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
NULL
textClust_R$methods(
  get_assignment = function() {
    assignment = .self$C$get_assignment()+1
    return(assignment)
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
      length(x$getTf())
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
  getRepresentatives = function(num=Inf, type="micro"){

    if(type=="micro"){
      clusters = .self$get_microclusters()
    } else{
      clusters = .self$get_macroclusters(merge=T)
    }

    ## for every cluster get the num entries with the heighest weight
    lapply(clusters, function(x){
      tf = sort(unlist(x$getTf()), decreasing=T)
      if(!is.null(num)){
        tf[seq_len(min(num, length(tf)))]
      }
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
    distance = C$dist(clusters)
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
    C$updateWeights()

    ## recluster
    .self$updateMacroClusters()

    microClusters = .self$get_microclusters()
    microWeights = .self$get_microweights()

    ## group mc by cluster
    clusters = lapply(seq_len(max(c(0,microMacroAssignment), na.rm = T)), function(x){
      microClusters[which(microMacroAssignment==x)]
    })


    ## return as list
    if(!merge){
      return(clusters)
    }

    ## return as merged cluster
    lapply(clusters, function(x){
      .self$C$mergeClusters(x)
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
    C$updateWeights()

    ## recluster
    .self$updateMacroClusters()

    ## extract weights
    microClusters = .self$get_microclusters()
    microWeights = .self$get_microweights()

    ## sum weights per cluster
    sapply(seq_len(max(c(0,.self$microMacroAssignment), na.rm = T)), function(x){
      sum(microWeights[which(.self$microMacroAssignment==x)])
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
    if(!C$upToDate){

      microClusters = .self$get_microclusters()
      microWeights = .self$get_microweights()
      microMacroAssignment <<- rep(NA_integer_, length(microClusters))

      ## filter by minWeight
      select = which(microWeights >= .self$minWeight)
      microClusters = microClusters[select]
      microWeights = microWeights[select]

      if(length(microClusters)>=2){
        distance = .self$dist(microClusters) ## get distance matrix
        if(.self$weightedReclustering){
          hc <<- hclust(distance,method=.self$linkage, members=microWeights) ## hierarchical clustering with weights
        } else{
          hc <<- hclust(distance,method=.self$linkage) ## hierarchical clustering without weights
        }
        if(!is.na(k)){
          microMacroAssignment[select] <<- cutree(hc, k = min(k,length(microClusters)))
        } else{
          microMacroAssignment[select] <<- cutree(hc, h = h)
        }
      } else{
        microMacroAssignment[select] <<- seq_along(microClusters)
      }
      C$upToDate <<- TRUE
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






