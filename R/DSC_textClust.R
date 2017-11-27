

#' textClust
#'
#' Stream Clustering algorithm which clusters messages. Contains a description string as well as a reference class object of \code{textClust} which is the work-horse.
#'
#' @param r distance threshold to merge two micro-clusters
#' @param lambda fading constant
#' @param tgap time between outlier removal
#' @param nmin min number of ngrams to use
#' @param nmax max number of ngrams to use
#' @param updateAll logical whether a new observations is used to update all micro-clusters within \code{r} or just the closest.
#' @param k number of clusters for macro-clustering
#' @param h height to determine number of clusters for macro-clustering
#' @param verbose logical whether to be more verbose
#' @param termFading logical whether individual terms should also be faded
#'
#' @return micro clusters
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @references
#' Matthias Carnein, Dennis Assenmacher, Heike Trautmann (2017)"Stream Clustering of Chat Messages with Applications to Twitch Streams". In: Advances in Conceptual Modeling. ER 2017. Lecture Notes in Computer Science, vol 10651. Springer, Cham
#'
#' @examples
#' stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' textClust <- DSC_textClust(r=.75, lambda=.1, tgap=1000)
#' update(textClust, stream, n=100)
#'
#' @export
DSC_textClust <- function(r=.4, lambda=0.1, tgap=1000, nmin=1, nmax=1, updateAll=F, k=NA_integer_, h=NA_real_, verbose = F, termFading =T) {

  textClust <- textClust_R$new(r, lambda, tgap, nmin, nmax, updateAll, k, h, verbose,termFading)

  structure(
    list(
      description = "textClust",
      RObj = textClust
    ), class = c("DSC_textClust", "DSC_Micro", "DSC_R", "DSC")
  )
}


#' Plot DSC_textClust
#'
#' Plot function for object of type DSC_textClust. Uses Multidimensional Scaling (MDS) to plot the words with the heighest weight for each micro-cluster
#'
#' @param dsc object of class DSC_textClust
#' @param numRepresentatives number of words to plot per micro cluster (default 1)
#' @param type string indicating whether to plot micro or macro cluster
#' @param dendrogram logical whether to plot dendrogram or use multidimensional scaling to plot representatives
#' @param merge logical whether to merge all clusters into one
#' @param interactive use interactive plots with the zoom package
#'
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' algorithm = DSC_textClust(lambda=0.001, r=0.2, tgap=100, verbose=T)
#' update(algorithm, stream, n=100)
#' plot(algorithm, numRepresentatives = 2)
#'
#' @export
plot.DSC_textClust <- function(dsc, numRepresentatives=1, type="micro", dendrogram=F, merge=F, interactive=T) {

  if(nclusters(dsc)<=2){
    stop("Too few clusters!")
  }

  if(dendrogram){
    plot(dsc$RObj$hc)
  } else{
    dsc$RObj$textClust_C$updateWeights()
    ## calculate distance and mds

    ## retieve centers
    if(type=="macro" && merge==T){
      clusters = get_centers(dsc, type="macro")
    } else{
      clusters = get_centers(dsc, type="micro")
    }

    ## calc dist matrix
    d = dsc$RObj$dist(clusters)

    mds <- cmdscale(d)

    ## get colors based on macro assignment or same color
    if(type=="macro" && merge==F){
      assignments = microToMacro(dsc)
    } else{
      assignments = rep(1, length(clusters))
    }
    ## empty plot
    plot(mds[,1], mds[,2], type="n", xlab="", ylab="")

    ## get word with highest weight for each mc
    if(type=="macro" && merge==T){
      reps = sapply(dsc$RObj$getRepresentatives(numRepresentatives, "macro"), function(x){
        paste(names(x), collapse=", ")
      })
      weights = get_weights(dsc, type="macro")
    } else{
      reps = sapply(dsc$RObj$getRepresentatives(numRepresentatives, "micro"), function(x){
        paste(names(x), collapse=", ")
      })
      weights = get_weights(dsc, type="micro")
    }

    ## plot
    text(mds[, 1], mds[, 2], reps, cex = ((weights - min(weights))/diff(range(weights)))+1, col=assignments)

    if(interactive) zm()
  }
}


#' Get Assignment for DSC_textClust
#'
#' Returns the index of the closest micro-cluster for every entry in points using the cosine similary of the tf-idf vectors.
#'
#' @param dsc object of class DSC_textClust
#' @param points matrix with the messages to assign
#'
#' @return Indicies of the closest micro-clusters
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' algorithm = DSC_textClust(lambda=0.001, r=0.2, tgap=100, verbose=T)
#' update(algorithm, stream, n=100)
#'
#' data = read.table("file.txt", header=F, sep="\t", quote="", comment.char = "", stringsAsFactors =F)
#' get_assignment(algorithm, data)
#'
#' @export
get_assignment.DSC_textClust <- function(dsc, points, ...) {

  colnames(points) = c("time", "user", "message")

  ## get mcs
  microClusters = get_centers(dsc)

  ## get idf
  currentIDF = dsc$RObj$calculateIDF()

  ## for every row
  assignment = apply(points, 1, function(x){

    ## split and tokenize sentence
    tokens = tokenize_ngrams(x["message"], n = dsc$RObj$nmax, n_min = dsc$RObj$nmin, lowercase=TRUE, simplify = TRUE)

    ## count term frequency
    tf = as.list(table(tokens))

    ## if message contains no token return NA
    if(length(tf) == 0){
      return(NA)
    }


    closest=dsc$RObj$textClust_C$findClosestMC(tf, currentIDF)+1
    if(closest == 0){
      return (NA)
    }
    else{
      return(closest)
    }
  })
  unlist(assignment)
}


#' Print DSC_textClust
#'
#' Print an object of type DSC_textClust.
#'
#' @param dsc object of class DSC_textClust
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' algorithm = DSC_textClust(lambda=0.001, r=0.2, tgap=100, verbose=T)
#' print(algorithm)
#'
#' @export
print.DSC_textClust <- function(dsc){
  print(dsc$description)
  print(paste("Class:", paste(class(dsc), collapse=", ")))
  print(paste("Number of micro-clusters:", length(dsc$RObj$get_microclusters())))
}


#' Number of clusters for DSC_textClust
#'
#' Returns the number of clusters for DSC_textClust
#'
#' @param x object of class DSC_textClust
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' algorithm = DSC_textClust(lambda=0.001, r=0.2, tgap=100, verbose=T)
#' print(algorithm)
#'
#' @export
nclusters.DSC_textClust <- function(x, type=c("auto", "micro", "macro"), ...) {
  length(get_centers(x, type=type, ...))
}
