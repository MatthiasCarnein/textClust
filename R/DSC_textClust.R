

#' textClust
#'
#' Stream Clustering algorithm which clusters text data.
#'
#' @param r distance threshold to merge two micro-clusters
#' @param lambda fading constant
#' @param tgap time between outlier removal
#' @param nmin min number of ngrams to use
#' @param nmax max number of ngrams to use
#' @param k number of clusters for macro-clustering
#' @param h height to determine number of clusters for macro-clustering
#' @param verbose logical whether to be more verbose
#' @param termFading logical whether individual terms should also be faded
#' @param stopword chracter vector of stopwords to remove
#' @param linkage method for hierarchical clustering
#' @param weightedReclustering logical whether reclustering should consider cluster weights
#' @param minWeight minimum weight of micro clusters to be used for reclustering
#' @param textCol index of column that contains the text which should be clustered
#' @param timeCol index of column that contains timestamps
#' @param groupByCol index of column that groups text into conversations (i.e. multiple texts into the same document)
#' @param parentTextCol index of column that contains the text of the parent when using groupByCol
#' @param parentTimeCol index of column that contains the time of the parent when using groupByCol
#' @param timeFormat string formatting of time Column
#' @param timePrecision Precision of fading, either seconds, minutes, hours or days
#' @param fadeNaturalTime Logical whether Natural Time or Number of observations should be used for fading
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
#' data = data.frame(text=sample(c("Main Topic", "Similar Topic", "Something Different"), size=1000, replace=T),stringsAsFactors=F)
#' stream = DSD_Memory(data)  ## Alternatively: stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' algorithm = DSC_textClust(r=.4, lambda=0.1, tgap=100, nmin=1, nmax=2, k=3, stopword=c(), minWeight=3, textCol=1)
#' update(algorithm, stream, n=1000)
#'
#' @importFrom stopwords stopwords
#'
#' @export
DSC_textClust <- function(r=.4, lambda=0.1, tgap=1000, nmin=1, nmax=1, k=NA_integer_, h=NA_real_, verbose = F, termFading =T, stopword=stopwords(language = "en", source = "stopwords-iso"), linkage="complete", weightedReclustering=TRUE, minWeight = 2, textCol = 1, timeCol = NA_integer_, groupByCol = NA_integer_, parentTextCol = NA_integer_, parentTimeCol = NA_integer_, timeFormat="%Y-%m-%d %H:%M:%S", timePrecision="days", fadeNaturalTime=FALSE) {

  textClust <- textClust_R$new(r=r, lambda=lambda, tgap=tgap, nmin=nmin, nmax=nmax, k=k, h=h, verbose=verbose, termFading=termFading, stopword=stopword, linkage=linkage, weightedReclustering=weightedReclustering, minWeight=minWeight, textCol=textCol, timeCol=timeCol, groupByCol=groupByCol, parentTextCol=parentTextCol, parentTimeCol=parentTimeCol, timeFormat=timeFormat, timePrecision=timePrecision, fadeNaturalTime=fadeNaturalTime)

  structure(
    list(
      description = "textClust",
      RObj = textClust
    ), class = c("DSC_textClust", "DSC_Micro", "DSC_R", "DSC")
  )
}






#' Get Assignment for DSC_textClust
#'
#' Returns the index of the closest micro-cluster for every entry in points using the cosine similary of the tf-idf vectors.
#'
#' @param dsc object of class DSC_textClust
#' @param points matrix with the messages to assign
#' @param ... optional arguments are ignored
#'
#' @return Indicies of the closest micro-clusters
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' data = data.frame(text=sample(c("Main Topic", "Similar Topic", "Something Different"), size=1000, replace=T),stringsAsFactors=F)
#' stream = DSD_Memory(data)  ## Alternatively: stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' algorithm = DSC_textClust(r=.4, lambda=0.1, tgap=100, nmin=1, nmax=2, k=3, stopword=c(), minWeight=3, textCol=1)
#' update(algorithm, stream, n=1000)
#'
#' data = data.frame(text=sample(c("Main Topic", "Something Different"), size=100, replace=T),stringsAsFactors=F)
#' get_assignment(algorithm, data)
#'
#' @export
get_assignment.DSC_textClust <- function(dsc, points, ...) {

  ## select text column
  points = points[,dsc$RObj$textCol, drop = FALSE]

  ## get mcs
  microClusters = dsc$RObj$C$get_microclusters()

  ## get idf
  currentIDF = dsc$RObj$C$precalculateIDF(microClusters)

  ## for every row
  assignment = apply(points, 1, function(x){

    ## split and tokenize sentence
    tokens = tokenize_ngrams(x, n = dsc$RObj$nmax, n_min = dsc$RObj$nmin, lowercase=TRUE, simplify = TRUE)

    ## count term frequency
    tf = as.list(table(tokens))

    ## if message contains no token return NA
    if(length(tf) == 0){
      return(NA)
    }

    closest=dsc$RObj$C$findClosestMC(tf, currentIDF)+1
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
#' @param ... optional arguments are ignored
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' algorithm = DSC_textClust(lambda=0.001, r=0.2, tgap=100, verbose=T)
#' print(algorithm)
#'
#' @export
print.DSC_textClust <- function(x, ...){
  print(x$description)
  print(paste("Class:", paste(class(x), collapse=", ")))
  print(paste("Number of micro-clusters:", length(x$RObj$get_microclusters())))
}


#' Number of clusters for DSC_textClust
#'
#' Returns the number of clusters for DSC_textClust
#'
#' @param x object of class DSC_textClust
#' @param type whether micro or macro clusters should be counted
#' @param ... optional arguments passed to get_centers
#'
#' @author
#' Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
#' @examples
#' data = data.frame(text=sample(c("Main Topic", "Similar Topic", "Something Different"), size=1000, replace=T),stringsAsFactors=F)
#' stream = DSD_Memory(data)  ## Alternatively: stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")
#' algorithm = DSC_textClust(r=.4, lambda=0.1, tgap=100, nmin=1, nmax=2, k=3, stopword=c(), minWeight=3, textCol=1)
#' update(algorithm, stream, n=1000)
#'
#' nclusters(algorithm)
#'
#' @export
nclusters.DSC_textClust <- function(x, type=c("auto", "micro", "macro"), ...) {
  length(get_centers(x, type=type, ...))
}
