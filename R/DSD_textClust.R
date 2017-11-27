#' DSD_textClust
#'
#' StreamClustering data source
#'
#' @param streams vector of stream file names
#' @param labels vector that specifies how the streams should be labelled.
#' @param time specifies whether the stream should be ordered by time
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @export
DSD_textClust <- function(streams,labels = NULL,time = T) {

  textClust = lapply(streams, function(stream){
    return (DSD_ReadCSV(stream, sep = "\t", comment.char="", quote=""))
  })

  state <- new.env()
  assign("buffer", 1L, envir = state)
  state$buffer = result <- vector("list", length(streams))

  structure(list(description = "Data Streams for differerent text streams",
                 streams = textClust, time = time, labels = labels,counter=0,state=state),
            class = c("DSD_textClust", "DSD_R", "DSD_data.frame", "DSD"))

}



#' Get Points from textClust source
#'
#' Return \code{n} points from a textClust data source.
#'
#' @param x \code{DSD_textClust} object to read the points from
#' @param n number of points to return. default: 1
#' @param cluster logical whether to attach cluster assignment as an attribute
#'
#' @author
#' Dennis Assenmacher \email{dennis.assenmacher@@wi.uni-muenster.de}
#'
#' @export
get_points.DSD_textClust <- function(x, n = 1,cluster=FALSE, ...){
  if(is.null(x$labels))
    x$labels = c(1:length(x$streams))

  ## Randomly selecting messages from the stream
  if(x$time == F){
    randomStream = sample(length(x$streams),n,replace=T)

    result = do.call(rbind,lapply(randomStream,function(stream){
      res = get_points(x$streams[[stream]],n=1)
      res = list(res$V3,x$labels[stream])
    }))
    x$counter <- x$counter+1
  }

  ## If we want to get data according to time
  if(x$time == T){
    ## First we fill the buffer if it is empty
    x$state$buffer <- lapply(1:length(x$streams),function(i){
      if(is.null(x$state$buffer[[i]])){
        return(get_points(x$streams[[i]],n=1))
      }
      else return (x$state$buffer[[i]])
    })

    ## Now we get n points from the stream.
    result = do.call(rbind,lapply(1:n,function(stream){
      ## We identify the index of the oldest message within our buffer

      times = (sapply(x$state$buffer,function(x){return(x$V1)}))
      nextIDX = (order(strptime(times, "%m/%d/%Y %H:%M:%S")))[1]

      ## We save the message as a data frame
      res = data.frame(time=x$state$buffer[[nextIDX]]$V1,name =x$state$buffer[[nextIDX]]$V2,message=x$state$buffer[[nextIDX]]$V3,cluster=x$labels[nextIDX], stringsAsFactors = F)

      ## Since we fetched a message from the buffer, we have to replace it with a new message from the corresponding stream
      x$state$buffer[[nextIDX]] <- get_points(x$streams[[nextIDX]],n=1)

      return (res)
    }))


    finalRes = as.data.frame(result)
    #finalRes=finalRes[,"message", drop = FALSE]
    finalRes = finalRes[,-4]
    if(cluster) attr(finalRes, "cluster") <- result$cluster
  }
  return(finalRes)
}

