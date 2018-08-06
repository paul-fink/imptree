#' @title Control parameters for generating
#' imptree objects
#' 
#' @description Initializing and validating 
#' the tree generation parameters
#' 
#' @param splitmetric Choosen split metric. 
#' See \code{\link{imptree}}
#' @param controlList Named list containing the processed arguments.
#' See details.
#' @param tbase Value that needs to be at least attained to qualify
#' for splitting (Default: 2)
#' @param gamma Weighting factor of the maximum entropy
#' (default: 1)
#' @param depth Integer limiting the tree to the given depth.
#' If not supplied, \code{NULL} (default) or smaller than 1 the
#' tree is grown to maximal size, the latter triggering a warning.
#' @param minbucket Positive integer as minimal leaf size
#' (default: 1)
#' @param \dots Argument gobbling; will not processed
#' 
#' @return A list containing the options. Missing options are set
#' to their default value.
#' 
#' @details
#' The argument \code{controlList} may be a named list with names in
#' \code{c("tbase", "gamma", "depth", "minbucket")}
#' Any values in this list will overwrite those supplied in 
#' named arguments.
#' When \code{controlList = NULL} (default) only the supplied 
#' arguments are checked.
#' 
#' In case \code{controlList} contains an argument named
#' \code{splitmetric}, this will be ignored.
#' If \code{splitmetric} is 0L, i.e. \code{globalmax}, the values 
#' for \code{gamma} and \code{tbase} 
#' are set to their default values, even if the user supplied 
#' different values.
#' 
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, \code{\link{imptree_params}}
#' 
#' @keywords tree
#' 
#' @export
imptree_control <- function(splitmetric, controlList = NULL, tbase = 1, gamma = 1,
                            depth = NULL, minbucket = 1L, ...) {

  # save the method parameter list
  sm <- splitmetric
  
  # generation of the bare returned list
  clist <- list(depth = depth, minbucket = minbucket,
                tbase = tbase, gamma = gamma)
  
  # first we process add the agruments supplied in controlList
  if(!is.null(controlList)) {	
    clist[names(controlList)[names(controlList) %in% names(clist)]] <- 
      controlList[names(controlList) %in% names(clist)]
  }
  
  if(any(nal <- is.na(clist))) {
    stop(sprintf("no 'NA' permitted in %s", 
                 paste(sQuote(names(clist)[nal]), collapse = ", ")))
  }

  # heading off now to test for inconsistencies
  if(sm) {

    # dealing inconsitencies in 'tbase'
    tbase <- as.double(clist[["tbase"]])
    if(tbase > 2 || tbase < -1) {
      stop(sprintf("value of 'tbase' (%f) must be between [-1,2]", tbase))
    }
  
    # dealing inconsitencies in 'gamma'
    gamma <- as.double(clist[["gamma"]])
    if(gamma > 1 || gamma < 0) {
      stop(sprintf("value of 'gamma' (%f) must be in [0,1]",
                   gamma))
    }
  } else {
  
    # if splitmetric is globalmax (i.e. 0L), then tbase and gamma are predefined
    clist$gamma <- 1
    clist$tbase <- 2
  }
  
  # dealing inconsitencies in 'depth'
  mydepth <- clist[["depth"]]
  if(!is.null(mydepth) && mydepth < 1L) {
    warning(sprintf("ignoring supplied 'depth'=%d and use default instead",
                    mydepth))
    mydepth <- NULL
  }
  if(is.null(mydepth)) {
    mydepth <- as.integer(.Machine$integer.max)
  }
  clist$depth <- as.integer(mydepth)
  
  # dealing with inconsistency in 'minbucket'
  if((minbucket <- clist[["minbucket"]]) < 1L) {
    warning(sprintf("ignoring supplied 'minbucket'=%d and use default instead",
                    minbucket))
    minbucket <- 1L
  }
  clist$minbucket <- as.integer(minbucket)
  clist
}
