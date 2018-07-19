#' @title Control parameters for generating imptree objects
#' 
#' @description Initializing and validating the tree generation parameters
#' 
#' @param splitmetric Choosen split metric. See \code{\link{imptree}}
#' @param controlList Named list containing the processed arguments. See details.
#' @param tbase Value that needs to be at least attained to qualify for splitting (Default: 2)
#' @param gamma Weighting factor of the maximum entropy (Default: 1)
#' @param depth Positive integer limiting the tree to the given depth. 
#' If not supplied tree is grown to maximal size (Default)
#' @param minbucket Minimal leaf size (Default: 0)
#' @param \dots Argument gobbling; will not processed
#' 
#' @return A list containing the options. Missing options are set to their default value.
#' 
#' @details
#' The argument \code{controlList} may be a named list with names in 
#' \code{c("tbase", "gamma", "depth", "minbucket")}
#' Any values in this list will overwrite those supplied in named arguments.
#' When \code{controlList = NULL} (default) only the supplied arguments are checked.
#' 
#' In case \code{controlList} contains an argument named \code{splitmetric},
#' this will be ignored.
#' If \code{splitmetric} is 0L, i.e. \code{globalmax}, the values for \code{gamma} and \code{tbase} 
#' are set to their default values, even if the user supplied different values.
#' 
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, \code{\link{imptree_params}}
#' 
#' @keywords tree
#' 
#' @export
imptree_control <- function(splitmetric, controlList = NULL, tbase = 1, gamma = 1,
                            depth = .Machine$integer.max, minbucket = 0, ...) {

  # save the method parameter list
  sm <- splitmetric
  
  # generation of the bare returned list
  clist <- list(depth = depth, minbucket = minbucket, tbase = tbase, gamma = gamma)
  
  # first we process add the agruments supplied in controlList
  if(!is.null(controlList)) {	
    clist[names(controlList)[names(controlList) %in% names(clist)]] <- 
      controlList[names(controlList) %in% names(clist)]
  }

  # heading off now to test for inconsistencies
  if(sm) {

    # dealing inconsitencies in 'tbase'
    tbase <- as.double(clist$tbase)
    if(tbase > 2 || tbase < -1) {
      stop(paste("The maximal value 'tbase' a splitting variable may attain",
                  "to still qualify as candidate must be between [-1,2]!"))
    }
  
    # dealing inconsitencies in 'gamma'
    gamma <- as.double(clist$gamma)
    if(gamma > 1 || gamma < 0) {
      stop("'gamma' as weight of the upper entropy comparison must be in [0,1]!")
    }
  } else {
  
    # if splitmetric is globalmax (i.e. 0L), then tbase and gamma are predefined
    clist$gamma <- 1L
    clist$tbase <- 2L
  }
  
  # dealing inconsitencies in 'depth'
  mydepth <- clist$depth
  if(!is.null(mydepth) && !is.na(mydepth) && mydepth < 1) {
    warning("Tree depth must be at least 1; ", "Using full depth instead")
    mydepth <- NA
  }
  if(is.null(mydepth) || is.na(mydepth)) {
    mydepth <- as.integer(.Machine$integer.max)
  }
  clist$depth <- as.integer(mydepth)
  
  # dealing with inconsistency in 'minbucket'
  if((minbucket <- clist$minubucket) < 0) {
    warning(paste("Negative 'minbucket' not meaningful for minimal leaf size;",
                  "Setting to default 0"))
    minbucket <- 0L
  }
  clist$minubucket <- as.integer(minbucket)
  clist
}
