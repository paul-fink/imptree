#' @title Classification with Imprecise Probabilities
#' 
#' @name imptree
#' 
#' @description \code{imptree} implements Abellan and Moral's tree 
#' algorithm (based on Quinlans ID3) for classification. It
#' employes either the Imprecise Dirichlet Model (IDM) or the 
#' Nonparametric Predictive Inference (NPI) to generate the nodes'
#' class probability distribution.
#' 
#' @aliases imptree imptree.formula imptree.default
#' 
#' @param formula Formula describing the strucutre
#'   (class variable ~ featutre variables).
#'   Any interaction terms trigger an error. Must be supplied!
#' @param data Data.frame to evaluate supplied formula on.
#'  If not provided the the formula is evaluated 
#'  on the calling environment
#' @param epistemic if \code{TRUE} the variables are supposed to be of epistemic nature,
#'  if \code{FALSE} then they are ontological. See details. 
#' @param weights Individual weight for the observations(Default: 1 to each)
#' @param control A named (partial) list according to the result of
#'  \code{\link{imptree_control}}.
#' @param method Method applied for calculating the probability intervals of
#'  the class probability. \code{"IDM"} for the Imprecise Dirichlet Model 
#'  (Default), \code{"NPI"} for use of the Nonparametric Predictive
#'  Inference approach and \code{"NPIapprox"} for use of the approximate 
#'  algorithm obtaining maximal entropy of NPI generate probability intervals.
#' @param method.param Named list specifying the mehtod specific parameter.
#'  See details.
#' @param \dots optional parameters to be passed to the main function
#'  \code{imptree.formula} or to the call of \code{\link{imptree_control}}.
#'  
#' @details
#'  A multi-state observation of a variable when treated epistemically, all
#'  the states are possible values of the observations, however in case of
#'  ontological interpretation each unique multi-state is assumed as a further
#'  single state. The a multi-state observation is characterized by a factor
#'  with the levels for a multi-state consisting of the unique single stated
#'  separated by a colon \code{":"}.
#'  
#' @return An object of class \code{imptree}, which is a list with the
#'  following components:
#'  \item{call}{Original call to \code{imptree}}
#'  \item{tree}{Object reference to the Java tree object.}
#'  \item{train}{Named list of training data containing the Java Object
#'    reference to the training data and the R model.frame}
#'  \item{formula}{The formula describing the data structure}
#' 
#' @references Abell\ifelse{latex}{\out{\'{a}}}{\ifelse{html}{\out{&aacute;}}{a}}n,
#' J. and Moral, S. (2005), Upper entropy of credal sets. Applications to 
#' credal classification, \emph{International Journal of Approximate Reasoning}
#' \bold{39}, 235--255.
#' @references Strobl, C. (2005), Variable Selection in Classification Trees Based on
#' Imprecise Probabilities, \emph{ISIPTA'05: Proceedings of the Fourth
#' International Symposium on Imprecise Probabilities and Their Applications},
#' 339--348.
#' @references Baker, R. M. (2010), \emph{Multinomial Nonparametric Predictive Inference:
#' Selection, Classification and Subcategory Data}.
#'  
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}, based on
#' algorithms by J. Abell\ifelse{latex}{\out{\'{a}}}{\ifelse{html}{\out{&aacute;}}{a}}n and
#' S. Moral for the IDM and R. M. Baker for the NPI approach.
#' 
#' @seealso \code{\link{predict.imptree}}, \code{\link{summary.imptree}},
#' \code{\link{imptree_params}}, \code{\link{imptree_control}}
#' 
#' @keywords tree
#'
#' @export
imptree.formula <- function(formula, data = NULL,
                            weights, control, method = c("IDM", "NPI", "NPIapprox"),
                            method.param, ...) {

  # making sure a valid formula is supplied  
  if(missing(formula) || !inherits(formula, "formula")) {
    stop("Please supply a valid formula")
  }
  
  # function call
  Call <- match.call()
  
  # generating a suitable data set representation
  dataset <- prepare_data(object = formula, data = data,
                            weights = weights, ...)

  # checking for the calculation method
  method <- match.arg(method)
  methodint <- as.integer(pmatch(method, c("IDM", "NPI", "NPIapprox")) - 1)
  
  # checking for the method specific parameters
  method.param <- imptree_params(method.param, method)
  
  # initializing the control arguements such as minbucket, depth, gamma
  # and tbase, they may also be supplied via '...'
  if (missing(control)) {
    control <- NULL
  }
  controls <- imptree_control(splitmetric = method.param$splitmetric, controlList = control, ...)

  # combine all controls and method parameters into a single list
  controlslist <- c(controls, method.param)
  controlslist$iptype <- methodint
  
  # init java tree 
  tree_cpp_object <- treebuilder_cpp(dataset, controlslist)

  # changing back the call to 'imptree'
  Call[[1]] <- as.name("imptree")
  # construction of the structure of object 'imptree'
  res <- list(call = Call,
              tree = tree_cpp_object,
              traindata = dataset,
              formula = formula(terms(formula, data = dataset)))
  # assigning class
  class(res) <- c("imptree")
  # return value
  res
}


#' @rdname imptree
#' @param x A data.frame or a matrix of feature variables. The columns are required to be named.
#' @param y A response vector as a factor.
#' @export
imptree.default <- function(x, y, ...) {
  
  if(missing(y) || missing(x)) {
    stop("Both y and x need to be supplied")
  }
  
  if(is.null(nam <- colnames(x))) {
    stop("x must contain column names")
  }
  formula <- as.formula(paste0(deparse(substitute(y)),"~",
                               paste(nam, collapse = "+")))
  res <- imptree.formula(formula, data = x, ...)
  call <- match.call()
  call[[1]] <- as.name("imptree")
  res$Call <- call
  res  
}


#' @rdname imptree
#' @export
imptree <- function(x, ...) {
  UseMethod("imptree")
}

#' @useDynLib imptree
#' @importFrom Rcpp sourceCpp
NULL