#' @name imptree-package
#' 
#' @title imptree: Classification Trees with Imprecise Probabilities
#' 
#' @description The \code{imptree} package implements the creation of 
#' imprecise classification trees based on algorithm developed by 
#' Abellan and Moral. 
#' The credal sets of the classification variable within each node
#' are estimated by either the Imprecise Dirichlet Model (IDM) or the 
#' Nonparametric Predictive Inference (NPI).
#' As split possible split criteria serve the 'information gain', 
#' based on the maximal entropy distribution, and the adaptable 
#' entropy-range based criterion propsed by Fink and Crossman.
#' It also implements different correction terms for the entropy.
#' 
#' The performance of the tree can be evaluated with respect to the
#' common criteria in the context of imprecise classification trees.
#' 
#' It also provides the functionality for estimating credal sets via 
#' IDM or NPI and obtain their minimal/maximal entropy (distribution) 
#' to be used outside the tree growing process.
#' 
#' 
#' @references Abell\ifelse{latex}{\out{\'{a}}}{\ifelse{html}{\out{&aacute;}}{a}}n,
#' J. and Moral, S. (2005), Upper entropy of credal sets. Applications to 
#' credal classification, \emph{International Journal of Approximate Reasoning}
#' \bold{39}, pp. 235--255.
#' @references Baker, R. M. (2010), \emph{Multinomial Nonparametric Predictive Inference:
#' Selection, Classification and Subcategory Data}, PhD thesis. Durham University, GB.
#' @references Strobl, C. (2005), Variable Selection in Classification Trees Based on
#' Imprecise Probabilities, \emph{ISIPTA '05: Proceedings of the Fourth
#' International Symposium on Imprecise Probabilities and Their Applications},
#' 339--348.
#' @references Fink, P. and Crossman, R.J. (2013), Entropy based classification trees,
#' \emph{ISIPTA '13: Proceedings of the Eighth International Symposium on Imprecise
#' Probability: Theories and Applications}, pp. 139--147.
#' 
#' @seealso
#' \code{\link{imptree}} for tree creation, \code{\link{probInterval}} for the credal set
#' and entropy estimation functionality
#' 
#' @keywords tree
#' 
#' @docType package
#' 
#' @useDynLib imptree
#' @importFrom Rcpp sourceCpp
NULL
