\name{imptree}
\alias{imptree}
\alias{imptree.formula}
\alias{imptree.default}
\alias{print.imptree}

\title{Classification with Imprecise Probabilities}
\description{
  \code{imptree} implements Abellan and Moral's tree
  algorithm (based on Quinlans ID3) for classification.  It
  employes either the Imprecise Dirichlet Model (IDM) or the 
  Nonparametric Predictive Inference to generate the nodes. 
}
\usage{
\method{imptree}{default}(x, y, \dots)
\method{imptree}{formula}(formula, data = NULL, testdata = NULL, 
  na.action = na.pass, control, gamma = 1, Tbase = 1,
  method = c("IDM", "NPI"), method.param, \dots)
\method{print}{imptree}(x, \dots)
}
\arguments{
  \item{data}{an optional data frame containing the variables in the model.
    By default the variables are taken from the environment which
    \code{imptree} is called from.}
  \item{x, formula}{a data frame or a matrix of features, or a formula
    describing the model to be fitted (for the
    \code{print} method, an \code{imptree} object).}
  \item{y}{A response vector, being a factor.}
  \item{testdata}{A \code{data.frame} for the test set. (NOTE: If given it 
    needs to contain both the class and features)}
  \item{control}{a named list according the result of \code{\link{imptree_control}}.
    May be a subset of the list}
  \item{gamma}{The weighting factor of the pessimism when 
    calculating the t-value. Setting to 1 (Default) is weakest restriction}
  \item{na.action}{A function to specify the action to be taken if NAs
    are found. (Default: na.pass) (NOTE: If given, this argument must be named.)}
  \item{Tbase}{The minimum t-value that needs to be attained to qualify for a split.}
  \item{method}{Method applied for calculating the probability intervals of the
    class probability. \code{"IDM"} for the Imprecise Dirichlet Model (Default) and
    \code{"NPI"} for use of the Nonparametric Predictive Inference approach.}
  \item{method.param}{Named list specifying the mehtod specific parameter. See details}
  \item{...}{optional parameters to be passed to the main function
    \code{imptree.formula} or to the call of \code{\link{imptree_control}}.}
}

\value{
  An object of class \code{imptree}, which is a list with the
  following components:
  \item{Call}{the original call to \code{imptree}}
  \item{Tree}{A list containing the tree structure.}
  \item{Data}{The training data used to construct the tree.}
  \item{formula}{The formula describing the data structure, 
    which was used in fitting.}
  \item{Accuracy}{Accuracy achieved on training data. \code{NULL} if \code{calc.acc = FALSE}.}
  \item{Test.result}{Accuracy achieved on supplied test data. \code{NULL} if \code{testdata = NULL}.}
  \item{Splitnames}{list of the names of the feature variables used for splitting.}
}

\details{
  For \code{method = "IDM"}, \code{method.param} takes the following inputs:
  \itemize{
    \item{s}{: Hyperparamter of the Imprecise Dirichlet Model (\code{s >= 0}), see below.}
    \item{correction}{: Entropy correction to be carried out, see below (Default \code{"no"}).}
  }
  The hyperparamter \code{s} of the Imprecise Dirchlet Model (IDM) may be given as any 
  non-negative value.  It defines the impression the locally applied IDMs introduce.  
  With increasing values of {s} more impression is added.  For \code{s=0} the IDM
  collapses to a Precise Dirichlet Model. 
  
  To account for a varying number of categories of the splitting candidates Strobl proposed the
  use of a correction based on the Miller-entropy correction: \code{correction = "strobl"}. 
  In their work Abellan and Moral favoured the use of a generalized Hartley measure so that
  final measure may be viewed as measure of total uncertainty: \code{correction = "abellan"}.

  For \code{method = "NPI"}, \code{method.param} takes the following inputs:
  \itemize{
    \item{maxEntropyMethod}{: Method for calculating the maximum entropy distribution, see below (Default \code{"approx"}).}
    \item{correction}{: Entropy correction to be carried out, see below (Default \code{"no"}).}
  }
  For calculating the maximum entropy distribution one can apply the approximate method, 
  which usually gives a distribution with higher entropy, but collides with the NPI underlying 
  graphical model of a probability wheel. To obtain the distribution with maximum probability 
  complying with the restrictions enforced by the wheel, \code{method = "exact"} may be used.

  Similarily to the IDM a correction fo the entropy may be obtained, based on the Miller-entropy
  correction: \code{correction = "strobl"}.
}

\references{
  \ifelse{latex}{\out{Abell\'{a}n, J. and Moral, S.}}{\ifelse{html}{\out{Abell&aacute;n, J. and Moral, S.}}{Abellan, J. and Moral, S.}} (2005), Upper entropy of credal sets. Applications to credal classification, \emph{International Journal of Approximate Reasoning} \bold{39}, 235--255.

  Strobl, C. (2005), Variable Selection in Classification Trees Based on Imprecise Probabilities, \emph{ISIPTA'05: Proceedings of the Fourth International Symposium on Imprecise Probabilities and Their Applications}, 339--348.
  
  Baker, R. M. (2010), \emph{Multinomial Nonparametric Predictive Inference: Selection, Classification and Subcategory Data}.

}

\author{Paul Fink \email{Paul.Fink@stat.uni-muenchen.de}, based on algorithms by
    \ifelse{latex}{\out{J. Abell\'{a}n and S. Moral}}{\ifelse{html}{\out{J. Abell&aacute;n and S. Moral}}{J. Abellan and S. Moral}} for the IDM and
    R. M. Baker for the NPI approach.
}

\seealso{\code{\link{predict.imptree}}, \code{\link{accuracy}}, \code{\link{imptree_control}}}

\examples{
  data(car_acceptance)
  # a subset of the data set
  car_sub <- car_acceptance[sample(1:NROW(car_acceptance), 100),]

  # almost default behaviour, increasing the imprecison employed
  itree1 <- imptree(acceptance ~ ., data = car_sub, 
    method = "IDM", method.param = list(s = 2))
  accuracy(itree1)

  # inlcuding the accuracy calculation in the tree growing
  # and further limit the tree to a depth of 2.
  #
  # It makes use of both passing the params to the 'control'
  # and the '...' argument
  itree2 <- imptree(acceptance ~ ., data = car_sub, 
    method = "IDM", control = list(calc.acc = TRUE), depth = 2)
  accuracy(itree2)
  itree2$Accuracy
}
\keyword{tree}
