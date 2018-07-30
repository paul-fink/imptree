#' @docType data
#' @name car_acceptance
#' 
#' @title Car Evaluation Database
#' 
#' @description
#' This is the 'Car Evaluation' data set from 
#' the UCI Machine Learning Repository.
#' \cr
#' The Car Evaluation Database gives the acceptance 
#' of a car directly related to the six input attributes:
#' buying, maint, doors, persons, lug_boot, safety.
#' 
#' @usage data(car_acceptance)
#' 
#' @format
#' A data frame with 1728 observations on the following 7 variables,
#' where each row contains information on one car.
#' All variables are factor variables.
#' \describe{
#'   \item{\code{buying}}{Buying price of the car
#'         (Levels: \code{high}, \code{low}, \code{med} ,\code{vhigh})}
#'   \item{\code{maint}}{Price of the maintenance
#'         (Levels: \code{high}, \code{low}, \code{med}, \code{vhigh})}
#'   \item{\code{doors}}{Number of doors
#'         (Levels: \code{2}, \code{3}, \code{4}, \code{5more})}
#'   \item{\code{persons}}{Capacity in terms of persons to carry
#'         (Levels: \code{2}, \code{4}, \code{more})}
#'   \item{\code{lug_boot}}{Size of luggage boot
#'         (Levels: \code{big}, \code{med}, \code{small})}
#'   \item{\code{safety}}{Estimated safety of the car
#'         (Levels: \code{high}, \code{low}, \code{med})}
#'   \item{\code{acceptance}}{Acceptance of the car (target variable)
#'         (Levels: \code{acc}, \code{good}, \code{unacc}, \code{vgood})}
#' }
#' 
#' @details 
#' Car Evaluation Database was derived from a simple hierarchical
#' decision model originally developed for the demonstration of DEX. 
#'
#'  The model evaluates cars according to the following concept structure:
#'  CAR                      car acceptability
#'  . PRICE                  overall price
#'  . . buying               buying price
#'  . . maint                price of the maintenance
#'  . TECH                   technical characteristics
#'  . . COMFORT              comfort
#'  . . . doors              number of doors
#'  . . . persons            capacity in terms of persons to carry
#'  . . . lug_boot           the size of luggage boot
#'  . . safety               estimated safety of the car
#'
#' Input attributes are printed in lowercase. Besides the target
#' concept (CAR), the model includes three intermediate concepts:
#' PRICE, TECH, COMFORT. 
#'
#' The Car Evaluation Database contains examples with the structural 
#' information removed, i.e., directly relates CAR to the six input 
#' attributes: buying, maint, doors, persons, lug_boot, safety.
#' 
#' @source 
#' The hierarchical decision model, from which this dataset is 
#' derived, was first presented in 
#' 
#' M. Bohanec and V. Rajkovic (1988), Knowledge acquisition and explanation for 
#' multi-attribute decision making, \emph{8th Intl. Workshop on Expert 
#' Systems and their Applications}, Avignon, France, 59--78.
#' 
#' The original data is at \url{https://archive.ics.uci.edu/ml/datasets/Car+Evaluation}.
#' 
#' @keywords datasets
NULL