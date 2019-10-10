#' @title One Way Analysis of Variance
#' @description \code{oneway} computes a one way analysis of variance and includes group-level summary statistics.
#' @param formula an object of class formula, relating the dependent variable to the grouping variable
#' @param data a dataframe containing the variables in the model
#' @return a list with two elements:
#' \item{oneway}{a list with the lm results}
#' \item{summarystats}{a dataframe with the summary statistics}
#' @details
#' This function computes a standard oneway ANOVA, means, and standard deviation.
#' Missing values are handled via listwise deletion.
#' @examples
#' mileage<-oneway(hwy ~ class, cars)
#' summary(mileage)
#' print(mileage)
#' plot(mileage)
#' @author Rob Kabacoff <rkabacoff@@wesleyan.edu>
#' @rdname oneway
#' @export

oneway <- function(formula, data) {

  # listwise deletion of missing values
  data_complete <- na.omit(data)

  # anova
  fit <- lm(formula, data_complete)

  stats <-  aggregate(formula,
                      data,
                      function(x) c(n = length(x), mean = mean(x), sd = sd(x)))


  result <- list(anova = fit, summarystats = stats)
  class(result) <- c("oneway", "list")
  return(result)

}
