#' @title Box plot for One Way ANOVA
#' @description Create a box plot of an oneway object
#' @param x object of class \code{oneway}
#' @param ... parameters passed to the boxplot function
#' @return NULL
#' @examples
#' mileage<-oneway(hwy ~ class, cars)
#' plot(mileage)
#' @rdname plot.oneway
#' @export

plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  boxplot(x$anova$terms, x$anova$model, col="skyblue", ...)
}
