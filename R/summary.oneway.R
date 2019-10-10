#' @title Summary of One Way ANOVA
#' @description Prints the summary of a oneway ANOVA
#' @param x an object of class \code{oneway}
#' @param ... parameters passed to print function
#' @return NULL
#' @examples
#' mileage<-oneway(hwy ~ class, cars)
#' summary(mileage)
#' @rdname summary.oneway
#' @export

summary.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("x must  be class 'oneway'")
  print(anova(x$anova), ...)
}
