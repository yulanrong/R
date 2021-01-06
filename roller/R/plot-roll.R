#' @title Plot of object roll
#' @description Plots the relative frequencies of a series of rolls
#' @param x an object of class \code{"device"}
#' @param y number of total rolls
#' @export
#' @examples
#'  \dontrun{
#'  # create an object and roll it 50 times
#'  fair_die <- device(sides = 1:6, prob = rep(1/6, 6))
#'  fair_50rolls <- roll(fair_die, times = 50)
#'
#'  plot(fair_50rolls)
#'  }

plot.rolls <- function(x, y) {
  dat2 <- table(x$roll) / x$total
  y <- x$total
  barplot(dat2,
          main = paste0("Relative Frequencies in a series of ", y , "rolls"),
          xlab = "sides of device", ylab = "relative frequencies",
          las = 1, border = FALSE)
}
