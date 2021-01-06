#' @title Roll a device
#' @description Creates an object of class \code{"rolls"}
#' @param device object of class \code{"device"}
#' @param times number of rolls
#' @return output, an object of class \code{"rolls"}
#' @export
#' @examples
#' fair_die <- device()
#'
#' # roll a fair die 50 times
#' fair50 <- roll(fair_die, times = 50)
#'
#' # add 5 more rolls
#' fair55 <- fair50 + 5


roll <- function(device, times = 1) {
  if(class(device) != "device") {stop("the class of object must be 'device'.")}
  check_times(times)
  rolls <- sample(device$sides, size = times, replace = TRUE, prob = device$prob)
  output <- list(rolls = rolls, sides = device$sides, prob = device$prob, total = times)
  class(output) <- "rolls"
  return(output)
}


# private function to check vector of 'times'
check_times <- function(times){
  if (times < 1 | (times - round(times) != 0)) {
    stop("'times' must be a positive integer greater than or equal to 1.")
  }
  else {TRUE}
}


#' @export
print.rolls <- function(x, ...) {
  cat('object "rolls"\n\n$rolls\n')
  print(x$rolls)
  invisible(x)
}


#' @export
summary.rolls <- function(x, ...) {
  count <- as.vector(table(x$rolls))
  proportions <- count / x$total
  freqs <- data.frame(side = x$sides, count = count, prop = proportions)
  output2 <- list(freqs = freqs)
  class(output2) <- "summary.rolls"
  return(output2)
}


#' @export
print.summary.rolls <- function(x, ...) {
  cat('summary "rolls"\n\n')
  print(x$freqs)
  invisible(x)
}


# extraction method
#' @export
"[.rolls" <- function(x, i) {
  x$rolls[i]
}


# replacement method
#' @export
"[<-.rolls" <- function(x, i, value) {
  if (value %in% x$sides==FALSE) {
    stop("Invalid replacement value")
  }
  if(i > x$total) {
    stop("\nindex out of bounds.")
  }
  x$rolls[i] <- value
  return(x)
}


# addition method
#' @export
"+.rolls" <- function(x, added_value) {
  if (length(added_value) != 1 | added_value <= 0) {
    stop("\ninvalid increment (must be positive)")
  }
  else{
    dev <- device(x$sides, x$prob)
    more_rolls <- roll(dev, times = added_value)
    x$rolls <- c(x$rolls, more_rolls$rolls)
    x$total <- x$total + added_value
    return(x)
  }
}
