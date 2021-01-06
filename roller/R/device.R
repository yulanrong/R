#' @title Device
#' @description Creates an R object of class \code{"device"}
#' @param sides vector of k ≥ 2 elements, by default numbers 1 and 2.
#' @param prob vector of probabilities for each side (all equal to 1/2 by default).
#' @return dev an object of class \code{"device"}
#' @seealso \code{\link{roll}}
#' @export
#' @examples
#' # default
#' fair_coin <- device()
#'
#' # another device
#' weird_die <- device(sides = c('i', 'ii', 'iii', 'iv'), prob = rep(1/4, 4))
#'
#' #loaded device
#' loaded_die <- device(sides = 1:6, prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
#'

device <- function(sides = c(1, 2), prob = rep(0.5, 2)) {
  check_sides(sides)
  check_prob(prob)

  if (length(sides) != length(prob)) {
    stop("'sides' and 'prob' have different lengths")
  }
  else{
    dev <- list(sides = sides, prob = prob)
    class(dev) <- "device"
  }
  return(dev)
}


# auxiliary function to check the validity of the argument sides.
check_sides <- function(sides) {
  if(length(sides)<=1) {
    stop("'sides' must be a vector of length greater than 1")
    }
  else if (anyDuplicated(sides) != 0) {
    stop("'sides' cannot have duplicated elements")
  }
  else { TRUE }
}


# auxiliary function to check the validity of the argument prob.
check_prob <- function(prob) {
  if(is.numeric(prob) == FALSE) { stop("'prob' must be a numeric vector") }
  else if(sum(prob) != 1) { stop("elements in 'prob' must add up to 1") }
  else if (any(prob < 0) | any(prob > 1)) { stop("'prob' must be between 0 and 1") }
  else { TRUE }
}


#' @export
print.device <- function(x, ...) {
  cat('object "device"\n\n')
  dat <- data.frame(
    side = x$sides, prob = x$prob
  )
  print(dat)
  invisible(x)
}


#' @rdname device
#' @param x an R object
#' @export
is.device <- function(x) {
  if(class(x) == "device") {return(TRUE)}
  else {return(FALSE)}
}
