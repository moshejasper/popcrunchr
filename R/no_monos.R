#' Process dataframe from upstream processing
#'
#' @param rawdata dataframe input
#' @param na_val value coding NA data in dataframe
#'
#' @return returns filtered dataset (removing monomorphic sites)
#' @export
#'
#' @examples
#' print("test")
no_monos <- function(rawdata, na_val = "000000") { # this function deals with a filtering issue
  indx <- sapply(rawdata, function(x) {
    y <- x[x != na_val]
    z <- length(unique(y))
    return(z > 1)
  })
  return(rawdata[indx])
}
