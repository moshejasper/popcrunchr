f1 <- function(vec) {
  m <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- m
  return(vec)
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

f3 <- function(vec) {
  if (sum(! is.na(vec)) > 0){
    m <- mean(vec, na.rm = TRUE)
    vec[is.na(vec)] <- m
    return(vec)}
  else {
    print("NAs not removed")
    return(vec)
  }
}

f4 <- function(vec) {
  if (sum(! is.na(vec)) > 0){
    m <- getmode(vec[! is.na(vec)])
    vec[is.na(vec)] <- m
    return(vec)}
  else {
    print("NAs not removed")
    return(vec)
  }
}
