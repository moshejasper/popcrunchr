#' Van specific scaler
#'
#' @param ref_frame reference tibble for graph
#'
#' @return character vector for text to plotly
#' @export
#'
#' @examples
#' print("test")
scaler <- function(ref_frame){
  return(paste0(ref_frame$region, "\n", ref_frame$chr_race, "\n", ref_frame$pop,
                "\n", ref_frame$id))
}

#' Generic scaler
#'
#' @param ref_frame reference tibble for graph
#'
#' @return character vector for text to plotly
#' @export
#'
#' @examples
#' print("test")
scaler2 <- function(ref_frame){
  return(paste0(ref_frame$species, "\n", ref_frame$region,
                "\n", ref_frame$pop, "\n", ref_frame$id))
}

