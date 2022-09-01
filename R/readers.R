#' Read raw gendata (from mj pipeline)
#'
#' @param filename path to input raw gendata file
#'
#' @return returns tibble of nomono-filtered data, sorted by Indv (matches targetid)
#' @export
#'
#' @examples
#' print("test")
read_genraw <- function(filename){
  return(read_tsv(filename) %>% no_monos %>% arrange(Indv))
}

#' Read genref (from mj pipeline)
#' Still missing regionr & relevlr steps... include later
#'
#' @param filename path to impot genref file
#'
#' @return returns reference tibble (matched to idv)
#' @export
#'
#' @examples
#' print("test")
read_genref <- function(filename){
  return(read_tsv(filename) %>% arrange(targetid))
}
