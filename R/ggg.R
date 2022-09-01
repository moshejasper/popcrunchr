#' Pass ggplotall to plotly (basic)
#'
#' @param a item to pass
#' @param b fields to pass
#'
#' @return  returns ggplotly viewer object
#' @export
#'
#' @examples
#' print("test")
gg <- function(a = (ggplot2::last_plot() + theme_bw()), b = c("all")){
  a %>% ggplotly(tooltip = b)
}

#' Pass ggplotall to plotly (trimmed)
#'
#' @param a item to pass
#' @param b field to pass
#'
#' @return returns ggplotly viewer object
#' @export
#'
#' @examples
#' print("test")
ggg <- function(a = (ggplot2::last_plot() + theme_bw()), b = c("text")){
  a %>% ggplotly(tooltip = b)
}
