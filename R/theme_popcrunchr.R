#' popcrunchr theme
#'
#' @param vals list of values for fill scale
#'
#' @return returns scale_fill_manual list for use with ggplot
#' @export
#'
#' @examples
#' theme_popcrunchr()
theme_popcrunchr <- function(vals = ColLabs){
  return(list(scale_fill_manual(values = vals, name = "Location"), theme_bw(),
              theme(panel.background = element_rect(fill = "white"), legend.position = "none",
                    plot.title = element_text(hjust = 0.5, size = 20), axis.title = element_text(size = 16),
                    panel.grid.minor = element_line(), panel.grid.major = element_line())))}

