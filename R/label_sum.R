#' Label sum
#'
#' @param Pop vector
#' @param xvals vector
#' @param yvals vector
#' @param text vector
#'
#' @return tibble of processed values for use in graphing
#' @export
#'
#' @examples
#' print("none")
label_sum <- function(Pop, xvals, yvals, text) {
  start_tibble <- tibble(Pop = Pop, xvals = xvals, yvals = yvals, text = text)
  end_tibble_a <- start_tibble %>% group_by(Pop) %>% summarise(xvals = median(xvals), yvals = median(yvals)) %>%
    mutate(Poplab = Pop, text = "") %>% ungroup
  end_tibble_b <- start_tibble %>% mutate(Poplab = "")
  end_tibble <- end_tibble_a %>% add_row(end_tibble_b)
  print(end_tibble)
  return(end_tibble)
}


#' Helper function for the main game (ggplotall)
#'
#' @param xvals x axis values
#' @param yvals y axis values
#' @param Pop pop list
#' @param levs pop levs
#' @param text text for plotly functions
#' @param force for geom_repel
#' @param force_pull for geom_repel
#' @param max.overlaps for geom_repel
#'
#' @return returns repel object
#' @export
#'
#' @examples
#' print("none")
repel_all <- function(xvals, yvals, Pop = allidv_gen@pop, levs = PopLevs, text = "", force = 3, force_pull = 1, max.overlaps = 200) {
  return(geom_label_repel(data = label_sum(Pop, xvals, yvals, text),
                          mapping = aes(x = xvals, y = yvals, fill = factor(Pop, levels = levs), label = Poplab, text = text),
                          force = force, force_pull = force_pull, alpha = 0.85, size = 3, segment.alpha = 0.5, max.overlaps = max.overlaps))
} # size used to be 5
