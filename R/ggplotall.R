#' Main plotter function
#'
#' @param df dataframe (tibble) to generate plot from
#' @param xval name of x data column
#' @param yval name of y data column
#' @param Pop vector of pops drawn from
#' @param xl x axis label (text)
#' @param yl y axis label (text)
#' @param title title for graph
#' @param labs text labels for pops
#' @param colrs colors for pops
#' @param text text for plotly
#' @param force internal value for repel
#' @param force_pull internal value for repel
#' @param max.overlaps internal value for repel
#'
#' @return returns ggplot graph (or paints it)
#' @export
#'
#' @examples
#' print("test")
ggplotall <- function(df, xval, yval, Pop = allidv_gen@pop, xl = xval, yl = yval, title = "",
                      labs = PopLabs, colrs = ColLabs, text = "", force = 3, force_pull = 1,
                      max.overlaps = 200){
  newdf <- df %>% select(xval = xval, yval = yval) %>% add_column(Pop = Pop) %>% mutate(text = text)
  newdf <- newdf %>% group_by(Pop) %>% mutate(Num = n()) %>% arrange(desc(Num), Pop) %>% ungroup()
  xvals <- newdf %>% select(xval) %>% unlist %>% as.numeric
  yvals <- newdf %>% select(yval) %>% unlist %>% as.numeric
  Pops <- newdf %>% select(Pop) %>% unlist# %>% factor(levels = lablevs)
  txt <- newdf %>% select(text) %>% unlist
  ggplot(newdf) + aes(x = xvals, y = yvals, fill = factor(Pops, levels = labs),
                      label = factor(Pops, levels = labs), text = txt) +
    geom_point(size = 4, alpha = 0.85, shape = 21, colour = "black") +
    repel_all(xvals, yvals, Pops, labs, txt, force, force_pull, max.overlaps = max.overlaps) +
    xlab(xl) + ylab(yl) + ggtitle(title) + #scale_x_continuous(breaks = c(0))+ scale_y_continuous(breaks = c(0)) +
    theme_warramaba(vals = colrs)
}
