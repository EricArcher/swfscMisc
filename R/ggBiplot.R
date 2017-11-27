#' @title ggBiplot
#' @description Plot a biplot of a Principal Components Analysis using ggplot2.
#' 
#' @param pca result from a call to \code{\link{princomp}}.
#' @param x,y the number or column names of the components to plot.
#' @param mult.fac multiplier factor for lengths of arrows from \code{0:1}.
#' @param arrow.size thickness of arrow lines.
#' @param label.size size of labels.
#' 
#' @return the ggplot2 object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' pc.cr <- princomp(USArrests, cor = TRUE)
#' ggBiplot(pc.cr)
#' 
#' @export
#' 
ggBiplot <- function(pca, x = 1, y = 2, mult.fac = 0.8, arrow.size = 1.5, label.size = 6) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(ggrepel)
  })
  if(is.numeric(x)) x <- colnames(pca$scores)[x]
  if(is.numeric(y)) y <- colnames(pca$scores)[y]
  
  scores <- as.data.frame(pca$scores) %>% 
    select_(x, y) %>% 
    setNames(c("x", "y"))
  
  mult <- min(
    (max(scores[, 1]) - min(scores[, 1]) / diff(range(pca$loadings[, x]))),
    (max(scores[, 2]) - min(scores[, 2]) / diff(range(pca$loadings[, y])))
  ) * mult.fac
  
  ldngs <- cbind(pca$loadings) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    select(rowname, x, y) %>% 
    setNames(c("rowname", "x", "y")) %>% 
    mutate(
      x = x * mult,
      y = y * mult,
      origin = 0
    ) %>% 
    column_to_rownames()
  
  g <- ggplot(scores, aes(x, y)) + 
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) +
    geom_point(color = "grey", alpha = 0.7) + 
    geom_segment(
      aes(x = origin, xend = x, y = origin, yend = y), 
      data = ldngs, 
      color = "red", 
      size = arrow.size, 
      arrow = arrow()
    ) +
    geom_label_repel(
      aes(x = x, y = y, label = rownames(ldngs)), 
      data = ldngs, 
      color = "red", 
      size = label.size
    ) +
    labs(x = x, y = y)
  print(g)
  
  invisible(g)
}