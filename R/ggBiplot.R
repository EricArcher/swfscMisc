#' @title ggBiplot
#' @description Create a biplot of a Principal Components Analysis using ggplot2.
#' 
#' @param pca result from a call to \code{\link{princomp}} or \code{\link{prcomp}}.
#' @param pcs a two element vector giving the number or column names of 
#'   the components to plot.
#' @param mult.fac multiplier factor for positions of points relative to extent of
#'   plot (\code{0:1}).
#' @param arrow.size thickness of arrow lines. If \code{NULL} no arrows are shown.
#' @param label.size size of labels.
#' @param plot show the plot?
#' 
#' @return the ggplot2 object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' pca.princomp <- princomp(USArrests, cor = TRUE)
#' ggBiplot(pca.princomp)
#' 
#' pca.prcomp <- prcomp(USArrests, cor = TRUE)
#' ggBiplot(pca.prcomp)
#' 
#' @export
#' 


ggBiplot <- function(
    pca, pcs = c(1, 2), mult.fac = 0.8, arrow.size = NULL, label.size = 3, 
    plot = TRUE
) {
  
  if(is.numeric(pcs)) {
    pcs <- switch(
      class(pca), 
      prcomp = colnames(pca$x)[pcs],
      princomp = colnames(pca$scores)[pcs]
    )
  }
  
  scores <- switch(
    class(pca),
    prcomp = pca$x,
    princomp = pca$scores 
  ) |> 
    as.data.frame() |> 
    dplyr::select(dplyr::all_of(pcs)) |> 
    stats::setNames(c('x', 'y'))
  
  loadings <- switch(
    class(pca),
    prcomp = pca$rotation,
    princomp = cbind(pca$loadings)
  ) |> 
    as.data.frame() |> 
    dplyr::select(dplyr::all_of(pcs)) |> 
    stats::setNames(c('x', 'y')) |> 
    tibble::rownames_to_column('variable')
  
  mult.x <- max(scores$x) - min(scores$x) / diff(range(loadings$x))
  mult.y <- max(scores$y) - min(scores$y) / diff(range(loadings$y))
  
  loadings <- dplyr::mutate(
    loadings,
    x = .data$x * mult.x,
    y = .data$y * mult.y,
    origin = 0
  )
  
  g <- scores |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) + 
    ggplot2::geom_hline(yintercept = 0) + 
    ggplot2::geom_vline(xintercept = 0) + 
    ggplot2::geom_point(color = "grey", alpha = 0.7) + 
    ggplot2::geom_point(
      data = loadings,
      color = 'red',
      shape = 8
    ) + 
    ggplot2::labs(x = pcs[1], y = pcs[2])
  
  if(!is.null(arrow.size)) {
    g <- g +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data$origin, 
          xend = .data$x, 
          y = .data$origin, 
          yend = .data$y
        ),
        data = loadings,
        color = "red",
        linewidth = arrow.size,
        arrow = ggplot2::arrow()
      )
  }
  
  g <- g + ggrepel::geom_label_repel(
    ggplot2::aes(label = .data$variable), 
    data = loadings, 
    color = "red", 
    size = label.size
  ) 
  
  if(plot) print(g)
  invisible(g)
}