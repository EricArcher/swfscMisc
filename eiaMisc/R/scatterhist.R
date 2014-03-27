#' @rdname scatterdens

scatterhist <- function(x, y, xlab = "", ylab = "", dens.frac = 1/5, ...) {
  zones <- matrix(c(2,0,1,3), ncol = 2, byrow = TRUE)
  layout(zones, widths = c(1 - dens.frac, dens.frac), heights = c(dens.frac, 1 - dens.frac))
  xhist <- hist(x, plot = FALSE)
  yhist <- hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  par(mar = c(3, 3, 1, 1))
  plot(x, y, ...)
  par(mar = c(0, 3, 1, 1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  par(mar = c(3, 0, 1, 1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
  par(oma = c(3, 3, 0, 0))
  mtext(xlab, side = 1, line = 1, outer = TRUE, adj = 0, 
        at = (.8 * (mean(x) - min(x)) / (max(x) - min(x))))
  mtext(ylab, side = 2, line = 1, outer = TRUE, adj = 0, 
        at = (.8 * (mean(y) - min(y)) / (max(y) - min(y))))
}