#' @title Plot Grid
#' @author Thomas Bryce Kelly
#' @description Plot a section object.
#' @keywords Section Plotting
#' @export
plotGrid = function(section,
                        xlim = NULL,
                        ylim = NULL,
                        xlab = 'x',
                        ylab = 'y',
                        zlim = NULL,
                        pal = 'greyscale',
                        rev = F,
                        N = 16,
                        ...) {

  ## Handy variables
  x = section$x
  y = section$y
  z = section$z

  if (is.null(zlim)) { zlim = range(pretty(z)) }
  if (is.null(xlim)) { xlim = range(pretty(x))}
  if (is.null(ylim)) { ylim = range(pretty(y)) }

  ## Plot iamge
  image(x = x, y = y, z = z, col = get.pal(N, pal = pal, rev = rev), ylab = ylab, xlab = xlab,
        xlim = xlim, ylim = ylim, zlim = zlim, ...)

  box() ## make sure plotting didn't cover bounding box
}
