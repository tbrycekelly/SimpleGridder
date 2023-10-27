#' @title Plot Grid
#' @author Thomas Bryce Kelly
#' @description Plot a section object.
#' @keywords Section Plotting
#' @export
plotGrid = function(section,
                    label,
                        xlim = NULL,
                        ylim = NULL,
                        xlab = 'x',
                        ylab = 'y',
                        zlim = NULL,
                        ztrim = NULL,
                        pal = greyscale(16),
                        ...) {

  ## Handy variables

  x = section$x
  y = section$y
  z = section$interp[[label]]

  if (is.null(zlim)) { zlim = range(pretty(z)) }
  if (is.null(xlim)) { xlim = range(pretty(x))}
  if (is.null(ylim)) { ylim = range(pretty(y)) }
  if (is.null(ztrim)) { ztrim = zlim }

  z[z < zlim[1]] = ztrim[1]
  z[z > zlim[2]] = ztrim[2]

  ## Plot image
  image.default(x = x, y = y, z = z, col = pal, ylab = ylab, xlab = xlab,
        xlim = xlim, ylim = ylim, zlim = zlim, ...)

  box() ## make sure plotting didn't cover bounding box
}
