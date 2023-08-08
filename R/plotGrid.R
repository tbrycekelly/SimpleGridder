#' @title Plot Grid
#' @author Thomas Bryce Kelly
#' @description Plot a section object.
#' @keywords Section Plotting
#' @export
#' @param section: section object (2D only) to be plotted.
#' @param  field Parameter name to be plotted (z-axis).
#' @param  xlim Limits of the plot, by default the limits are set to the limits of the section grid.
#' @param  tlim Limits of the plot, by default the limits are set to the limits of the section grid.
#' @param  xlab X-axis label.
#' @param  ylab Y-axis label.
#' @param  log Flag for turning on log transformation of the z-axis. If TRUE then z = log(z) is performed prior to plotting.
#' @param  base Used when log = TRUE to set the base of the log.
#' @param  zlim The zlim imposed on the z-axis, which by default is set to the range of z values in the data.
#' @param  pal The color palette used; should be modeled after get.pal().
#' @param  rev Boolean for reversing the palette colors.
#' @param  include.data Flag for whether the data used to construct the grid should be plotted using the same palette.
#' @param  mark.points Flag for marking the location of each sample used to construct the grid.
#' @param  include.pch The pch used for when mark.points = TRUE.
#' @param  include.cex Sets the point size for when either include.data or mark.points are TRUE. (Used for both).
#' @param  main The title text to be included in the top line of the plot. Defaults to the name of the field.
#' @param  col.low [unimplemented] The color used when plotting out-of-range z values on the low end. Default value of '' indicates to use the minimum value of pal(). A value of NA skips the plotting of these data. Otherwise the color given is used (e.g. col.low = 'blue').
#' @param  col.high [unimplemented] Same as col.low but for out-of-range high values.
plotGrid = function(section, field = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        by.lon = F,
                        by.lat = F,
                        xlab = 'x',
                        ylab = 'y',
                        log = FALSE,
                        base = 10,
                        zlim = NULL,
                        pal = 'greyscale',
                        rev = F,
                        include.data = F,
                        mark.points = F,
                        include.pch = 21,
                        include.cex = 1,
                        main = NULL,
                        col.low = '',
                        col.high = '',
                        N = 255,
                        indicate = T,
                        ...) {

  ## Set Defaults
  # Check if values need to set and set them.
  if (is.null(field)) {
    field = colnames(section$grid)[3]
    warning('No field name provided, using first gridded data: ', field)
  }

  ## Handy variables
  x = section$x
  y = section$y
  z = matrix(section$grid[,field], nrow = length(x))
  if (by.lon) {
    if (verbose) { message(' Plotting based on longitude.')}
    x = approx(section$x, section$lon, xout = x, rule = 2)$y
  }
  if (by.lat) {
    if (verbose) { message(' Plotting based on latitude.')}
    x = approx(section$x, section$lat, xout = x, rule = 2)$y
  }


  if (log) {
    z = log(z, base)
    if (is.null(zlim)) {
      zlim = range(pretty(z), na.rm = TRUE)
    }
  }

  if (is.null(zlim)) { zlim = range(pretty(z), na.rm = TRUE) }

  if (is.null(main)) { main = field }
  if (is.null(xlim)) { xlim = range(x, na.rm = T)}
  if (is.null(ylim)) { ylim = range(y, na.rm = T) }

  ## Out of Range
  # Set values to zlim if you want the out of range values plotted at the zlim values
  if (!is.na(col.low) & col.low == '') {
    z[z < zlim[1]] = zlim[1]
  }
  if (!is.na(col.high) & col.high == '') {
    z[z > zlim[2]] = zlim[2]
  }

  ## Plot iamge
  image(x = x, y = y, z = z, col = get.pal(N, pal = pal, rev = rev), ylab = ylab, xlab = xlab,
        xlim = xlim, ylim = ylim, zlim = zlim, ...)

  # Plot points that are out of range when a color is given
  if (!is.na(col.low) & col.low != '') {
    zz = z
    zz[zz >= zlim[1]] = NA
    image(x = x, y = y, z = zz, col = col.low, add = TRUE)#, pch = include.pch, cex = include.cex)
  }

  if (!is.na(col.high) & col.high != '') {
    zz = z
    zz[zz <= zlim[2]] = NA
    image(x = x, y = y, z = zz, col = col.high, add = TRUE)#, pch = include.pch, cex = include.cex)
  }

  if (mark.points) {
    points(x = section$data$x, y = section$data$y, pch = 20, cex = include.cex) ## Add black points
  }
  if (include.data) {
    col = make.pal(section$data$z[,field], pal = pal, n = N, min = zlim[1], max = zlim[2], rev = rev)
    points(x = section$data$x, y = section$data$y, pch = include.pch,
           cex = include.cex, col = col, bg = col)
  }

  ## Add Title text
  if (indicate) {
    if (log) {
      st = paste0(main, '   zlim: (', round(base^zlim[1], 3), ', ', round(base^zlim[2],3), ')')
    } else {
      st = paste0(main, '   zlim: (', round(zlim[1], 3), ', ', round(zlim[2],3), ')')
    }
    mtext(st, line = 0.25, adj = 1, cex = 0.7)
  }
  box() ## make sure plotting didn't cover bounding box
}
