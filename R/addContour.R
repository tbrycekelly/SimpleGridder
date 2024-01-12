#' @title Add Contour to Section Plot
#' @author Thomas Bryce Kelly
#' @param section a section object from "build.section()"
#' @param field the field name to use for contouring
#' @param levels the level(s) to draw contours at
#' @param col the color of the contour line(s)
#' @param lty the lty for the line(s)
#' @param labels a boolean for whether the label(s) should be drawn
#' @param cex.lab the cex (i.e. size) of the labels
#' @export
addContour = function(section, field = NULL, levels = NULL, col = 'black', lty = 1, lwd = 1, labels = NULL, cex.lab = 1) {
  if (is.null(field)) {
    field = colnames(section$interp)[1] # first interpolated field
  }
  z = matrix(section$interp[[field]], nrow = length(section$x))
  if (is.null(levels)) { levels = pretty(range(as.numeric(z), na.rm = T), n = 5) }

  contour(section$x, section$y, z, add = TRUE, levels = levels, col = col, lty = lty, lwd = lwd,
          labels = labels, labcex = cex.lab, drawlabels = !isFALSE(labels))
}
