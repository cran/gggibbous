#' gggibbous: Moon charts, a pie chart alternative for two groups
#'
#' Moon charts are like pie charts except that the proportions are shown as
#' crescent or gibbous portions of a circle, like the lit and unlit portions of
#' the moon. As such, they work best with only one or two groups.
#' \code{gggibbous} extends \code{ggplot2} to allow for plotting multiple moon
#' charts in a single panel and does not require a square coordinate system.
#' 
#' The workhorse function is \code{geom_moon}, which adds a moon chart layer to
#' a ggplot2 plot.
#' The \code{draw_key_moon}, \code{draw_key_moon_left}, and
#' \code{draw_key_full_moon} functions provides legend key glyphs for plots that
#' use \code{geom_moon}.
#' There are also functions for the raw \code{grid} grobs: \code{grid.moon} and
#' \code{moonGrob}.
#' 
#' For more information, see the \code{gggibbous} vignette.
#' 
#' @docType package
#' @name gggibbous
NULL