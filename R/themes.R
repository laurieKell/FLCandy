#' @title Theme for Beamer Presentations
#'
#' @description Creates a minimal theme optimized for Beamer presentations with scaled text sizes
#' and minimal space between panels
#'
#' @param scale Numeric scaling factor for text sizes, default is 1
#'
#' @return A ggplot2 theme object
#'
#' @details
#' The theme is based on theme_minimal with:
#' * Base text size of 18 (scaled)
#' * Axis and strip text size of 16 (scaled)
#' * Legend at bottom
#' * Minimal panel spacing
#' * Light gray panel borders
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_beamer()
#'   
#' # With larger text
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_beamer(scale = 1.2)
#' }
#'
#' @import ggplot2
#' @export
theme_beamer <- function(scale = 1) {
  theme_minimal(18 * scale) +
    theme(
      text = element_text(size = 18 * scale),
      axis.title = element_text(size = 16 * scale),
      strip.text = element_text(size = 16 * scale),
      legend.position = "bottom",
      # Add to minimise space between panels
      panel.spacing = unit(0.01, "lines"),
      #plot.margin   = margin(5, 5, 5, 5),
      panel.border  = element_rect(color = "gray80", fill = NA, size = 0.01)
    )
}

#' @title Custom Minimal Theme for ggplot2
#'
#' @description Creates a custom theme based on theme_minimal with specific text sizes and no legend by default
#'
#' @param base_size Base font size, default is 12
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top"), default is "none"
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_my()
#'   
#' # With legend
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   theme_my(legend_position = "right")
#' }
#'
#' @import ggplot2
#' @export
theme_my <- function(base_size = 12, legend_position = "none") {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title     = element_text(size = base_size + 2, face = "bold"),
      axis.title     = element_text(size = base_size - 2),
      legend.position = legend_position,
      legend.title   = element_text(size = base_size - 2),
      legend.text    = element_text(size = base_size - 4)
    )}

