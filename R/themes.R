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
#'   theme_beamer(scale =1.2)
#' }
#'
#' @import ggplot2
#' @export
theme_beamer<-function(scale=1){
  theme_minimal(18*scale) +
    theme(
      text =element_text(size =18 * scale),
      axis.title =element_text(size =16 * scale),
      strip.text =element_text(size =16 * scale),
      legend.position ="bottom",
      # Add to minimise space between panels
      panel.spacing =unit(0.01, "lines"),
      #plot.margin   =margin(5, 5, 5, 5),
      panel.border  =element_rect(color ="gray80", fill =NA, size =0.01))}

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
#' ggplot(mtcars, aes(wt, mpg, color =factor(cyl))) +
#'   geom_point() +
#'   theme_my(legend_position ="right")
#' }
#'
#' @import ggplot2
#' @export

# Create custom themes
theme_base=theme_minimal()+
  theme(
    text =element_text(),
    plot.title =element_text(size =12, face ="bold", hjust =0.5),
    axis.title =element_text(size =10),
    axis.text =element_text(size =8),
    strip.text =element_text(size =10, face ="bold"),
    panel.grid.major =element_line(color ="grey90", size =0.2),
    panel.grid.minor =element_blank(),
    panel.spacing =unit(0.5, "lines")
  )

theme_my<-function(base_size =12, legend_position ="none") {
  theme_minimal(base_size =base_size) +
    theme(
      plot.title     =element_text(size =base_size + 2, face ="bold"),
      axis.title     =element_text(size =base_size - 2),
      legend.position=legend_position,
      legend.title   =element_text(size =base_size - 2),
      legend.text    =element_text(size =base_size - 4)
    )}

theme_facet<-function(base_size =12) {
  theme_minimal(base_size =base_size) %+replace%
    theme(
      # Remove legends
      legend.position="none",
      legend.title  =element_blank(),
    
      # Remove facet labels
      strip.text =element_blank(),
      
      # Remove axis elements
      axis.text =element_blank(),
      axis.ticks =element_blank(),
      
      # Grid customization
      panel.grid.minor =element_blank(),
      panel.grid.major =element_line(color ="grey90"),
      
      # Title and subtitle formatting
      plot.title    =element_text(face="bold", size =14,       hjust=0),
      plot.subtitle =element_text(size=12,     color="grey30", hjust=0),
      
      # Panel spacing
      panel.spacing =unit(0, "lines"))}

theme_no_y=theme_base +
  theme(
    axis.title.y =element_blank(),
    axis.text.y =element_blank(),
    axis.ticks.y =element_blank(),
    panel.grid.major.y =element_blank(),
    plot.margin =unit(c(5.5, 2, 5.5, 2), "points"))

theme_no_title=theme(
  strip.text.y =element_blank(),
  strip.background =element_blank())

# AUC theme
theme_auc=theme_base +
  theme(
    axis.title.y =element_blank(),
    axis.text.y  =element_blank(),
    panel.grid   =element_blank())

