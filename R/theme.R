#' UKFSR chart theme
#'
#' @param base_size Base font size in pts
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size Base size for rect elements
#'
#' @return
#' @export
#'
#' @examples
theme_ukfsr <- function(base_size = 10, base_family = "",
                           base_line_size = base_size / 20,
                           base_rect_size = base_size / 20) {
  
  # https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
  
  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size/2`
  # instead.
  half_line <- base_size / 2
  
  # Throughout the theme, we use three font sizes, `base_size` (`rel(1)`)
  # for normal, `rel(0.8)` for small, and `rel(1.2)` for large.
  
  t <- ggplot2::theme_void() + 
    ggplot2::theme(
    # ggplot2::elements in this first block aren't used directly, but are inherited
    # by others
    line = ggplot2::element_line(
      colour = "black",
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "white", 
      colour = "black",
      linewidth = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "black", 
      size = base_size,
      lineheight = 0.9, 
      hjust = 0.5,
      vjust = 0.5, 
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line.y =        ggplot2::element_line(colour = "black"),
    axis.line.x =        ggplot2::element_blank(),
    axis.text =          ggplot2::element_text(size = ggplot2::rel(1.8)),
    axis.text.x =        ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    # axis.text.x.top =    ggplot2::element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    # axis.text.y.right =  ggplot2::element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         ggplot2::element_blank(),
    # axis.ticks.length =  unit(half_line / 2, "pt"),
    # axis.ticks.length.x = NULL,
    # axis.ticks.length.x.top = NULL,
    # axis.ticks.length.x.bottom = NULL,
    # axis.ticks.length.y = NULL,
    # axis.ticks.length.y.left = NULL,
    # axis.ticks.length.y.right = NULL,
    # axis.minor.ticks.length = rel(0.75),
    axis.title.x =       ggplot2::element_text(
      size = ggplot2::rel(1.8),
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
    ),
    # axis.title.x.top =   ggplot2::element_text(
    #   size = ggplot2::rel(1.8),
    #   margin = ggplot2::margin(b = half_line / 2),
    #   vjust = 0
    # ),
    axis.title.y =       ggplot2::element_text(
      size = ggplot2::rel(1.8),
      angle = 90,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = 1
    ),
    # axis.title.y.right = ggplot2::element_text(
    #   size = ggplot2::rel(1.8),
    #   angle = -90,
    #   margin = ggplot2::margin(l = half_line / 2),
    #   vjust = 0
    # ),
    
    legend.background =  ggplot2::element_rect(colour = NA),
    legend.spacing =     ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x =   NULL,
    legend.spacing.y =   NULL,
    legend.margin =      ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key =         ggplot2::element_blank(),
    legend.key.size =    ggplot2::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    # legend.key.spacing = ggplot2::unit(half_line, "pt"),
    legend.text =        ggplot2::element_text(size = ggplot2::rel(2)),
    legend.title =       ggplot2::element_blank(),
    legend.position =    "bottom",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    
    
    panel.background =   ggplot2::element_rect(fill = "white", colour = NA),
    panel.border =       ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "black",linewidth = ggplot2::rel(0.5)),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor =   ggplot2::element_blank(),
    panel.spacing =      ggplot2::unit(half_line, "pt"),
    # panel.spacing.x =    NULL,
    # panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,
    
    strip.background =   ggplot2::element_rect(fill = "grey95", colour = NA),
    strip.clip =         "inherit",
    strip.text =         ggplot2::element_text(
      colour = "black",
      size = ggplot2::rel(2.2),
      margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       ggplot2::element_text(angle = -90),
    strip.text.y.left =  ggplot2::element_text(angle = 90),
    strip.placement =    "inside",
    # strip.placement.x =  NULL,
    # strip.placement.y =  NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    
    plot.background =    ggplot2::element_rect(colour = "white"),
    plot.title =         ggplot2::element_text( # font size "large"
      size = ggplot2::rel(1.2),
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "panel",
    plot.subtitle =      ggplot2::element_text( # font size "regular"
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption =       ggplot2::element_text( # font size "small"
      size = ggplot2::rel(0.8),
      hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "panel",
    plot.tag =           ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5, vjust = 0.5
    ),
    plot.tag.position =  'topleft',
    plot.margin =        ggplot2::margin(half_line, half_line, half_line, half_line)
    
    
  )
  
  t
  
}