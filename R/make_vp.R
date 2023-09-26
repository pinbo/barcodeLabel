#' make_vp: a slightly simple way to make grid viewport (layout)
#'
#' @param x coordinate on the x axis
#' @param y coordinate on the y axis
#' @param width rectangle area width in inch
#' @param height rectangle area height in inch
#' @param just coordinates x and y are for (relative to)
#' @param font_size font size; automatically adjsuted smaller based on line number and text area height
#' @param line_number how many lines of text?
#' @param unit unit to be used, such as inch, mm, cm etc
#'
#' @return make_vp: an R object of class viewport.
#' @export
#'
#' @rdname make_custom_label
make_vp <- function(x, y, width, height, just = c("left", "top"), font_size=12, line_number=1, unit="inch"){
  Fsz = ifelse(font_size * line_number > height * 72, height * 72 / line_number, font_size)
  cat("Font size used is", Fsz, "\n")
  vp = grid::viewport(
    x = grid::unit(x, unit), 
    y = grid::unit(y, unit),
    width  = grid::unit(width, unit),
    height = grid::unit(height, unit),
    just = just,
    gp = grid::gpar(fontsize = Fsz, lineheight = 0.8)
  )
  return(vp)
}