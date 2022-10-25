#' preview_label: Print the first label to preivew the results.
#'
#' @param label_width label width in inch (default label size is Avery 5967)
#' @param label_height label height in inch 
#' @param vp_list a list of grid viewports for how to layout elements on the label
#' @param content_list a list of contents to put for each element in the vp_list
#'
#' @return preview_label: NULL
#' @export
#'
#' @rdname make_custom_label
preview_label <- function(label_width=1.75, label_height=0.5, vp_list, content_list){
  # pdf(file="preview001.pdf", width=label_width, height=label_height) # in inch
  dev.new(width=label_width, height=label_height, unit="in")
  grid::grid.rect()
  if (length(vp_list) > 0){
    for (j in 1:length(vp_list)){
      vp = vp_list[[j]]
      content = content_list[[j]]
      grid::pushViewport(vp)
      if (class(content) == "list") {# grob list
        grid::grid.draw(content[[1]])
      } else {# text
          grid::grid.text(label = content[1])
      }
      grid::popViewport()
    }
  }

}
