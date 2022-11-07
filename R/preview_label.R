#' preview_label: Print the first label to preivew the results.
#'
#' @param label_width label width in inch (default label size is Avery 5967)
#' @param label_height label height in inch 
#' @param vp_list a list of grid viewports for how to layout elements on the label
#' @param content_list a list of contents to put for each element in the vp_list
#' @param text_align text alignment (left, center, or right). Default is "center"
#' @param useMarkdown whether treat ** quotes as markdown (only support fontfaces)
#'
#' @return preview_label: NULL
#' @export
#'
#' @rdname make_custom_label
preview_label <- function(label_width=1.75, label_height=0.5, vp_list, content_list, text_align="center", useMarkdown=FALSE){
  grid:: grid.newpage()
  vpbox = grid::viewport(x=0.5, y=0.5, width=grid::unit(label_width, "inches"), height=grid::unit(label_height, "inches"))
  grid::pushViewport(vpbox)
  grid::grid.rect()
  if (length(vp_list) > 0){
    for (j in 1:length(vp_list)){
      vp = vp_list[[j]]
      content = content_list[[j]]
      grid::pushViewport(vp)
      if (class(content) == "list") {# grob list
        grid::grid.draw(content[[1]])
      } else {# text
        if (text_align == "left"){
          # grid::grid.text(label = content[i], x = grid::unit(0, "npc"), just = "left")
          richtext(content[1], x=0, hjust=0, useMarkdown=useMarkdown)
        } else if (text_align == "right") {
          richtext(content[1], x=1, hjust=1, useMarkdown=useMarkdown)
        } else richtext(content[1], useMarkdown=useMarkdown)
      }
      grid::popViewport()
    }
  }
  grid::popViewport()
}
