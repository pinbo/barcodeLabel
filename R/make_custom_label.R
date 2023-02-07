#' Make pdf pages for barcode labels based on user defined label area layout.
#'
#' @param label_number # how many labels to print.
#' @param name pdf output name (no ".pdf)
#' @param label_type Presets for label type "avery5967" (1.75" x 0.5") or "avery5960" (2.63" x 1") or  "tough-spots-3/8inch" (USA Scientific TOUGH-SPOTS LABELS ON SHEETS cat# 9185-1000 to 9185-1008 3/8 inch diameter). Default is NULL so you can change the page size and label size parameters.
#' @param Across logical. When TRUE, print labels across rows, left to right. When FALSE, print labels down columns, top to bottom.
#' @param ERows number of rows to skip
#' @param ECols number of columns to skip
#' @param numrow number of label rows per page
#' @param numcol number of label columns per page
#' @param page_width page width in inch
#' @param page_height page height in inch
#' @param width_margin side margin of label sheet in inch
#' @param height_margin top margin of lablel sheet in inch
#' @param label_width label width in inch
#' @param label_height label height in inch
#' @param fontfamily font family ("mono", "sans", "serif") or specific font based on your operation system.
#' @param showborder logical: whether to show border of labels. Set to TRUE to check whether everything fit in the label area.
#' @param border_type rectangle, circle, or both
#' @param vp_list a list of grid viewport list for user designed label area layout (positions of rectangles on the label)
#' @param content_list a list of contents to fill in the label area layout
#' @param text_align left, right, or center alignment for text.
#' @param useMarkdown whether treat ** quotes as markdown (only support fontfaces)
#' 
#' @return make_custom_label: NULL, just create a pdf
#' @export
#'
#' @examples
#' 
#' dd <- data.frame(plot = 101:110, accession = LETTERS[1:10])
#' label_width = 1.75 # x axis
#' label_height = 0.5 # y axis
#' useMarkdown = TRUE
#' vp_list = list(
#'   # rectangle area 1: text top left: take up 0.3 of label_height, 0.5 of label_length
#'   text_vp1 = make_vp(
#'     x = 0,
#'     y = label_height,
#'     width  = label_width * 0.5,
#'     height = 0.3 * label_height,
#'     font_size = 6
#'   ),
#'   # rectangle area 2: text top right: take up 0.3 of label_height, 0.5 of label_length
#'   text_vp2 = make_vp(
#'     x = label_width * 0.5,
#'     y = label_height,
#'     width  = label_width * 0.5,
#'     height = 0.3 * label_height,
#'     font_size = 7
#'   ),
#'   # rectangle area 3: barcode in the middle: take up 0.4 of label_height
#'   code_vp1 = make_vp(
#'     x = 0,
#'     y = label_height * 0.7,
#'     width  = label_width,
#'     height = 0.4 * label_height
#'   ),
#'   # rectangle area 4: text at bottom
#'   text_vp3 = make_vp(
#'     x = 0,
#'     y = 0.3 * label_height,
#'     width  = label_width,
#'     height = 0.3 * label_height,
#'     font_size=12, line_number=2
#'   )
#' )
#' # content list: should have contents for each element in the vp_list
#' content_list = list(
#'   text1 = rep("*Davis Field 2022*", nrow(dd)),
#'   text2 = paste0("**Plot** ",dd$plot),
#'   code = lapply(as.character(dd$plot), code_128_make),
#'   text3 = paste0("Plot ",dd$plot, "\n", "Acc ", dd$accession)
#' )
#' # preview
#' preview_label(label_width=1.75, label_height=0.5, vp_list, content_list, useMarkdown=useMarkdown)
#' # create pdf
#' make_custom_label(
#'   label_number = nrow(dd), # how many labels to print
#'   name = "linear_custom_layout_test", # pdf output file name
#'   label_type = "avery5967",
#'   fontfamily = "mono", # "mono", "sans", "serif"
#'   showborder = T, # whether to show border of labels
#'   vp_list = vp_list,
#'   content_list = content_list,
#'   text_align = "center", # left or center
#'   useMarkdown = useMarkdown
#' )

make_custom_label <- function(
    label_number, # how many labels to print
    name = "LabelsOut", # pdf output file name
    label_type = NULL,
    Across = TRUE, # logical. When TRUE, print labels across rows, left to right. When FALSE, print labels down columns, top to bottom.
    ERows = 0, # number of rows to skip.
    ECols = 0, # number of columns to skip.
    numrow = 20, # Number of label rows per page
    numcol = 4, # Number of columns per page
    page_width = 8.5, # page width in inch
    page_height = 11, # page height in inch
    width_margin = 0.3, # side margin in inch
    height_margin = 0.5, # top margin in inch
    label_width = 1.75, # label width in inch
    label_height = 0.5, # label height in inch
    fontfamily = "mono", # "mono", "sans", "serif"
    showborder = FALSE, # whether to show border of labels
    border_type = "rectangle", # rectangle, circle, or both
    vp_list = NULL,
    content_list = NULL,
    text_align = "center", # left or center
    useMarkdown = FALSE # whether treat ** quotes as markdown (only support fontfaces)
){
  if (!is.null(label_type)){
    if (label_type == "avery5967"){
      numrow=20
      numcol=4
      width_margin=0.3
      height_margin=0.5
      label_width=1.75
      label_height=0.5
    } else if (label_type == "avery5960"){
      numrow=10
      numcol=3
      width_margin=0.19
      height_margin=0.5
      label_width=2.63
      label_height=1
    } else if (label_type == "tough-spots-3/8inch"){
      # USA Scientific TOUGH-SPOTS® LABELS ON SHEETS cat# 9185-1000 to 9185-1008
      # 3/8 inch diameter
      numrow=16
      numcol=12
      width_margin=0.5625 # 9/16 inch
      height_margin=0.578125 # 9.25/16 inch
      label_width=0.375 # 3/8 diameter
      label_height=0.375
      page_width = 8.4375 # 8 7/16 inch
    } else {
      cat("Unknown label type!! It now only has preset label sizes for avery5960 and avery5967.\nPlease set the label size manually.\n")
    }
  }
  # clean up any open graphical devices if function fails
  on.exit(grDevices::graphics.off())
  
  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  
  if(!is.numeric(label_width)){label_width <- width_margin/numcol}
  if(!is.numeric(label_height)){label_height <- height_margin/numrow}
  
  column_space <- (width_margin - label_width * numcol)/(numcol - 1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 1)
  
  # Viewport Setup
  ## grid for page, the layout is set up so last row and column do not include the spacers for the other columns
  # barcode_layout <- grid::grid.layout(
  #   numrow, 
  #   numcol,
  #   widths = grid::unit(c(rep(label_width + column_space, numcol-1), label_width), "in"),
  #   heights = grid::unit(c(rep(label_height + row_space, numrow-1), label_height), "in")
  # )
  # make exact grid
  barcode_layout <- grid::grid.layout(
    numrow*2 - 1, 
    numcol*2 - 1,
    widths = grid::unit(c(rep(c(label_width, column_space), numcol-1), label_width), "in"),
    heights = grid::unit(c(rep(c(label_height, row_space), numrow-1), label_height), "in")
  )
  
  # generate label positions
  if(Across){
    # across = TRUE
    positions <- expand.grid(x = 1:numcol, y = 1:numrow)
  } else {
    # across = FALSE
    positions <- expand.grid(y = 1:numrow, x = 1:numcol)
  }
  # make df of position for each label
  # this extra 5 is so that even if starting position is last cell, there are enough positions generated, hopefully
  duplication <- ceiling(label_number / nrow(positions)) + 5
  label_positions <- do.call("rbind", replicate(duplication, positions, simplify = FALSE))
  # condition here for col/row skipping
  starting_pos_index <- min(which(label_positions$x == ECols + 1  & label_positions$y == ERows + 1))
  if(ECols > numcol | ERows > numrow){
    warning("Number of rows/columns to skip greater than number of rows/columns on page. Labels will start in top left corner.") 
    starting_pos_index <- 1
  }
  label_positions <- label_positions[seq(starting_pos_index, starting_pos_index + label_number),]
  # File Creation
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, 
                 width = page_width, 
                 height = page_height, 
                 onefile = TRUE, 
                 family = fontfamily) # Standard North American 8.5 x 11
  
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  ##################
  for (i in seq(1, label_number)){
    lab_pos <- label_positions[i,]
    if(all(i != 1 & lab_pos == c(1, 1))){
      grid::grid.newpage()
      grid::pushViewport(
        grid::viewport(width = grid::unit(page_width, "in"), 
                       height = grid::unit(page_height, "in"))
      )
      grid::pushViewport(bc_vp)
    }
    # grid::pushViewport(grid::viewport(layout.pos.row=lab_pos$y, layout.pos.col=lab_pos$x))
    grid::pushViewport(grid::viewport(layout.pos.row=lab_pos$y*2-1, layout.pos.col=lab_pos$x*2-1))
    if (showborder) {
      if (border_type == "rectangle") grid::grid.rect()
      else if (border_type == "circle") grid::grid.circle(r=grid::unit(min(label_width, label_height)/2, "inches"))
      else { # both
        grid::grid.rect()
        grid::grid.circle(r=grid::unit(min(label_width, label_height)/2, "inches"))
      }
    }
    # draw barcodes
    if (length(vp_list) > 0){
      for (j in 1:length(vp_list)){
        vp = vp_list[[j]]
        content = content_list[[j]]
        grid::pushViewport(vp)
        if (class(content) == "list") {# grob list
          grid::grid.draw(content[[i]])
        } else {# text
          if (text_align == "left"){
            richtext(content[i], x=0, hjust=0, useMarkdown=useMarkdown)
          } else if (text_align == "right") {
            richtext(content[i], x=1, hjust=1, useMarkdown=useMarkdown)
          } else richtext(content[i], useMarkdown=useMarkdown)
        }
        grid::popViewport()
      }
    }
    grid::popViewport()
  }
}
