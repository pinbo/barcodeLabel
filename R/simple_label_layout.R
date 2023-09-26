#' Make simple label layout for barcode printing
#' 
#' The layout on each label is simple: 1 text area + 1 barcode area (up and down) for linear barcodes, 
#' and left QR code + right text for matrix barcodes.
#' @param barcode_text a vector of strings for generating barcodes
#' @param print_text a vector of strings for printing on the label (could use "\\n" for line break)
#' @param barcode_type "null" for no barcode needed, "linear" for code128, "qr" for QR code, and "dm" for datamatrix (ecc200)
#' @param label_width label width in inch
#' @param label_height label height in inch
#' @param label_margin 0-1, margin proportion of the short side (label height)
#' @param font_size font size to use for text, will be adjusted automatically to fit text area
#' @param fontfamily font family to use: mono, sans, or serif. You can also use specific font available in your OS (such as Times).
#' It is used to determine string width.
#' @param line_number how many lines are in the print_text; auto determined based on "\\n".
#' @param barcode_on_top whether to put the linear barcode on the top, text at the bottom.
#' @param barcode_height barcode height proportion of the label height.
#' @param ecl error correction value for matrix labels only (1 = Low (7\%), 2 = Medium (15\%), 3 = Quantile (25\%), 4 = High (30\%)
#' @param useMarkdown whether treat ** quotes as markdown (only support fontfaces)
#' @param barcode_scale 0-1, scale barcode plot inside the barcode drawing area
#' @param font_col font color, default "black"
#' @param fontface The specification of fontface: 1 = plain, 2 = bold, 3 = italic, 4 = bold italic.
#' @param unit unit to be used, such as inch, mm, cm etc
#'
#' @return a list of label layout (vp_list) and content (content_list) for input of function "custom_label" parameters 'vp_list' and 'content_list'
#' @export
#'
#' @examples
#' 
#' dd <- data.frame(plot = 101:110, accession = LETTERS[1:10])
#' 
#' # for 1D (linear code128) labels
#' # 1. create simple element layout on each label
#' linear_label_list <- simple_label_layout(
#'   barcode_text=dd$plot,
#'   print_text = paste0("**Plot** ",dd$plot, "\n", "**Acc** ", dd$accession),
#'   barcode_on_top = T, barcode_type="linear", font_size =12,
#'   barcode_height = 0.4, fontfamily = "sans", useMarkdown = T)
#' # 2. create pdf file
#' make_custom_label(
#'     label_number = nrow(dd), # how many labels to print
#'     name = "linear_barcode_test", # pdf output file name
#'     label_type = "avery5967",
#'     fontfamily = "mono", # "mono", "sans", "serif"
#'     showborder = T, # whether to show border of labels
#'     vp_list = linear_label_list$vp_list,
#'     content_list = linear_label_list$content_list,
#'     text_align = "center", # left or center
#'     useMarkdown = T
#' )
#' # for QR codes
#' # 1. create simple element layout on each label
#' qr_label_list <- simple_label_layout(
#'   barcode_text=dd$plot,
#'   print_text = paste0("**Plot** ",dd$plot, "\n", "**Acc** ", dd$accession),
#'   barcode_on_top = T, barcode_type="qr", font_size=12,
#'   barcode_height = 1, fontfamily = "sans", useMarkdown=T)
#' # 2. create pdf file
#' make_custom_label(
#'   label_number = nrow(dd), # how many labels to print
#'   name = "QR_barcode_test", # pdf output file name
#'   label_type = "avery5967",
#'   fontfamily = "mono", # "mono", "sans", "serif"
#'   showborder = T, # whether to show border of labels
#'   vp_list = qr_label_list$vp_list,
#'   content_list = qr_label_list$content_list,
#'   text_align = "left", # left or center
#'   useMarkdown = T
#' )
#' 
simple_label_layout = function(
    barcode_text = NULL, # text to generate barcode
    print_text = barcode_text, # text to be printed on the label; default is the same as barcode_text
    barcode_type="linear",
    label_width=1.75, label_height=0.5, # label size in inch
    label_margin = 0.05, # margin proportion of label size
    font_size=12, # font size, could be adjusted smaller to fit the space
    fontfamily = "mono", # font family to use, for calculating string width
    line_number = NULL, # how many lines of print_text; determined by number of "\n" in the text if NULL.
    barcode_on_top = FALSE, # for linear barcode128 only, whether it should be put on top of the text
    barcode_height = ifelse(barcode_type=="linear", 0.5, 1), # barcode height proportion
    ecl = 1, # error correction level for QR code.  1-4 (1 = Low (7\%), 2 = Medium (15\%), 3 = Quantile (25\%), 4 = High (30\%).
    useMarkdown = FALSE, # whether treat ** quotes as markdown (only support fontfaces)
    barcode_scale = 1, # 0-1, scale barcode inside the barcode area
    font_col = "black", # text color
    fontface = 1,
    unit = "inch" # unit to be used, such as inch, mm, cm etc
){
  if (length(barcode_text) == 0 & is.null(print_text)) stop("barcode_text and print_text are NULL! Nothing to do.")
  if (is.null(barcode_text)){
    barcode_type = "null"
    barcode_height = 0
  }
  if (!is.null(print_text)){
    if (is.null(line_number)){
      line_number = nchar(gsub("[^\n]", "", print_text[1])) + 1 # number of lines
      cat("No. of lines is", line_number, "\n")
    }
  }
  text_height = ifelse(barcode_type=="linear", 1 - barcode_height, 1)
  text_height_inch = (1-2*label_margin) * label_height * text_height
  # Fsz = ifelse(font_size * line_number > text_height_inch * 72, text_height_inch * 72 / line_number, font_size)
  # cat("Font size used is", Fsz, "\n")
  # use the shorter one as label margin in inch
  short_side = min(label_height, label_width)
  label_margin_inch = label_margin * short_side
  ## get vp_list and content_list
  # x cannot be center because the label layout width is different for columns
  # normaltext = print_text
  # fontface = 4
  # if(useMarkdown){
  #   normaltext = gsub("(\\*+)((.|\n)+?)\\1", "\\2", print_text, perl = T)
  #   fontface = 4
  # }
  # pdf(NULL) # and this could be opened with additional parameters
  # par(ps = Fsz, family = fontfamily, font=fontface)
  # max_text_width = max(strwidth(normaltext, units = 'in'))
  # dev.off()
  # normaltext = if(useMarkdown) gsub("(\\*+)((.|\n)+?)\\1", "\\2", print_text, perl = T) else print_text
  # textsizes = sapply(normaltext, function(x) getxy2(x, unit="inch", gp=gpar(fontsize=Fsz, fontfamily=fontfamily, fontface=4))) # use fontface4 to get the max width
  # max_text_width = max(textsizes[1,])
  # cat("max_text_width is", max_text_width, "\n")
  pdf(file=NULL) # mystrwidth depends on the device.
  if(barcode_type == "null"){
    text_width = label_width - 2*label_margin_inch
    # Fsz = if (max_text_width > text_width) floor(text_width/max_text_width*Fsz) else Fsz
    tt = text_array_wrap(print_text, font_size, text_width, text_height_inch, fontfamily, useMarkdown)
    print_text = tt$text
    Fsz = tt$font_size
    cat("Final Font size used is", Fsz, "\n")
    # view port list
    vp_list = list(
      text_vp = grid::viewport(
        x = grid::unit(label_margin_inch , unit),
        y = grid::unit(label_margin_inch, unit),
        width  = grid::unit( label_width - 2*label_margin_inch, unit),
        height = grid::unit( (label_height - 2*label_margin_inch) * text_height, unit), 
        just = c("left", "bottom"),
        gp = grid::gpar(fontsize = Fsz, lineheight = 0.8, col = font_col, fontface = fontface)
      )
    )
    # content list
    content_list = list(
      text = print_text
    )
  } else if(barcode_type == "linear"){
    text_width = label_width - 2*label_margin_inch
    # Fsz = if (max_text_width > text_width) floor(text_width/max_text_width*Fsz) else Fsz
    tt = text_array_wrap(print_text, font_size, text_width, text_height_inch, fontfamily, useMarkdown)
    print_text = tt$text
    Fsz = tt$font_size
    cat("Final Font size used is", Fsz, "\n")
    # view port list
    vp_list = list(
      code_vp = grid::viewport(
        x = grid::unit(0.01 , unit),
        y = if(barcode_on_top) grid::unit(label_height - label_margin_inch, unit) else grid::unit(label_margin_inch, unit),
        # width  = grid::unit(label_width - 2*label_margin_inch, unit), 
        width  = grid::unit(label_width*0.98, unit), # look like code128 output already has white space on the end
        height = grid::unit( (label_height - 2*label_margin_inch) * barcode_height, unit),
        just = if (barcode_on_top) c("left", "top") else c("left", "bottom")
      ),
      text_vp = grid::viewport(
        x = grid::unit(label_margin_inch , unit),
        y = if(barcode_on_top)  grid::unit(label_margin_inch, unit) else  grid::unit(label_height - label_margin_inch, unit),
        width  = grid::unit( label_width - 2*label_margin_inch, unit),
        height = grid::unit( (label_height - 2*label_margin_inch) * text_height, unit), 
        just = if (barcode_on_top) c("left", "bottom") else c("left", "top"),
        gp = grid::gpar(fontsize = Fsz, lineheight = 0.8, col = font_col, fontface = fontface)
      )
    )
    # content list
    content_list = list(
      code = lapply(as.character(barcode_text), code_128_make),
      text = print_text
    )
  } else if (barcode_type =="qr" | barcode_type =="dm"){
    text_width = label_width - barcode_height * short_side - label_margin_inch
    cat("text_width is ", text_width, "\n")
    # Fsz = if (max_text_width > text_width) floor(text_width/max_text_width*Fsz) else Fsz
    tt = text_array_wrap(print_text, font_size, text_width, text_height_inch, fontfamily, useMarkdown)
    print_text = tt$text
    Fsz = tt$font_size
    cat("Final Font size used is", Fsz, "\n")
    vp_list = list(
      code_vp = grid::viewport(
        x = grid::unit(label_margin_inch, unit),
        y = grid::unit(0.5, "npc"), 
        width  = grid::unit(barcode_height * (1-2*label_margin)*short_side, unit), 
        height = grid::unit(barcode_height * (1-2*label_margin)*short_side, unit), 
        just=c("left", "center")),
      text_vp = grid::viewport(
        x=grid::unit( barcode_height * label_height, unit),
        y=grid::unit(0.5, "npc"), 
        width = grid::unit(label_width - barcode_height * label_height, unit), 
        height = grid::unit( (1 - 2*label_margin) * label_height, unit), 
        just=c("left", "center"),
        gp = grid::gpar(fontsize = Fsz, lineheight = 0.8, col = font_col, fontface = fontface)
      )
    )
    # content list
    if(barcode_type =="qr")
      code = lapply(as.character(barcode_text), qrcode_make, ErrCorr = ecl, scale=barcode_scale)
    else code = lapply(as.character(barcode_text), dmcode_make, scale=barcode_scale)
    content_list = list(
        code = code,
      text = print_text
    )
  } else {stop("Barcode type must be null, linear, qr or dm")}
  dev.off()
  
  return(list(vp_list=vp_list, content_list=content_list))
}

