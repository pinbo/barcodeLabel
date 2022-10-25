#' Make Label sheets with barcodes
#'
#' @param Labels a vector containing label names used for generating barcodes
#' @param name pdf output file name
#' @param barcode_type "linear" for code128, or "matrix" for QR code
#' @param label_type Presets for label type "avery5967" (1.75" x 0.5") or "avery5960" (2.63" x 1"). Default is NULL so you can change the page size and lable size parameters.
#' @param ErrCorr error correction value for matrix labels only (1 = Low (7\%), 2 = Medium (15\%), 3 = Quantile (25\%), 4 = High (30\%)
#' @param Fsz Font size,  will be adjusted to fit text space
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
#' @param x_space A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".
#' @param y_space A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".
#' @param fontfamily font family ("mono", "sans", "serif") or specific font based on your operation system.
#' @param showborder logical: whether to show border of labels. Set to TRUE to check whether everything fit in the label area.
#' @param barcode_width 0-1, barcode proportion of the label width
#' @param barcode_height 0-1, barcode proportion of the label height
#' @param text_top_height 0-1. Linear barcode allows 2 text boxes (above and below the barcode). This sets the height proportion of the label width for the text box above the barcode
#' @param text_bottom_height 0-1. Sets the height proportion of the label width for the text box below the barcode
#' @param text_top text above the 1D barcode (can use "\\n" for line break)
#' @param text_bottom text bottom (can use "\\n" for line break)
#' @param text_qr text for on the right side of QR (can use "\\n" for line break)
#' @param label_margin 0-1. Set a margin to avoid page shift during printing
#' @param text_align left or center alignment for text. Default is center for 1D barcode and left for 2D barcodes.
#'
#' @return Does not return anything, just create a PDF files with labels.
#' @export
#'
#' @examples
#' dd <- data.frame(plot = 1:10, accession = LETTERS[1:10])
#' # use plot to generate barcodes, and give different text
#' # for 1D (linear code128) labels
#' makeLabel(Labels=dd$plot, name="field2022_linear.pdf", text_top = paste0("Plot ",dd$plot, "\n", "Acc ", dd$accession), 
#' text_bottom = rep("Davis Field 2022", nrow(dd)), barcode_type="linear", Fsz=9, 
#' label_type = "avery5967", showborder = T, text_top_height = 0.4, 
#' text_bottom_height = 0.2, barcode_height = 0.4, barcode_width = 0.9, fontfamily = "sans")
#' # for QR codes
#' makeLabel(Labels=dd$plot, name="field2022_QR", text_qr = paste0("Plot ",dd$plot, "\n", "Acc ", dd$accession), 
#' barcode_type="matrix", Fsz=12, label_type = "avery5967", showborder = T, 
#' barcode_height = 1, fontfamily = "sans")
makeLabel <- function(
    Labels = NULL, # vector containing label names used for generating barcodes
    name = "LabelsOut", # pdf output file name
    barcode_type = "linear", # "linear" for code128, or "matrix" for QR code
    label_type = NULL, # could choose "avery5967" (1.75" x 0.5") or "avery5960" (2.63" x 1")
    ErrCorr = 1, #  error correction value for matrix labels only: (L = Low (7%), M = Medium (15%), Q = Quantile (25%), H = High (30%)
    Fsz = 12, # font size, will be adjusted to fit text space
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
    x_space = 0.1, # A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".
    y_space = 0.5, # A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".
    fontfamily = "mono", # "mono", "sans", "serif"
    showborder = FALSE, # whether to show border of labels
    barcode_width = 0.9, # 0-1, proportion of the label width, only for 1D barcodes
    barcode_height = 0.5, # 0-1, proportion of the label height, only for 1D barcod
    text_top_height = 0, # 0-1, proportion of the label height, only for 1D barcod
    text_bottom_height = 0.5, # 0-1, proportion of the label height, only for 1D barcod
    # text_width = 0.9, # leave space for paper miss alignment; 0-1, proportion of the label height
    text_top = NULL, # text above the 1D barcode
    text_bottom = NULL, # text botoom
    text_qr = NULL, # text for QR
    label_margin = 0.05,
    text_align = ifelse(barcode_type=="linear", "center", "left")  # left or center
    ){
  cat("text_align is: ", text_align, "\n")
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
    } else {
      cat("Unknown label type!! It now only has preset label sizes for avery5960 and avery5967.\nPlease set the label size manually.\n")
    }
  }
  if (length(Labels) == 0) stop("Labels do not exist. Please pass in Labels")
  # what to do depending on class of Label input
  if(class(Labels)[1] %in% c("character", "integer", "numeric", "factor")){
    # treat as vector
    Labels <- Labels
  } else if (class(Labels)[1] == "data.frame") {
    if (any(tolower(names(Labels)) == "label")){
      Labels <- Labels[, "label"]
    } else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  } else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (all(vapply(c(
    numcol, numrow, 
    Fsz, ERows, ECols, 
    page_width, page_height, 
    height_margin, width_margin, 
    x_space, y_space), is.numeric, logical(1L))) != TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  
  if (length(text_top) > 0) {
    if(length(text_top) != length(Labels)) {
      stop("Length of alt-text and Labels not equal.")
    }
    # text_top <- as.factor(text_top)
  }
  
  if (length(text_bottom) > 0) {
    if(length(text_bottom) != length(Labels)) {
      stop("Length of alt-text and Labels not equal.")
    }
    # text_bottom <- as.factor(text_bottom)
  }
  
  # clean up any open graphical devices if function fails
  on.exit(grDevices::graphics.off())
  
  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  
  if(!is.numeric(label_width)){label_width <- width_margin/numcol}
  if(!is.numeric(label_height)){label_height <- height_margin/numrow}
  
  if((barcode_type == "linear") & label_width / labelLength < 0.03)
    warning("Linear barcodes created will have bar width smaller than 0.03 inches. \n  Increase label width to make them readable by all scanners.")
  
  column_space <- (width_margin - label_width * numcol)/(numcol - 1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 1)
  
  # Viewport Setup
  ## grid for page, the layout is set up so last row and column do not include the spacers for the other columns
  barcode_layout <- grid::grid.layout(numrow, 
        numcol,
        widths = grid::unit(c(rep(label_width + column_space, numcol-1), label_width), "in"),
        heights = grid::unit(c(rep(label_height + row_space, numrow-1), label_height), "in")
        )
  
  ## line numbers by counting "\n"
  nline_text_top = 1
  nline_text_bottom = 1
  nline_text_qr = 1
  if (!is.null(text_top)){
    nline_text_top = nchar(gsub("[^\n]", "", text_top[1])) + 1 # number of lines
    cat("No. of top text lines is", nline_text_top, "\n")
  }
  if (!is.null(text_bottom)){
    nline_text_bottom = nchar(gsub("[^\n]", "", text_bottom[1])) + 1 # number of lines
    cat("No. of bottom text lines is", nline_text_bottom, "\n")
  }
  if (!is.null(text_qr)){
    nline_text_qr = nchar(gsub("[^\n]", "", text_qr[1])) + 1 # number of lines
    cat("No. of QR text lines is", nline_text_qr, "\n")
  }
  Fsz_top = ifelse(Fsz * nline_text_top > text_top_height *label_height * 72, text_top_height *label_height * 72 / nline_text_top, Fsz)
  Fsz_bottom = ifelse(Fsz * nline_text_bottom > text_bottom_height *label_height * 72, text_bottom_height *label_height * 72 / nline_text_bottom, Fsz)
  Fsz_qr = ifelse(Fsz * nline_text_qr > (1 - 2 * label_margin) *label_height * 72, (1 - 2 * label_margin) *label_height * 72 / nline_text_qr, Fsz)
  cat("Font size used at top is", Fsz_top, "\nFont size used at bottom is", Fsz_bottom, "\nFont size used for QR is", Fsz_qr, "\n")
  
  ## change viewport and barcode generator depending on qr or 1d barcodes
  if(barcode_type == "linear"){
    label_vp_top <- grid::viewport(
      x = grid::unit(label_margin * label_width, "in"), 
      y = grid::unit(1 - label_margin, "npc"),
      width  = grid::unit( (1-2*label_margin) * label_width, "in"),
      height = grid::unit( (1-2*label_margin) * text_top_height * label_height, "in"), 
      just = c("left", "top"))
    
    code_vp <- grid::viewport(
      x = grid::unit(0.5 * label_width, "in"),
      # x=grid::unit(label_width/(label_width + column_space)*(0.5+label_margin), "npc"), 
      y = grid::unit(label_margin + (1-2*label_margin)*(text_bottom_height + barcode_height), "npc"), 
      width  = grid::unit( (1-2*label_margin) * barcode_width * label_width, "in"), 
      height = grid::unit( (1-2*label_margin) * barcode_height * label_height, "in"),
      # just=c("left", "top")
      just=c("center", "top")
      )
    
    label_vp_bottom <- grid::viewport(
      x = grid::unit(label_margin * label_width, "in"), 
      y = grid::unit(label_margin + (1-2*label_margin)*text_bottom_height, "npc"),
      width  = grid::unit( (1-2*label_margin) * label_width, "in"),
      height = grid::unit( (1-2*label_margin) * text_bottom_height * label_height, "in"), 
      just = c("left", "top"))

    label_plots <- sapply(as.character(Labels), code_128_make, USE.NAMES = TRUE, simplify = FALSE)
  } else if (barcode_type =="matrix"){
    # vp for the qrcode within the grid layout
    code_vp <- grid::viewport(
      x = grid::unit(label_margin * label_height, "in"),
      y = grid::unit(0.5, "npc"), 
      width  = grid::unit(barcode_height * label_height, "in"), 
      height = grid::unit(barcode_height * label_height, "in"), 
      just=c("left", "center"))
    # vp for the text label within the grid layout
    label_vp_qr <- grid::viewport(
      x=grid::unit( (1 + x_space) * barcode_height * label_height, "in"),
      y=grid::unit(y_space, "npc"), 
      width = grid::unit(label_width - (1 + x_space) * barcode_height * label_height, "in"), 
      height = grid::unit( (1 - label_margin) * label_height, "in"), 
      just=c("left", "center"))
    
    #################
    label_vp_top <- grid::viewport(
      x = grid::unit( (1 + x_space) * barcode_height * label_height, "in"),
      y = grid::unit(1 - label_margin, "npc"),
      width  = grid::unit( (1-2*label_margin) * label_width, "in"),
      height = grid::unit( (1-2*label_margin) * text_top_height * label_height, "in"), 
      just = c("left", "top"))
   
    label_vp_bottom <- grid::viewport(
      x = grid::unit( (1 + x_space) * barcode_height * label_height, "in"),
      y = grid::unit(label_margin + (1-2*label_margin)*text_bottom_height, "npc"),
      width  = grid::unit( (1-2*label_margin) * label_width, "in"),
      height = grid::unit( (1-2*label_margin) * text_bottom_height * label_height, "in"), 
      just = c("left", "top"))
    # generate qr, most time intensive part
    label_plots <- sapply(as.character(Labels), qrcode_make, ErrCorr = ErrCorr, USE.NAMES = TRUE, simplify = FALSE)
  } else {stop("Barcode type must be linear or matrix")}

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
  duplication <- ceiling(length(Labels) / nrow(positions)) + 5
  
  label_positions <- do.call("rbind", replicate(duplication, positions, simplify = FALSE))
  
  # condition here for col/row skipping
  starting_pos_index <- min(which(label_positions$x == ECols + 1  & label_positions$y == ERows + 1))
  if(ECols > numcol | ERows > numrow){
      warning("Number of rows/columns to skip greater than number of rows/columns on page. Labels will start in top left corner.") 
      starting_pos_index <- 1
  }
  label_positions <- label_positions[seq(starting_pos_index, starting_pos_index + length(Labels)),]
  
  # File Creation
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, 
                 width = page_width, 
                 height = page_height, 
                 onefile = TRUE, 
                 family = fontfamily) # Standard North American 8.5 x 11

  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  
  for (i in seq(1,length(label_plots))){
    # Split label to count characters 
    # Xsplt <- names(label_plots[i])
    # Xalt <- paste(alt_text[i])
    lab_pos <- label_positions[i,]
    
    if(all(i != 1 & lab_pos == c(1, 1))){
      grid::grid.newpage()
      grid::pushViewport(
        grid::viewport(width = grid::unit(page_width, "in"), 
                       height = grid::unit(page_height, "in"))
      )
      # barcode_layout=grid.layout(numrow, numcol, widths = widths, heights = heights)
      grid::pushViewport(bc_vp)
    }

    grid::pushViewport(grid::viewport(layout.pos.row=lab_pos$y, layout.pos.col=lab_pos$x))
    if (showborder) grid::grid.rect() # Junli: need to comment out
    # draw barcodes
    grid::pushViewport(code_vp)
    grid::grid.draw(label_plots[[i]])
    grid::popViewport()
    # fill text top
    if (length(text_top) > 0) {
      grid::pushViewport(label_vp_top)
      if(barcode_type =="linear"){
        grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
      }
      # grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, lineheight = 0.8))
      if (text_align == "left"){
        grid::grid.text(label = text_top[i], x = grid::unit(0, "npc"), just = "left", gp = grid::gpar(fontsize = Fsz_top, lineheight = 0.8))
      } else {
        grid::grid.text(label = text_top[i], gp = grid::gpar(fontsize = Fsz_top, lineheight = 0.8))
      }
      grid::popViewport()
    }
    # fill text below
    if (length(text_bottom) > 0) {
      grid::pushViewport(label_vp_bottom)
      if(barcode_type =="linear"){
        grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
      }
      if (text_align == "left"){
        grid::grid.text(label = text_bottom[i], x = grid::unit(0, "npc"), just = "left", gp = grid::gpar(fontsize = Fsz_bottom, lineheight = 0.8))
      } else {
        grid::grid.text(label = text_bottom[i], gp = grid::gpar(fontsize = Fsz_bottom, lineheight = 0.8))
      }
      grid::popViewport()
    }
    # fill QR code text
    if (length(text_qr) > 0) {
      grid::pushViewport(label_vp_qr)
      if (text_align == "left"){
        grid::grid.text(label = text_qr[i], x = grid::unit(0, "npc"), just = "left", gp = grid::gpar(fontsize = Fsz_qr, lineheight = 0.8))
      } else { # center
        grid::grid.text(label = text_qr[i], gp = grid::gpar(fontsize = Fsz_qr, lineheight = 0.8))
      }
      grid::popViewport()
    }
    grid::popViewport()
  }

} #end custom_create_PDF()

#' qrcode_make: create a grid Grob object for drawing QR code generated by function "qr"
#' @rdname makeLabel
#' @export
qrcode_make<-function(Labels, ErrCorr){
  # Create text label
  # Xtxt<-gsub("_", "-", Labels)
  Xtxt <- Labels
  if(nchar(Xtxt) < 1){
    Xtxt <- paste0("  ", Xtxt)
    warning("Label is blank. Padding with empty spaces.")
  }
  # Create qrcode
  Xpng <- grid::rasterGrob(
    qrcode(paste0(Xtxt), ecl = ErrCorr), 
    interpolate = FALSE)
  return(Xpng)
}

#' code_128_make: create a grid Grob object for drawing linear code128
#' @rdname makeLabel
#' @export
code_128_make <- function(Labels){
  ## labels is a character string
  ## read in dict 
  Barcodes <- barcodes128
  ## double check Labels
  Labels <- as.character(Labels)
  Labels <- iconv(Labels, from = "utf-8", to = "ascii", sub = "-")
  start_code <- 209
  lab_chars <- unlist(strsplit(Labels, split = ""))
  lab_values <- sapply(lab_chars, function(x) utf8ToInt(x))
  # ascii to code 128 is just a difference of 32, this line keeps clarity
  code_values <- lab_values - 32
  # 104 is the start value for start code b, hardcoded right now
  check_sum <- 104 + sum(code_values * seq(1,length(code_values)))
  check_character <- check_sum %% 103
  Binary_code <- sapply(lab_values, 
                        function(x, Barcodes) Barcodes$Barcode[x == Barcodes$ASCII], 
                        Barcodes = Barcodes)
  ## create quiet zone
  quiet_zone <- paste(c(1:(10)*0),collapse="")
  ## paste together in order: quiet zone, start code binary, binary label, checksum character
  ## stop code, and quiet zone. Barcode for checksum is extracted based on position in Barcodes.
  binary_label <- paste(quiet_zone, 
                        Barcodes$Barcode[Barcodes$ASCII == start_code],
                        paste(Binary_code, collapse=""),
                        Barcodes$Barcode[check_character + 1],
                        "1100011101011",
                        quiet_zone,
                        collapse = "", sep ="")
  ## split binary apart for 
  bar_values <- as.numeric(unlist(strsplit(binary_label, split = "")))
  barcode_bars <- grid::rasterGrob(t(!as.matrix(bar_values)), width = 1, height = 1, interpolate = FALSE)
  return(barcode_bars)
}

