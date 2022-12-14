% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeLabel.R
\name{makeLabel}
\alias{makeLabel}
\alias{dmcode_make}
\alias{qrcode_make}
\alias{code_128_make}
\title{Make Label sheets with barcodes}
\usage{
makeLabel(
  Labels = NULL,
  name = "LabelsOut",
  barcode_type = "linear",
  label_type = NULL,
  ErrCorr = 1,
  Fsz = 12,
  Across = TRUE,
  ERows = 0,
  ECols = 0,
  numrow = 20,
  numcol = 4,
  page_width = 8.5,
  page_height = 11,
  width_margin = 0.3,
  height_margin = 0.5,
  label_width = 1.75,
  label_height = 0.5,
  x_space = 0.1,
  y_space = 0.5,
  fontfamily = "mono",
  showborder = FALSE,
  barcode_width = 0.9,
  barcode_height = 0.5,
  text_top_height = 0,
  text_bottom_height = 0.5,
  text_top = NULL,
  text_bottom = NULL,
  text_qr = NULL,
  label_margin = 0.05,
  text_align = ifelse(barcode_type == "linear", "center", "left")
)

dmcode_make(Labels, square = 1, scale = 1)

qrcode_make(Labels, ErrCorr = 2, scale = 1)

code_128_make(Labels)
}
\arguments{
\item{Labels}{a vector containing label names used for generating barcodes}

\item{name}{pdf output file name}

\item{barcode_type}{"linear" for code128, or "matrix" for QR code}

\item{label_type}{Presets for label type "avery5967" (1.75" x 0.5") or "avery5960" (2.63" x 1"). Default is NULL so you can change the page size and lable size parameters.}

\item{ErrCorr}{error correction value for matrix labels only (1 = Low (7\%), 2 = Medium (15\%), 3 = Quantile (25\%), 4 = High (30\%)}

\item{Fsz}{Font size,  will be adjusted to fit text space}

\item{Across}{logical. When TRUE, print labels across rows, left to right. When FALSE, print labels down columns, top to bottom.}

\item{ERows}{number of rows to skip}

\item{ECols}{number of columns to skip}

\item{numrow}{number of label rows per page}

\item{numcol}{number of label columns per page}

\item{page_width}{page width in inch}

\item{page_height}{page height in inch}

\item{width_margin}{side margin of label sheet in inch}

\item{height_margin}{top margin of lablel sheet in inch}

\item{label_width}{label width in inch}

\item{label_height}{label height in inch}

\item{x_space}{A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".}

\item{y_space}{A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when barcode_type = "matrix".}

\item{fontfamily}{font family ("mono", "sans", "serif") or specific font based on your operation system.}

\item{showborder}{logical: whether to show border of labels. Set to TRUE to check whether everything fit in the label area.}

\item{barcode_width}{0-1, barcode proportion of the label width}

\item{barcode_height}{0-1, barcode proportion of the label height}

\item{text_top_height}{0-1. Linear barcode allows 2 text boxes (above and below the barcode). This sets the height proportion of the label width for the text box above the barcode}

\item{text_bottom_height}{0-1. Sets the height proportion of the label width for the text box below the barcode}

\item{text_top}{text above the 1D barcode (can use "\\n" for line break)}

\item{text_bottom}{text bottom (can use "\\n" for line break)}

\item{text_qr}{text for on the right side of QR (can use "\\n" for line break)}

\item{label_margin}{0-1. Set a margin to avoid page shift during printing}

\item{text_align}{left or center alignment for text. Default is center for 1D barcode and left for 2D barcodes.}

\item{scale}{0-1 barcode scaling inside the drawing area}
}
\value{
Does not return anything, just create a PDF files with labels.
}
\description{
Make Label sheets with barcodes

dmcode_make: create a grid Grob object for drawing data matrix code generated by function "dmcode"

qrcode_make: create a grid Grob object for drawing QR code generated by function "qr"

code_128_make: create a grid Grob object for drawing linear code128
}
\examples{
dd <- data.frame(plot = 1:10, accession = LETTERS[1:10])
# use plot to generate barcodes, and give different text
# for 1D (linear code128) labels
makeLabel(Labels=dd$plot, name="field2022_linear.pdf", text_top = paste0("Plot ",dd$plot, "\n", "Acc ", dd$accession), 
text_bottom = rep("Davis Field 2022", nrow(dd)), barcode_type="linear", Fsz=9, 
label_type = "avery5967", showborder = T, text_top_height = 0.4, 
text_bottom_height = 0.2, barcode_height = 0.4, barcode_width = 0.9, fontfamily = "sans")
# for QR codes
makeLabel(Labels=dd$plot, name="field2022_QR", text_qr = paste0("Plot ",dd$plot, "\n", "Acc ", dd$accession), 
barcode_type="matrix", Fsz=12, label_type = "avery5967", showborder = T, 
barcode_height = 1, fontfamily = "sans")
}
