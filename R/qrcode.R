#' Make QR code raw data for plotting.
#' 
#' This function is very fast because it uses C code in the hood.
#'
#' @param text input text for QR code generation
#' @param ecl Error correction level (1 = Low (tolerate about  7% erroneous codewords); 2 = Medium (15%); 3 = QUARTILE (25%); 4 = High (30%)). 
#' The correction level of the result may be higher than the ecl argument if it can be done without increasing the QR code version (dimension).
#'
#' @return a number matrix representing the QR code
#' @export
#'
#' @examples
#' qrout <- qrcode("Hello, world!", 1)
#' plotqr(qrout)
qrcode <- function(text, ecl=1) {
  result <- .Call("qrcode2", text, ecl)
  return(t(result)) # seems rotate or not all give the correct text retrun
}

#' Make data matrix code
#'
#' @param text input text for datamatrix code generation
#' @param square whether to use squared shape (1 is Yes; 0 is NO). If not, the function will automatically decide the shape.
#'
#' @return a number matrix representing the datamatrix code
#' @export
#'
#' @examples
#' dmout <- dmcode("Hello, world!", 1)
#' plotqr(dmout)
dmcode <- function(text, square=1) {
  result <- .Call("dmcode", text, square)
  return(t(result)) # seems rotate or not all give the correct text retrun
}

#' Plot the QR code
#' @param x output of qrcode function
#' @examples
#' qrout <- qrcode("Hello, world!", 1)
#' plotqr(qrout)
#' @export
plotqr <- function(x) {
  grid::grid.newpage()
  grid::grid.raster(x, interpolate=FALSE)
}
