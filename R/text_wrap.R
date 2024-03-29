#' Split string to word with non-alpha-numeric characters
#'
#' @param str a string
#'
#' @return a vector of split 'words'
# @export
#'
#' @examples
#' word_split("abc\n123 abc-ef-ghijk*ll")
word_split = function(str){
  t1 = gsub("([^a-zA-Z0-9]+)", "\\1_-_-_", str)
  unlist(strsplit(t1, "_-_-_"))
}

# to split with non-alpha-numeric characters except *
word_split2 = function(str){ # for markdown text split
  t1 = gsub("([^a-zA-Z0-9\\*]+)", "\\1_-_-_", str)
  unlist(strsplit(t1, "_-_-_"))
}

# function to wrap string for width (inch) limit
#' A function to wrap string for width (inch) limit
#'
#' @param str_vec a vector of split words vector from word_split 
#' @param width width limit in inch
#'
#' @return a list of 2 elements: a string with "\n" added between words (You can use `cat` to see the word wrap) and a vector of positions of newline added (which elements in str_vec were added "\n")
# @export
#'
#' @examples
#' text = "line1\na-long-text-line_long_long_long abc"
#' str_vec = word_split(text)
#' new_text = str_wrap_inch(str_vec, grid::gpar(fontsize=12), width = 1.2)
#' cat(new_text$text)
str_wrap_inch = function(str_vec, width, gp = grid::get.gpar(), unit = "in"){# t2 is split word vector from word_split
  if(length(str_vec)==1) return(list(text = str_vec[1], newline_pos = c()))
  t2 = str_vec
  sl2 = mystrwidth(t2, gp, unit) # get the width of each word
  max_text_width = max(sl2)
  if (max_text_width > width) {
    font_size = floor(width/max_text_width*gp$fontsize*10)/10
    cat("Waring: single word is too long. Reduce font size to", font_size, "to fit the width\n")
    gp$fontsize = font_size
    sl2 = mystrwidth(t2, gp, unit)
  }
  newline_pos = c()
  tmp = sl2[1]
  for ( i in 2:length(t2) ){
    if (endsWith(t2[i-1], '\n')) {tmp = sl2[i]; next}
    tmp = tmp + sl2[i]
    if (tmp > width) {
      t2[i-1] = paste0(t2[i-1], "\n")
      tmp = sl2[i]
      newline_pos = c(newline_pos, i-1)
    } 
    # if ( grepl("\n", t2[i]) )  tmp = 0 # if t2[i] already has '\n'
  }
  # t2
  list(text = paste0(t2, collapse=""), newline_pos = newline_pos)
}

# text box size
#' Wrap text within a text box
#'
#' @param text a single string
#' @param font_size font size
#' @param box_width text box width
#' @param box_height text box height
#' @param fontfamily font family ("mono", "sans", "serif") or specific font based on your operation system.
#' @param useMarkdown TRUE or FALSE: whether to treat "*" as markdown symbols.
#' @param unit length unit, default "inch"
#'
#' @return a vector with 2 elements: new string with "\n" added and the final adjusted font size
# @export
#'
#' @examples
#' box_width = 1
#' box_height = 0.5
#' text = "line1\na-long-text-line_long_long_long"
#' font_size = 10
#' text_box_wrap(text, font_size, box_width, box_height, "mono")
text_box_wrap = function(text, font_size, box_width, box_height, fontfamily = "sans", useMarkdown = FALSE, unit="in"){
  if (text == "") return( c(text = text, font_size = font_size) )
  #line_number = nchar(gsub("[^\n]", "", text)) + 1 # number of lines
  #font_size = ifelse(font_size * line_number > box_height * 72, box_height * 72 / line_number, font_size)
  #text_height = mystrheight(text, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=2), unit)
  #font_size = ifelse(text_height > box_height, font_size * box_height / text_height, font_size)
  text0 = text # original text
  if (useMarkdown) {
    text = gsub("(\\*+)((.|\n)+?)\\1", "\\2", text, perl = T)
    text = gsub("(\\*+)((.|\n)+?)\\1", "\\2", text, perl = T) # in case of nested *
    }
  text2 = word_split(text)
  # pdf(NULL) # and this could be opened with additional parameters
  # par(ps = font_size, family = fontfamily, font=4)
  max_text_width = max( mystrwidth(text2, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=4), unit) )
  if (max_text_width > box_width) font_size = floor(box_width/max_text_width*font_size*10)/10 # to make sure the longest unsplit words can fit
  text3 = str_wrap_inch(text2, box_width, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=4), unit=unit)
  text_height = mystrheight(text3$text, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=2), unit)
  #line_number = nchar(gsub("[^\n]", "", text3$text)) + 1 # number of lines
  #while (font_size * line_number > box_height * 72){
  while (text_height > box_height){
    font_size = font_size - 0.5
    # par(ps = font_size, family = fontfamily, font=4)
    # max_text_width = max( mystrwidth(text2, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=2), unit) )
    # if (max_text_width > box_width) font_size = floor(box_width/max_text_width*font_size*10)/10
    text3 = str_wrap_inch(text2, box_width, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=4), unit=unit)
    #line_number = nchar(gsub("[^\n]", "", text3$text)) + 1 # number of lines
    text_height = mystrheight(text3$text, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=2), unit)
  }
  # dev.off()
  if (useMarkdown) {
    text4 = word_split2(text0)
    for (i in text3$newline_pos) text4[i] = paste0(text4[i], "\n")
    return(c(text = paste(text4, collapse = ""), font_size = font_size))
  }
  c(text = text3$text, font_size = font_size)
}

#' Wrap a vector of strings to fit a text box.
#' Just a vectorized function of "text_box_wrap"
#'
#' @param text_array a string vector
#' @param font_size inital font size
#' @param box_width text box width
#' @param box_height text box height
#' @param fontfamily font family ("mono", "sans", "serif") or specific font based on your operation system.
#' @param useMarkdown TRUE or FALSE: whether to treat "*" as markdown symbols.
#' @param unit length unit (inch, mm, etc)
#'
#' @return a list of 2 elements: a vector of wrapped string and the final font size.
#' @export
#'
#' @examples
#' ss = c("a very **long** long string", "another-long-long-string")
#' text_array_wrap(ss, 12, 1, 0.5, useMarkdown=T)
text_array_wrap = function(text_array, font_size=12, box_width, box_height, fontfamily = "sans", useMarkdown = FALSE, unit="in"){
  # w1 = mystrwidth(text_array, gp = grid::gpar(fontsize=font_size, fontfamily=fontfamily, fontface=4))
  f1 = text_box_wrap(text_array[1], font_size, box_width, box_height, fontfamily, useMarkdown, unit=unit)
  cat("Initial font is", f1[2], "\n")
  if (length(text_array)>1){
    dd2 = t(sapply(text_array[-1], text_box_wrap, as.numeric(f1[2]), box_width, box_height, fontfamily, useMarkdown, unit, USE.NAMES=F))
    # dd2 = t(sapply(text_array, text_box_wrap, font_size, box_width, box_height, fontfamily, useMarkdown, USE.NAMES=F))
    return(list(text = c(f1[1], dd2[,1]), font_size = min(as.numeric(dd2[,2]))))
  } else {
    return(list(text = f1[1], font_size = as.numeric(f1[2])))
  }
}
