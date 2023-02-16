#' Draw formatted text labels
#' 
#' richtext() can draw text labels with mixed fontface from markdown syntax (only support * quotes)" *italic*, or **bold**, or ***bold italic***
#' 
#' @param txt a string
#' @param x coordinate on the x axis
#' @param y coordinate on the y axis
#' @param gp An object of class "gpar", typically the output from a call to the function gpar.
#' @param unit A string indicating the default units to use if x or y are only given as numeric values.
#' @param hjust A number specifying horizontal justification.
#' @param vjust A number specifying vertical justification. 
#' @param draw A logical value indicating whether graphics output should be produced.
#' @param useMarkdown whether treat ** quotes as markdown (only support fontfaces)
#'
#' @return richtext() returns a gList that can be used in grid::grid.draw
#' @export
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' vp = viewport(x=0.5, y=0.5, width=unit(3.5, "inches"), height=unit(1, "inches"), gp=gpar(fontsize=12, lineheight=0.8))
#' pushViewport(vp)
#' grid.rect()
#' # can use "\n" for a new line
#' txt = "plain text and\n *italic text* and **bold text** and\n***bold italic text***"
#' gg = richtext(txt, draw = TRUE)
#' # grid.draw(gg)
#' popViewport(1)
#' 
richtext = function(txt, x=0.5, y=0.5, gp = grid::gpar(), unit="npc", hjust=0.5, vjust=0.5, draw=TRUE, useMarkdown=TRUE){#x, y is for top left by default
  if (!useMarkdown | !grepl("\\*", txt)){
    return(grid::grid.text(txt, x=x, y=y, gp=gp, default.units = unit, hjust=hjust, vjust=vjust, draw = draw))
  } else {
    txt2 = seprichlines(txt) # need to fill * if \n inside a * quote
    txtfont = lapply(txt2, getfontface)
    nline = length(txtfont) # number of lines
    totalHeight = getxy2(txt, gp=gp, unit=unit)$y
    addHeight = getxy("ab\ncd",gp=gp, unit=unit)$y
    # dafault cusrsor for top left 0, 1
    xcursor = grid::unit(x, unit)
    ycursor = grid::unit(y, unit)
    # adjust height based on vjust
    if (vjust == 0.5){ # center
      ycursor = ycursor + totalHeight / 2
    } else if (vjust == 0){ # bottom
      ycursor = ycursor + totalHeight
    }
    # get x
    groblist = list()
    for (i in 1:nline){
      bb = txtfont[[i]]
      list0 = mixfontGrob(bb$txt, bb$font, x=xcursor, y=ycursor, gp=gp, unit = unit, hjust = hjust)
      groblist = append(groblist, list0)
      ycursor = ycursor - addHeight
    }
    glist2 = do.call(grid::gList, groblist)
    if(draw) grid::grid.draw(glist2)
    return(glist2)
    # gtree1 = grid::gTree(gp=gp, children = glist2, cl = "richtext")
    # if(draw) grid::grid.draw(gtree1)
    # return(gtree1)
  }
}

## separate lines and make up split * quotes in case "\n" is inside a * quote
seprichlines = function(txt){ 
  txtpieces = strsplit(txt, "\n")[[1]]
  if (length(txtpieces)>1){ # if there is newline
    nn = gregexpr('\n+', txt)[[1]] # newline positions
    bb = gregexpr("(\\*+)((.|\n)+?)\\1", txt, perl = T)[[1]] # quote pairs
    bblen = attributes(bb)$match.length
    bb_end = bb + bblen - 1 # quote ending positions
    bb_star_len = attributes(bb)$capture.length[,1]
    t1 = sapply(nn, function(x){
      xx = bb < x
      yy = bb_end > x
      zz = which(xx & yy)
      ifelse( length(zz) > 0, bb_star_len[zz], 0)
    }) # return a vector of nstar to add
    if (sum(t1)>0){# if any newlines are inside the * quotes
      pp = which(t1 > 0) # which newline are in quotes
      for ( i in pp ){
        ss = paste0(rep("*", t1[i]),  collapse = "")
        txtpieces[i] = paste0(txtpieces[i],ss)
        txtpieces[i+1] = paste0(ss, txtpieces[i+1])
      }
    }
  }
  txtpieces
}

## split a rich line by fontfaces
# return a list for split text pieces and corresponding fontfaces
getfontface = function(txt){
  # txt = "a *abc* ef **d*** e"
  nc = nchar(txt)
  aa=gregexpr("(\\*+)((.|\n)+?)\\1", txt, perl = T)[[1]] # match newlines too
  star2font = c(0,2,1,3) # number of * for normal, bold, italic, and bold italic
  if (aa[1] < 0) return(list(txt=txt, font=1))
  else { # at least 1 pair of * quote
    fontc = c()
    if(aa[1]!=1) fontc = 1
    bb=attributes(aa)$capture.length[,1] # number of star in each pair (left=right)
    bb[bb%%2==0] = 2 # in case >3 stars
    bb[bb%%2==1&bb>3] = 3 # in case >3 stars
    bbfont = sapply(bb, function(x) which(star2font == x))
    for (i in bbfont) fontc = c(fontc, i, 1) # add 1 on the right of each pair, which might cause 1 more 1 at the end
    # get split text
    txt0 = gsub("(\\*+)((.|\n)+?)\\1(\\**)", "__xx__\\2\\4__xx__", txt, perl = T) # match newlines too
    txtpieces = strsplit(txt0, "__xx__")[[1]]
    if(txtpieces[1]=="") txtpieces = txtpieces[-1]
    return(list(txt=txtpieces, font=fontc[1:length(txtpieces)])) # the last 1 might be extra
  }
}


## get grob of each text: just one line: no newline, so split by newline first
mixfontGrob = function(txtvec, fontvec, x, y, gp=gpar(), unit="pt", hjust = 0){#vector of text and corresponding font faces
  txtpieces = txtvec
  n = length(txtpieces)
  xcursor = grid::unit(x, unit)
  ycursor = grid::unit(y, unit)
  xstart = xcursor
  ystart = ycursor
  tmplist = list()
  for (i in 1:n){
    text = txtpieces[i]
    font = fontvec[i]
    gp2 = c(gp, font=as.integer(font))
    # gp2 = grid:::validGP(gp2)
    class(gp2) = "gpar"
    g1 = grid::grid.text(text, x=xcursor, y=ycursor, hjust=0, vjust=1, gp=gp2, draw = F)
    tmplist = append(tmplist, list(g1))
    xcursor = xcursor + grid::convertWidth(grid::grobWidth(g1), unit)
  }
  linewidth = xcursor - xstart
  for (i in 1:length(tmplist)){
    if(hjust == 0.5) tmplist[[i]]$x = tmplist[[i]]$x - linewidth/2
    else if (hjust == 1) tmplist[[i]]$x = tmplist[[i]]$x - linewidth
  }
  
  return(tmplist)
}

## get x and y cursur positions for multi-line text (has \n)
getxy = function(text, gp = grid::gpar(), unit="pt"){
  tt1 = strsplit(text, "\n")[[1]]
  tt2 = tt1[length(tt1)] # the last line
  g1 = grid::textGrob(tt2, gp=gp, hjust=0, vjust=1) # for x cursor
  g2 = grid::textGrob(text, gp=gp, hjust=0, vjust=1)  # for height y
  w1 = grid::convertWidth(grid::grobWidth(g1), unit)
  h1 = grid::convertHeight(grid::grobHeight(g1), unit)
  h2 = grid::convertHeight(grid::grobHeight(g2), unit)
  multline=FALSE
  if(length(tt1)>1) multline=TRUE
  return(list(x=w1, y=h2-h1,multline=multline))
}

## return total width and height of text
getxy2 = function(text, gp = grid::gpar(), unit="pt"){
  g2 = grid::textGrob(text, gp=gp, hjust=0, vjust=1)  # for height y
  w2 = grid::convertWidth(grid::grobWidth(g2), unit)
  h2 = grid::convertHeight(grid::grobHeight(g2), unit)
  return(list(x=w2, y=h2))
}

## only get width of text vector
mystrwidth = function(text, gp = grid::gpar(), unit="inch"){
  sapply(text, function(x) getxy2(x, gp, unit)$x)
}
