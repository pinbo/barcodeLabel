
####
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


seprichlines = function(txt){ # separate lines and make up split * quotes
  txtpieces = strsplit(txt, "\n")[[1]]
  if (length(txtpieces)>1){ # if there is newline
    nn = gregexpr('\n', txt)[[1]] # newline positions
    bb = gregexpr('\\*+', txt)[[1]] # individual star quotes
    bblen = attributes(bb)$match.length
    t1 = sapply(nn, function(x){
      min(which(bb>x))
    })
    newlinepos = which(t1%%2==0)
    if (length(newlinepos)>0){# if any newlines are inside the * quotes
      bblenpos = t1[t1%%2==0]/2
      nbblen = length(bblen)
      nstar2add = sapply(seq(2, nbblen, 2), function(x){
        min(bblen[x], bblen[x-1])
      })
      for (i in 1:length(newlinepos)){
        txtpos = newlinepos[i]
        ss = paste0(rep("*", nstar2add[i]),  collapse = "")
        txtpieces[txtpos] = paste0(txtpieces[txtpos], ss)
        txtpieces[txtpos+1] = paste0(ss, txtpieces[txtpos+1])
      }
    }
  }
  txtpieces
}

#get font face: parse markdown 
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

# # old but working version
# # get font face: parse markdown 
# getfontface = function(txt){
#   # txt = "and **bold** again *."
#   nc = nchar(txt)
#   txtpieces = strsplit(txt, "\\*+", perl=T)[[1]]
#   if(txtpieces[1]=="") txtpieces = txtpieces[-1]
#   fontc = c()
#   textsegnum = 1
#   aa = gregexpr('\\*+(.+?)\\*+', txt)[[1]] # paired star quotes
#   if(aa[1]!=1) {
#     fontc = 1
#     textsegnum = 2
#   }
#   if (aa[1] > 0){ # if there are * quotes
#     bb = gregexpr('\\*+', txt)[[1]] # single star quotes
#     bblen = attributes(bb)$match.length
#     # if odd number of \*+, split by pair
#     if (length(bblen)%%2 == 1)
#       txtpieces = strsplit(txt, "(\\*+)(?=.+?\\*+)", perl=T)[[1]]
#     if(txtpieces[1]=="") txtpieces = txtpieces[-1]
#     star2font = c(0,2,1,3) # number of * for normal, bold, italic, and bold italic
#     naa = length(aa)
#     for (i in 1:naa){
#       x = aa[i]
#       pos = which(bb==x)
#       len1 = bblen[pos] # left star quote length
#       len2 = bblen[pos+1] # right star quote length
#       cc = min(bblen[pos:(pos+1)]) # star length
#       dd = which(star2font == cc)
#       fontc = c(fontc, dd)
#       if (bb[pos+1]+len2-1<nc) fontc = c(fontc, 1)
#       if (len1 > len2) {# add star to left
#         txtpieces[textsegnum] = paste0(rep("*", len1-len2), txtpieces[textsegnum])
#       } else if (len1 < len2){
#         txtpieces[textsegnum] = paste0(txtpieces[textsegnum], rep("*", len2-len1))
#       }
#       textsegnum = textsegnum + 2
#     }
#   } else txtpieces = strsplit(txt, "(\\*+)(?=.+?\\*+)", perl=T)[[1]]
#   return(list(txt=txtpieces, font=fontc))
# }

# get grob of each text: just one line: no newline, so split by newline first
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

## split text by "/n"
getxy = function(text, gp = grid::gpar(), unit="pt"){
  tt1 = strsplit(text, "\n")[[1]]
  tt2 = tt1[length(tt1)] # the last line
  g1 = grid::textGrob(tt2, gp=gp, hjust=0, vjust=1, name="xxxg1") # for x cursor
  g2 = grid::textGrob(text, gp=gp, hjust=0, vjust=1, name="yyyg2")  # for height y
  w1 = grid::convertWidth(grid::grobWidth(g1), unit)
  h1 = grid::convertHeight(grid::grobHeight(g1), unit)
  h2 = grid::convertHeight(grid::grobHeight(g2), unit)
  multline=FALSE
  if(length(tt1)>1) multline=TRUE
  return(list(x=w1, y=h2-h1,multline=multline))
}

# return total width and height of text
getxy2 = function(text, gp = grid::gpar(), unit="pt"){
  g2 = grid::textGrob(text, gp=gp, hjust=0, vjust=1)  # for height y
  w2 = grid::convertWidth(grid::grobWidth(g2), unit)
  h2 = grid::convertHeight(grid::grobHeight(g2), unit)
  return(list(x=w2, y=h2))
}

