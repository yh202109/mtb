#  Copyright (C) 2021 Y Hsu <yh202109@gmail.com>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public license as published by
#  the Free software Foundation, either version 3 of the License, or
#  any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details
#
#  You should have received a copy of the GNU General Public license
#  along with this program. If not, see <https://www.gnu.org/license/>
#' @importFrom grDevices col2rgb
#' @importFrom graphics legend
#'
NULL

############################################################
mtb_cleanupstr <- function(str = "") {
  trimws(gsub("[^a-zA-Z0-9()<>=,._;!@#$%&+\\*/-]", " ", str), which = c("both"), whitespace = "[ \t\r\n]")
}

############################################################
mtb_color2rgb <- function(str = "", alpha = 255, totri=TRUE, outmaxhue=255, inmaxhue=255, outalpha=FALSE) {
  outmaxhue=floor(max(1,as.numeric(outmaxhue),na.rm=TRUE))
  inmaxhue=floor(max(1,as.numeric(inmaxhue),na.rm=TRUE))
  outrgb=rep(0.5*outmaxhue, 3)
  if(outmaxhue!=1){outrgb=floor(outrgb)}

  if(outalpha==TRUE){
    if (missing(alpha) | is.null(alpha) | min(anyNA(alpha)) == 1) {
      alpha=outmaxhue
      alphastr="FF"
    } else {
      alpha <- as.numeric(alpha)[1]
      alpha <- max(0.1*outmaxhue, min(outmaxhue, outmaxhue*alpha/inmaxhue, na.rm = TRUE))
      if(outmaxhue!=1){alpha=floor(alpha)}
      alphastr=as.character(as.hexmode(255*alpha/outmaxhue))
    }
  }else{
    alpha=outmaxhue
    alphastr="FF"
  }
  if ( missing(str) | is.null(str) | min(anyNA(str)) == 1) {
    if(outalpha==TRUE){
      if(totri==TRUE){return(c(outrgb, alpha)) }else{ return('#999999FF')}
    }else{
      if(totri==TRUE){return(c(outrgb)) }else{ return('#999999')}
    }
  }
  if ( length(str) == 1) {
    if (is.character(str)) {
      if ((nchar(str) == 9 & grepl("^#[0-9A-Fa-f]{8}$", str))) {
        alphastr = substr(str, 8,9)
        alpha = outmaxhue*as.numeric(as.hexmode(alphastr))/255
        if(outmaxhue!=1){alpha=floor(alpha)}
        str = substr(str, 1,7)
      }
      if ((nchar(str) == 7 & grepl("^#[0-9A-Fa-f]{6}$", str)) | str %in% colors()) {
        outrgb <- col2rgb(str, alpha = FALSE)
      }
    }
  } else if (length(str)>=3&length(str)<=4) {
    outrgb <- as.numeric(str)
    outrgb[is.na(outrgb)] <- outmaxhue/2
    if(outmaxhue!=1){outrgb=floor(outrgb)}
    if (length(str) == 4) {
      alpha=outrgb[4]
      alpha=max(0.1*outmaxhue, min(outmaxhue, outmaxhue*alpha/inmaxhue, na.rm = TRUE))
      if(outmaxhue!=1){alpha=floor(alpha)}
      alphastr=as.character(as.hexmode(255*alpha/outmaxhue))
      outrgb=outrgb[1:3]
    }
    outrgb <- sapply( sapply( outrgb, max, 0, na.rm=TRUE), min, outmaxhue)
    if(outmaxhue!=1){ outrgb <- floor(outrgb) }
  } else {
    warning("invalid color")
  }
  if(totri==TRUE){
    if(outalpha==TRUE){ return(c(outrgb, alpha)) }else{ return(c(outrgb)) }
  }else{
    if(outalpha==TRUE){
      return(rgb(t(as.matrix(outrgb)), maxColorValue=outmaxhue))
    }else{
      return(paste( rgb(t(as.matrix(outrgb)), maxColorValue=outmaxhue), alphastr, sep=""))
    }
  }
}


############################################################
mtb_dt_toPOSIXct = function(str, origin='1970-01-01 00:00:00'){
  em = 'NA introduced'
  if(is.numeric(str)){
    tstr=tryCatch(as.POSIXct(str, origin=origin),error=function(r){message(em);return(NA)},warning=function(r){message(em);return(NA)})
  }else{
    tstr=tryCatch(as.POSIXct(str, tryFormats=c( "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d", "%H:%M:%OS" ) ),error=function(r){message(em);return(NA)},warning=function(r){message(em);return(NA)})
  }
  tstr
}
