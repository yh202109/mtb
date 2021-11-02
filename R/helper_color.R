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

############################################################
#' Generate a color vector
#'
#' Create a list of colors for a data vector by a list major colors.
#'
#' @importFrom grDevices colorRamp colors rgb
#'
#' @param vect A vector for groups.
#' @param vectn An integer vector with length 0 or with the same length of \code{vect} for order of elements in \code{vect}. default=c()
#' @param cols One of
#'   - A color names vector
#'   - An RGB triplet vector
#'   - A HEX vector
#' @param black A level in \code{vect} that should be assigned to black color. default=""
#' @param gray9 A level in \code{vect} that should be assigned to gray9 color. default=""
#'
#' @return a named vector
#'
#' @examples
#' color_set_palette( c('apple', 'orange', 'lime', 'apple'), c(2,1,3,2), 'red', 'blue')
#'
#' @export
#'

color_set_palette = function( vect=c(), vectn=c(), cols=c('blue','cyan','darkorange'), black="", gray9="" ){
  nobs0 = length(vect)
  vect = as.character(vect)
  vect = vect[vect!=""]
  nvect = length(unique(vect))
  nobs = length(vect)
  vectn = as.numeric(vectn)
  nvectn = length(unique(vectn))
  if( nobs != nobs0 ){ warning(paste('removed', nobs0-nobs, 'empty records in vect')) }
  if( nvectn>0&nvectn==nvect ){vectn=vectn[vect!=""]}
  if( missing(vect) | is.null(vect) | length(vect)==0 | sum(vect=="")==nobs ){ warning('invalid vect'); return(c())}
  if( missing(vectn) | is.null(vectn) | sum(is.na(vectn))>0 | nvectn==0 | (nvectn>0&nvect!=nvectn)  ){ vectn = as.numeric(factor(vect));nvectn=length(unique(vectn)); warning('invalid vectn')}
  tmpdt = unique(data.frame( gp = vect, gpn = vectn ))
  if( dim(tmpdt)[1] != nvect ){
    vectn = as.numeric(factor(vect))
    tmpdt = unique(data.frame( gp = vect, gpn = vectn ))
    warning('vect and vectn are not one to one')
  }
  tmpdt = tmpdt[order(tmpdt$gpn),]
  colsrgb = sapply(cols, mtb_color2rgb, totri=FALSE)
  tmpdt$color = ""
  tmpdt$color[tmpdt$gp == black] = "#000000"
  tmpdt$color[tmpdt$gp == gray9] = "#999999"
  ncolneeded = sum(tmpdt$color=="")
  if(ncolneeded<=length(colsrgb)){
    colsrgb = colsrgb[1:ncolneeded]
    tmpdt$color[tmpdt$color==""] = colsrgb
  }else{
    ramp = colorRamp(colsrgb)
    tmpdt$color[tmpdt$color == ""] = rgb(ramp(seq(0,1,length=ncolneeded)), maxColorValue=255)
  }
  setNames( tmpdt$color, tmpdt$gp )
}

############################################################
#' Test a color vector
#'
#' Create a figure using the assigned color vector
#'
#' @importFrom graphics barplot plot lines
#' @importFrom stats dnorm setNames
#'
#' @param colvect A vector returned by \code{color_set_palette()}
#' @param type One of
#'   - 'line' for using the color vector on a line plot (Default)
#'   - 'box' for using the color vector on a box plot.
#'
#' @return A plot
#'
#' @examples
#' color_test_palette( setNames(c(1,2,3,4), c('apple','orange','avocado','lime') ))
#'
#' @export
#'
color_test_palette = function( colvect=c(), type='line' ){
  x = seq(-3,3,0.1)
  ngp = length(colvect)
  gp = names(colvect)
  sd = seq(1,2,length=ngp)
  if( is.null(colvect)|missing(colvect)|length(colvect)==0 ){ return(NULL) }
  if( type == 'line'){
    dt=NULL
    plot( x, y=dnorm(x, 0, sd[1]), type='l', frame=FALSE, col=colvect[1], xlab='x', ylab='y')
    if( ngp>1 ){
      for(idx in 2:ngp){
        lines( x, dnorm(x, 0, sd[idx]), type='l', lty=(1+((idx-1)%%6)), col=colvect[idx])
      }
    }
    legend('topleft', legend=names(colvect), col=colvect, lty=(1+(seq(1,ngp)-1)%%6))
  }else if( type=='bar'){
    barplot(rep(1,ngp), axes=FALSE, space=0, col=colvect, names.arg = gp)
  }else{ warning('plot type not specified'); NULL}
}


