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
#' Transformation for continuous data with a finite number of distinct values
#'
#' @description
#' \code{trans_composition()} derives a transformation from a numerical vector with a smaller number (ideally < 30) of distinct values.
#' The return can be used with function \code{ggplot::scale_x_continuous()}
#' or \code{ggplot::scale_y_continuous()} to create a desired axis.
#'
#' @importFrom scales identity_trans
#' @importFrom stats approx quantile approxfun
#' @importFrom labeling extended
#'
#' @param x A numerical vector used in a plot as (typically) \code{x}
#' @param nb An integer for the maximum number of breaks. Default=30
#' @param brk One of
#'   - A numerical value within \code{range(x)}. All values after the value will be spaced equally
#'   - \code{NA} or a numerical value that is greater than or equal to \code{max(x)}. All values will be plotted in the original scale
#'   - A numerical value that is smaller than or equal to \code{max(x)}. All values will be plotted in equal space
#' @param dab One of
#'   - NA for a value calculated automatically
#'   - A number for the distance after \code{brk}
#' @param dgrd One of
#'   - NA for a value calculated automatically
#'   - A number for the minimum space between major grids
#' @param dgrd2 One of
#'   - NA for a value calculated automatically
#'   - A number for the minimum space between major grids
#'
#' @return A transformation function
#'
#' @examples
#' library(ggplot2)
#' pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5))
#' pdt$y=pdt$x+rnorm(length(pdt$x))
#' t=trans_composition(pdt$x,brk=50, dab=3)
#' ggplot(pdt, aes(x=x, y=y))+geom_point()+scale_x_continuous(trans=t)
#'
#' @export

trans_composition <- function( x=NULL, nb=30, brk=NA, dab=NA, dgrd=NA, dgrd2=NA ){
  if(is.null(x)|missing(x)){stop('x should be a numerical vector')}
  xori=as.numeric(x)
  if(sum(is.na(xori))>0){stop('x is should not included NAs')}
  if(typeof(x)!='double'|length(xori)<2){stop('x should be a numerical vector')}
  if(is.null(nb)|missing(nb)){nb=NA}
  nb=as.numeric(nb)
  if(is.na(nb)|length(nb)!=1){nb=30}
  xb=unique(xori)
  xb=xb[order(xb)]
  xnb=length(nb)
  if(xnb>nb){stop('x has more distinct values than nb')}
  if(is.null(brk)|missing(brk)){brk=max(xori)+1}
  brk=as.numeric(brk)
  if(is.na(brk)|length(brk)!=1){brk=max(xori)+1}
  if(is.null(dab)|missing(dab)){dab=NA}
  dab=as.numeric(dab)
  if(is.na(dab)|length(dab)!=1|dab<=0){dab=as.numeric(quantile(diff(xb),0.2))}
  if(is.null(dgrd)|missing(dgrd)){dgrd=NA}
  if(is.na(dgrd)|length(dgrd)!=1){ if(brk>min(xori)){dgrd=floor((min(max(xori),brk)-min(xori))/24)}else{dgrd=floor(max(xori)-min(xori))/24} }
  if(is.null(dgrd2)|missing(dgrd2)){dgrd2=NA}
  if(is.na(dgrd2)|length(dgrd2)!=1){if(brk>min(xori)){dgrd2=floor((min(max(xori),brk)-min(xori))/48)}else{dgrd2=floor((max(xori)-min(xori))/48)}}

  trans=identity_trans()
  trans$name='composition'
  xl1=c();xb1=c();xl2=c();xb2=c()
  if(brk>=max(xb)){
    xl1=xb
    xb1=xb
    trans$breaks=function(x,n=xnb){
      x=x[is.finite(x)]
      if(length(x)==0){return(numeric())}
      if(sum(!missing(x))==0){return(numeric())}
      fxl1=eval(xl1)
      fxb1=eval(xb1)
      if(sum(!is.na(fxl1))<2 | sum(!is.na(fxb1))<2){return(x)}
      mingr=eval(dgrd)
      for(itr0 in 1:5){
        if(min(diff(fxl1))>=mingr){
          break
        }else{
          for(itr in 2:length(fxl1)){
            if(fxl1[itr]-fxl1[itr-1]<mingr){
              if(round(fxl1[itr])==fxl1[itr]&round(fxl1[itr-1])!=fxl1[itr-1]){
                fxl1[itr-1]=fxl1[itr]
              }else{
                fxl1[itr]=fxl1[itr-1]
              }
            }
          }
        }
      }
      fxl1=unique(fxl1)
      fxl1
    }
  }else if(brk<=min(xb)){
    xl2=xb
    xb2=seq(min(xl2),min(xl2)+(length(xb)-1)*dab, by=dab)

    trans$transform=function(x){
      if(length(x)==0){return(x)}
      if(sum(!missing(x))==0){return(x)}
      if(sum(!is.na(x))==0){return(x)}
      fxl2=eval(xl2)
      fxb2=eval(xb2)
      if(sum(!is.na(fxl2))<2 | sum(!is.na(fxb2))<2){return(x)}
      ff=approx(fxl2, fxb2, x, method='linear', rule=2)$y
      ff
    }
    trans$inverse=function(x){
      if(length(x)==0){return(x)}
      if(sum(!missing(x))==0){return(x)}
      if(sum(!is.na(x))==0){return(x)}
      fxl2=eval(xl2)
      fxb2=eval(xb2)
      if(sum(!is.na(fxl2))<2 | sum(!is.na(fxb2))<2){return(x)}
      ff=approx(fxb2, fxl2, x, method='linear', rule=2)$y
      ff
    }
    trans$breaks=function(x,n=xnb){
      x=x[is.finite(x)]
      if(length(x)==0){return(numeric())}
      if(sum(!missing(x))==0){return(numeric())}
      fxl2=eval(xl2)
      fxb2=eval(xb2)
      if(sum(!is.na(fxl2))<2 | sum(!is.na(fxb2))<2){return(x)}
      fxl2=eval(xl2)
      out=c(fxl2)
      out
    }
  }else{
    xl1=xb[xb<=brk]
    xb1=xl1
    xl2=xb[xb>brk]
    xb2=max(xb1)+dab*1+seq(dab,length(xl2)*dab,by=dab)
    brk=max(xb1)+dab

    trans$transform=function(x){
      if(length(x)==0){return(x)}
      if(sum(!missing(x))==0){return(x)}
      if(sum(!is.na(x))==0){return(x)}
      fxl2=eval(xl2)
      fxb2=eval(xb2)
      fbrk=eval(brk)
      if(sum(!is.na(fxl2))<2 | sum(!is.na(fxb2))<2){return(x)}
      ff=approx(fxl2, fxb2, x, method='linear', rule=2)$y
      x*(x<=fbrk)+(x>fbrk)*ff
    }
    trans$inverse=function(x){
      if(length(x)==0){return(x)}
      if(sum(!missing(x))==0){return(x)}
      if(sum(!is.na(x))==0){return(x)}
      fxl2=eval(xl2)
      fxb2=eval(xb2)
      fbrk=eval(brk)
      if(sum(!is.na(fxl2))<2 | sum(!is.na(fxb2))<2){return(x)}
      ff=approx(fxb2, fxl2, x, method='linear', rule=2)$y
      x*(x<=fbrk)+(x>fbrk)*ff
    }
    trans$breaks=function(x,n=xnb){
      x=x[is.finite(x)]
      if(length(x)==0){return(numeric())}
      if(sum(!missing(x))==0){return(numeric())}
      x=unique(x)
      fxl1=eval(xl1)
      fxb1=eval(xb1)
      if(sum(!is.na(fxl1))<2 | sum(!is.na(fxb1))<2){return(x)}
      mingr=eval(dgrd)
      for(itr0 in 1:5){
        if(min(diff(fxl1))>=mingr){
          break
        }else{
          for(itr in 2:length(fxl1)){
            if(fxl1[itr]-fxl1[itr-1]<mingr){
              if(round(fxl1[itr])==fxl1[itr]&round(fxl1[itr-1])!=fxl1[itr-1]){
                fxl1[itr-1]=fxl1[itr]
              }else{
                fxl1[itr]=fxl1[itr-1]
              }
            }
          }
        }
      }
      fxl1=unique(fxl1)
      fxl2=eval(xl2)
      out=c(fxl1, fxl2)
      out
    }
  }

  trans$minor_breaks=function(b,limits=range(xb),n=xnb){
    b=b[is.finite(b)]
    if(length(b)<2){return(numeric())}
    if(sum(!missing(b))==0){ return(numeric())}
    fxl=eval(xb)
    fbrk=eval(brk)
    if(length(fxl)<2){ return(numeric())}
    fd=eval(dgrd2)
    fxlmin=floor(min(fxl))
    fxlmax=ceiling(max(fxl))
    if(max(limits)>fxlmax) fxlmax=ceiling(max(limits))
    if(min(limits)<fxlmax) fxlmin=floor(min(limits))
    if(fbrk > fxlmin){
      fxc=min(fxlmax, fbrk)
    }else{
      fxc=fxlmax
    }
    out1=labeling::extended(fxlmin, fxc, m=max(2,floor(1+(fxc-fxlmin)/fd)))
    out1
  }
  trans
}

