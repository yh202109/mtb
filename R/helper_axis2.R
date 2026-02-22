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
#' \code{trans_loglinear()} derives a log transformation from a numerical vector with a smaller number (ideally < 30) of distinct values.
#' The return can be used with function \code{ggplot::scale_x_continuous()}
#' or \code{ggplot::scale_y_continuous()} to create a desired axis.
#'
#' @importFrom scales new_transform
#' @importFrom stats quantile
#' @importFrom labeling extended
#'
#' @param x A numerical vector used in a plot as (typically) \code{x}
#' @param nb An integer for the maximum number of breaks. Default=30
#' @param int One of
#' \itemize{
#'   \item NA for a value calculated automatically
#'   \item A real number (>=0) for the shift before log transform
#' }
#' @param scale One of
#' \itemize{
#'   \item NA for a value calculated automatically
#'   \item A real number (>0) for the scale before log transform
#' }
#' @param mindist One of
#' \itemize{
#'   \item NA for a default value set to 0.03
#'   \item A real number between 0 and 0.2 for the minimum distance ratio between major ticks
#' }
#'
#' @return A transformation function
#'
#' @examples
#' library(ggplot2)
#' pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5))
#' pdt$y=pdt$x+rnorm(length(pdt$x))
#' t=trans_loglinear(pdt$x)
#' ggplot(pdt, aes(x=x, y=y))+geom_point()+scale_x_continuous(trans=t)
#'
#' @export

trans_loglinear <- function( x=NULL, nb=30, int=NA, scale=NA, mindist=0.03 ){
  if(is.null(x)|missing(x)){stop('x should be a numerical vector')}
  xori=as.numeric(x)
  if(sum(is.na(xori))>0){stop('x should not include NAs')}
  if(typeof(x)!='double'|length(xori)<2){stop('x should be a numerical vector')}
  int=as.numeric(int)
  scale=as.numeric(scale)
  if(is.null(nb)|missing(nb)){nb=NA}
  nb=as.numeric(nb)
  if(is.na(nb)|length(nb)!=1){nb=30}
  xb=unique(xori)
  xb=xb[order(xb)]
  xnb=length(xb)
  if(xnb>nb){stop('x has more distinct values than nb')}
  mindist=as.numeric(mindist)
  if(is.na(mindist)){mindist=0.03}
  if(mindist>0.2){mindist=0.2}
  if(mindist<0){mindist=0}

  base=exp(1)
  if(is.na(int)|is.na(scale)){
    dx = quantile(x, c(0, 0.3, 0.8, 1), na.rm=TRUE)
    if(dx[4]-dx[3]<=dx[2]-dx[1]|min(diff(dx))==0){int=NA; scale=NA
    }else{scale=(dx[2]+dx[3]-dx[1]-dx[4])/(dx[1]*dx[4]-dx[2]*dx[3]); if(scale<=0){scale=NA;int=NA}else{int=base/(dx[1]*scale+1); scale=int*scale}}
  }else{if(int<0|scale<=0){scale=NA;int=NA}}
  if(is.na(int)){ return(new_transform("identity", transform=identity, inverse=identity)) }
  trans=function(x)log(int+scale*x,base)
  inv=function(x) (base^x-int)/scale
  breaks=function(x, n = 30){
    fs=eval(scale)
    fi=eval(int)
    fb=eval(xb)
    fmin=eval(mindist)
    raw_rng <- suppressWarnings(range(fb, na.rm = TRUE))
    if(any(!is.finite(raw_rng))){return(numeric())}
    rng <- trans(fb)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min)
      return((base^min-fi)/fs)
    breaks <- fb
    for(itr0 in 1:5){
      ftb=trans(breaks)
      fdb=(max(ftb)-min(ftb))*fmin
      if(min(diff(ftb))>=fdb){
        break
      }else{
        for(itr in 2:length(breaks)){
          if(ftb[itr]-ftb[itr-1]<fdb){
            if(round(breaks[itr])==breaks[itr]&round(breaks[itr-1])!=breaks[itr-1]){
              breaks[itr-1]=breaks[itr]
            }else{
              breaks[itr]=breaks[itr-1]
            }
          }
        }
      }
      breaks=unique(breaks)
    }
    return(breaks)
  }
  minor_breaks=function(b,limits=range(xori),n=xnb*6){
    b=b[is.finite(b)]
    if(length(b)<2){return(numeric())}
    if(sum(!missing(b))==0){ return(numeric())}
    fx=eval(xb)
    dfx=diff(fx)
    if(length(fx)<=2){ return(numeric())}
    fxmin=floor(min(fx))
    fxmax=ceiling(max(fx))
    if(max(limits)>fxmax) fxmax=ceiling(max(limits))
    if(min(limits)<fxmax) fxmin=floor(min(limits))
    dm=min(c(1,dfx), na.rm=TRUE)
    ndm=ceiling((fxmax-fxmin)/dm)
    out1=labeling::extended(fxmin, fxmax, m=ndm)
    out1=out1[out1>0]
    trans(out1)
  }
  new_transform('loglinear', transform=trans, inverse=inv, breaks=breaks, minor_breaks=minor_breaks, domain=c(1e-100,Inf))
}

