#  Copyright (C) 2020 Y Hsu <yh202109@gmail.com>
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
#' Plot labeled events
#'
#' @description
#' Create a plot for events with labels
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_label xlab ylab arrow labs geom_rect geom_hline geom_vline theme element_blank geom_text geom_point coord_flip
#' @importFrom rlang .data
#' @importFrom stats aggregate
#'
#' @param dt a \code{data.frame} with the following columns
#' \describe{
#'   \item{id}{for ID of each group}
#'   \item{idn}{for order of ID}
#'   \item{start}{for starting time}
#'   \item{end}{for ending time with arrow head}
#'   \item{label}{for labeling the starting time}
#'   \item{labelend}{for labeling the ending time of a interval}
#'   \item{type}{for event type as one of p (point), i (interval), b (box)}
#'   \item{color}{a string for event color}
#' }
#' @param xlab A string for the x-axis title
#' @param anchor A Boolean value for the vertical lines linking \code{start} to the x-axis
#' @param compact A Boolean value for reducing the vertical spacing when applicable
#'
#' @return a plot
#'
#' @examples
#' library(ggplot2)
#' dt = data.frame( id=paste('member',c(rep(c(1,2,3),each=3),3),sep=""),
#' idn=c(rep(1,3),rep(-1,3), rep(2,4)),
#' start=1800*c(0,1,2, 0.5, 1.2, 3, 1,2,3,4),
#' end=1800*c(2,NA,3, 2, 6, NA, 2,2.5,3, 3.5),
#' label=c(paste('event-',seq(1,10),sep='')),
#' labelend=c('','','?',')','','','','','>','X'),
#' type=c('b', 'p', 'i','i','p','p','p','b','i','i' ),
#' color=c('stove', 'oven', 'oven','oven','stove','oven','oven','other','stove','oven' )
#'  )
#' time_plot_event( dt )
#'
#' @export

time_plot_event <- function( dt, xlab='Time', anchor=TRUE, compact=FALSE ){
  dt$start = mtb_dt_toPOSIXct(dt$start)
  dt$end = mtb_dt_toPOSIXct(dt$end)
  dt=dt[!is.na(dt$start),]
  lstn1=c('id','idn','start','end','label','labelend','type', 'color')
  lstn2=colnames(dt)
  if( length(intersect(lstn1, lstn2))!=length(lstn1) ){stop('There were missing columns')}
  dt2=unique(dt[,c('id','idn')])    
  dt=dt[!is.na(dt$start),]
  lstn1=c('id','idn','start','end','label','labelend','type', 'color')
  lstn2=colnames(dt)
  if( length(intersect(lstn1, lstn2))!=length(lstn1) ){stop('There were missing columns')}
  dt2=unique(dt[,c('id','idn')])
  if(dim(dt2)[1] != length(unique(dt2$id)) | dim(dt2)[1] != length(unique(dt2$id))){dt2$idn=as.numeric(as.factor(dt2$id));dt2=unique(dt2)}
  dt2=dt2[order(dt2$idn, decreasing=TRUE),]
  dt$id = factor(dt$id, levels=dt2$id)
  dt$idn = NULL
  dt=merge(dt, dt2, by='id', all.x=TRUE)
  dt=dt[order(dt$idn, dt$start),]
  dt$yloc=seq(1, length(dt$id))
  dt$type[is.na(dt$end)]='p'
  dt$type[dt$start==dt$end]='p'
  dt$end[dt$type=='p']=dt$start[dt$type=='p']
  xlab=as.character(xlab)
  if( xlab=="" ){xlab='Time'}
  if(compact==TRUE){
    dt$end[dt$type=='p']=dt$start[dt$type=='p']+diff(range(c(dt$start, dt$end)))/6
    for(idx in dim(dt2)[1]:1){
      fdt=dt[dt$idn==dt2$idn[idx],]
      if(nrow(fdt)>1){
        fyl=aggregate(fdt$end, list(fdt$yloc), FUN=max)
        for(idx2 in 2:nrow(fdt) ){
          if( fdt$start[idx2]>min(fyl$x) ){
            fdt$yloc[idx2]=min(fyl$Group.1[fyl$x<=fdt$start[idx2]])
            fyl=aggregate(fdt$end, list(fdt$yloc), FUN=max)
          }
        }
        dt[dt$idn==dt2$idn[idx],'yloc']=fdt$yloc
      }
    }
    dt=dt[order(dt$idn, dt$yloc, dt$start),]
  }
  dt$yloc=as.numeric(as.factor(dt$yloc))
  dt$yloc2=dt$yloc+(dt$type=='i')*0.2
  yh=dt$yloc[c(diff(dt$idn)!=0, FALSE)]+0.5
  yl=aggregate(dt$yloc, list(dt$id), FUN=mean)

  p = ggplot(dt, aes(color=.data[["color"]]))+ geom_hline(yintercept=yh)+xlab(xlab)+
    theme(
      panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
      panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
          axis.ticks.y=element_blank(),axis.title.y=element_blank())+
    scale_y_continuous(breaks=yl$x, labels=yl$Group.1)
  if(anchor==TRUE){
    p=p+geom_point(aes(x=.data[["start"]], y=0), alpha=0.7)+
      geom_segment(dt,mapping=aes(x=.data[["start"]], xend=.data[["start"]], y=0, yend=.data[["yloc"]]), lwd=1, alpha=0.5, color='gray', lty=1)
  }
  if(sum(dt$type=='b')>0){
    p=p+geom_rect(dt[dt$type=='b',],mapping=aes(xmin=.data[["start"]], xmax=.data[["end"]], ymin=.data[["yloc"]]+0.25, ymax=.data[["yloc"]]-0.25), alpha=0.5, fill='gray', linejoin='round')
  }
  if(sum(dt$type=='i')>0){
    p=p+geom_segment(dt[dt$type=='i',],mapping=aes(x=.data[["start"]], xend=.data[["end"]], y=.data[["yloc"]], yend=.data[["yloc"]]), lwd=3, alpha=0.3)+
      geom_point(dt[dt$type=='i',],mapping=aes(x=.data[["start"]], y=.data[["yloc"]]), size=4, alpha=0.5)+
      geom_text(dt[dt$type=='i',],mapping=aes(x=.data[["end"]], y=.data[["yloc"]], label=paste(.data[["labelend"]])), hjust=1, vjust=0.5, size=4, fontface='bold')
  }
  if(sum(dt$type=='p')>0){
    p=p+geom_point(dt[dt$type=='p',],mapping=aes(x=.data[["start"]], y=.data[["yloc"]]), alpha=0.7, size=4)
  }
  p+geom_text(aes(x=.data[["start"]], y=.data[["yloc2"]], label=paste(" ", .data[["label"]]), hjust=0, vjust=0))
}


